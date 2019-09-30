/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.deftrade
package model

import keyval._, time._, money._
import capital.Instrument, reference.Mic

import cats.implicits._
import cats.{ Foldable, Monad, SemigroupK }
import cats.data.{ EitherT, Kleisli, NonEmptySet }

import eu.timepit.refined
//import refined.cats._
import refined.auto._

import scala.collection.immutable.SortedSet
import scala.language.higherKinds

/**
  * What does "double entry bookkeeping" mean in the context of a shared distributed ledger with
  * multiple OMS gateways to external markets?
  *
  * It means:
  *   - we keep contra accounts per OMS gateway
  *   - we debit that account when we "buy shares (units)" (creates negative balance)
  *   - we credit that account when settlement happens (zeros out the balance)
  *   - we "reverse polarity" when we enter a short position.
  *   - we can indexAndSum settled positions for reconcilliation
  *
  * TODO - provisional:
  * we depend on `Balances` because it makes no sense
  * to trade blind with respect to account sums:
  * risk controls, margin calcs depend on them.
  */
trait Trading {
  self: Balances with Accounting with Pricing with Ledger with ModuleTypes =>

  /** All things financial begin with an allocation. */
  type Allocation = UnitPartition[Account.Key, Quantity]

  /** Namespace placeholder */
  object Allocation

  /** */
  sealed trait Market { def entity: LegalEntity.Key }

  /** */
  object Market extends WithOpaqueKey[Long, Market]

  /**
    * Models a private party whose [[Ledger]]s we run [[Transaction]]s against.
    *
    * Their [[Ledger]] [[Account]] is assumed to be "real", i.e.
    * not a virtual contra account which is used for reconcilliation.
    */
  sealed abstract case class Counterparty(
      final val label: Label,
      final val contra: Account.Key,
      final val entity: LegalEntity.Key
  ) extends Market

  /** */
  object Counterparty {}

  /**
    * Single effective counterparty: the `Exchange` itself.
    *   - [[reference.Mic]]s are unique.
    *   - seller for all buyers and vice versa.
    *   - activity recorded in a `contra account`
    */
  sealed abstract case class Exchange private (
      final val mic: Mic,
      final val contra: Account.Key,
      final val entity: LegalEntity.Key
  ) extends Market

  /** */
  object Exchange {

    /** */
    def fromMic(mic: Mic): Exchange = ??? // make new contra account

    /** */
    def withEntity(lek: LegalEntity.Key): Exchange => Exchange =
      x => new Exchange(x.mic, x.contra, lek) {}
  }

  /**
    * Price all the things.
    */
  sealed abstract case class MarketDataSource(market: Market) {

    /** `Currency`-specific quote factory. */
    def quotedIn[C: Currency](ik: Instrument.Key): Instrument.Key QuotedIn C

    /** */
    final def quote[F[_]: Monad, C: Currency](ik: Instrument.Key): F[Mny[C]] =
      Monad[F] pure (Currency[C] fiat quotedIn(ik).mid)

    /** */
    final def quoteLeg[F[_]: Monad, C: Currency](leg: Leg): F[Mny[C]] =
      leg match {
        case (security, quantity) => quote[F, C](security) map (_ * quantity)
      }

    /**
      * Simple per-leg pricing. Note, this method is `override`-able.
      *
      * TODO: this is so minimal as to be of questionable viablity... but is correct
      */
    def quoteTrade[F[_]: Monad, C: Currency](trade: Trade): F[Mny[C]] =
      trade.toList foldMapM quoteLeg[F, C]
  }

  /** */
  object MarketDataSource extends WithOpaqueKey[Long, MarketDataSource] {

    /** */
    def apply(m: Market): MarketDataSource = new MarketDataSource(m) {

      /** FIXME: how do we specialize? */
      def quotedIn[C: Currency](ik: Instrument.Key): Instrument.Key QuotedIn C = ???
    }
  }

  /**
    * What the client wants [[Execution]] of.
    *
    * Note: a denominating currency is always required by the [[Exchange]]s,
    * in order to fully specify the trade, even if there is no limit amount attached.
    *
    * TODO: revisit parent/child orders
    */
  sealed abstract case class Order(
      ts: Instant,
      market: Market.Key,
      trade: Trade,
      currency: CurrencyLike,
      limit: Option[MonetaryAmount]
  )

  /** */
  object Order extends WithOpaqueKey[Long, Order] {

    /**
      * Note that currency is required even for market orders.
      */
    def market[C: Currency](market: Market.Key, trade: Trade): Order =
      new Order(instant, market, trade, Currency[C], none) {}

    /** */
    def limit[C: Currency](market: Market.Key, trade: Trade, limit: Mny[C]): Order =
      new Order(instant, market, trade, Currency[C], limit.amount.some) {}
  }

  /**
    * What actually happened to the [[Order]].
    */
  sealed abstract case class Execution(
      ts: Instant,
      oms: OMS.Key,
      orderKey: Order.Key,
      tx: Transaction
  )

  /** */
  object Execution extends WithOpaqueKey[Long, Execution] {}

  /**
    *`OMS` := Order Management System. Ubiquitous domain acronym.
    *
    * The methods on `OMS` return [[cats.data.Kleisli]] arrows, which are intended to be chained with
    * `andThen`.
    *
    * Reference:
    * [[https://livebook.manning.com/#!/book/functional-and-reactive-domain-modeling/chapter-4/270 Functional and Reactive Domain Modelling, section 4.4]]
    *
    * TODO: Flesh this out - currently just a sketch
    *   - `SemigroupK` for `Kleisli` is restrictive.
    *   - OTOH it *is* a pipeline and so the Kleisli modeling has fidelity.
    *   - If it is possible for the arrows to be sequenced in a semantically incorrect way per the
    * domain model, use phantom types to ensure proper sequencing.
    *
    */
  sealed abstract case class OMS[F[_]: Monad: SemigroupK: Foldable] private (
      entity: LegalEntity.Key,
      contra: Account.Key,
      markets: NonEmptySet[Market.Key]
  ) {

    /**
      * `Folio` is a `Map` of `Map`s, which, normalized and written out as a list,
      * has rows of type: {{{
      *   (Folio.Key, Instrument.Key, Quantity)
      * }}}
      */
    type FolioTable = Map[Folio.Key, Folio.Value]

    /** */
    type FF[R] = EitherT[F, Fail, R]

    /** */
    type FK[T, R] = Kleisli[FF, T, R]

    /** */
    final def process[C: Currency, A](p: Account.Key, a: Allocation)(
        folios: FolioTable
    )(
        block: => A
    ): FK[A, FolioTable] =
      riskCheck[C, A](p)(block) andThen trade(p) andThen allocate(a) andThen settle(folios)(p)

    /** */
    def riskCheck[C: Currency, A](p: Account.Key)(a: A): FK[A, Order] =
      ???

    /** */
    def trade(p: Account.Key): FK[Order, Execution] =
      ???

    /** */
    def allocate(a: Allocation): FK[Execution, Execution] =
      ???

    /** */
    final def settle(
        folios: Map[Folio.Key, Folio.Value]
    )(
        p: Account.Key
    ): FK[Execution, FolioTable] = ???
  }

  /** */
  object OMS extends WithOpaqueKey[Long, OMS[cats.Id]] {

    /**
      * FIXME: augment/evolve creation pattern.
      */
    def apply[F[_]: Monad: SemigroupK: Foldable](
        key: LegalEntity.Key,
        market: Market.Key,
        ms: Market.Key*
    ): OMS[F] =
      new OMS[F](key, newContraAccount, NonEmptySet(market, SortedSet(ms: _*))) {}
  }

  /** */
  private def newContraAccount: Account.Key = ???

}
