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
import capital.Instrument, reference.Mic, Currency.USD

import cats.implicits._
import cats.{ Foldable, Monad, SemigroupK }
import cats.data.{ EitherT, Kleisli, NonEmptySet }

import eu.timepit.refined
import refined.auto._

import scala.collection.immutable.SortedSet
import scala.language.higherKinds

/**
  * What does "double entry bookkeeping" mean in the context of a shared distributed ledger with
  * multiple OMS gateways to external markets?
  *
  * It means this:
  *   - we keep contra accounts per OMS gateway
  *   - we debit that account when we "buy shares" (creates negative balance)
  *   - we credit that account when settlement happens (zeros out the balance)
  *   - we "reverse polarity" when we enter a short position.
  *   - we can indexAndSum settled positions for reconcilliation
  *
  * TODO - provisional:
  * we depend on `Balances` because it makes no sense
  * to trade blind with respect to account sums:
  * risk controls, margin calcs depend on them.
  */
abstract class Trading[MA: Financial, Q: Financial] extends Balances[MA, Q] {

  /** All things financial begin with an allocation. */
  type Allocation = UnitPartition[Account.Key, Quantity]

  /** Namespace placeholder */
  object Allocation

  /** */
  sealed trait Market { def entity: LegalEntity.Key }

  /** */
  object Market extends WithOpaqueKey[Long, Market]

  /**
    * Models a direct deal facing a private party.
    *
    * Their [[Ledger]] [[Account]] is
    * assumed to be "real" (not a contra account) although it can assume those characteristics
    * when the `Counterparty` is sourcing `Instruments` from private flows.
    *   - (eg exempt Securities for accredited individuals or qualified institutions)
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
    * Minimum viable `Order` type. What the client would _like_ to have happen.
    * TODO: revisit parent/child orders
    * TODO: Eliminate `C` ?! Normalize the trade. MA and Currency as separate fields.
    */
  sealed abstract case class Order[C: Currency](
      market: Market.Key,
      ts: Instant,
      trade: Trade,
      limit: Option[Money[MA, C]]
  ) {

    /** */
    def currency = Currency[C]
  }

  /**
    * Price all the things.
    */
  sealed abstract case class MarketDataSource(market: Market) {

    /** */
    def quote[F[_]: Monad, C: Currency](ik: Instrument.Key): F[Mny[C]] =
      Monad[F] pure (Currency[C] fiat quotedIn(ik).mid)

    /** FIXME: how do we materialize these? */
    def quotedIn[C: Currency](ik: Instrument.Key): Instrument.Key QuotedIn C = ???
  }

  object MarketDataSource extends WithOpaqueKey[Long, MarketDataSource] {
    def apply(m: Market): MarketDataSource = new MarketDataSource(m) {}
  }

  /** */
  object Order extends WithOpaqueKey[Long, Order[USD]] {

    /** */
    def market[C: Currency](market: Market.Key, trade: Trade): Order[C] =
      new Order(market, instant, trade, none) {}

    /** */
    def limit[C: Currency](market: Market.Key, trade: Trade, limit: Money[MA, C]): Order[C] =
      new Order(market, instant, trade, limit.some) {}

    /** `Market` orders */
    def buy[C: Currency]: Order[C] = ???

    /** */
    def sell[C: Currency]: Order[C] = ???

    /** `Limit` orders */
    def buy[C: Currency](bid: Money[MonetaryAmount, C]): Order[C] = ???

    /** */
    def sell[C: Currency](ask: Money[MonetaryAmount, C]): Order[C] = ???
  }

  /**
    * What actually happened.
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
    * Note: The methods on `OMS` return `Kliesli` arrows, which are intended to be chained with
    * `andThen`.
    *
    * If it is possible for the arrows to be sequenced in a semantically incorrect way per the
    * domain model, use phantom types to ensure proper sequencing.
    *
    * Reference:
    * [Functional and Reactive Domain Modelling 4.4](https://livebook.manning.com/#!/book/functional-and-reactive-domain-modeling/chapter-4/270)
    *
    * TODO: Revisit design; SemigroupK for Kleisli is restrictive.
    * OTOH it *is* a pipeline and so the Kleisli modeling has fidelity.
    *
    */
  sealed abstract case class OMS[F[_]: Monad: SemigroupK: Foldable] private (
      entity: LegalEntity.Key,
      contra: Account.Key,
      markets: NonEmptySet[Market.Key]
  ) {

    /** */
    type FF[R] = EitherT[F, Fail, R]

    /** */
    type FK[T, R] = Kleisli[FF, T, R]

    /** */
    final def process[C: Currency, A](p: Account.Key, a: Allocation)(
        block: => A
    ): FK[A, Transaction] =
      riskCheck[C, A](p)(block) andThen trade[C](p) andThen allocate[C](a) andThen settle[C](p)

    /** */
    def riskCheck[C: Currency, A](p: Account.Key)(a: A): FK[A, Order[C]] =
      ???

    /** */
    def trade[C](p: Account.Key): FK[Order[C], Execution] = ???

    /** */
    private final def allocate[C: Currency](
        a: Allocation
    ): FK[Execution, Execution] =
      ???

    /** */
    private def settle[C: Currency](p: Account.Key): FK[Execution, Transaction] = ???
  }

  /**
    * Where do Order Management Systems come from? (Here.)
    */
  object OMS extends WithOpaqueKey[Long, OMS[cats.Id]] {

    /**
      * TODO: augment/evolve creation pattern.
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

  /** top level package methods */
  def quoteLeg[F[_]: Monad, C: Currency](mds: MarketDataSource)(leg: Leg): F[Mny[C]] =
    leg match {
      case (security, quantity) => (mds quote [F, C] security) map (_ * quantity)
    }

  /** */
  def quote[F[_]: Monad, C: Currency](mds: MarketDataSource)(trade: Trade): F[Mny[C]] =
    trade.toList foldMapM quoteLeg[F, C](mds)

  /**
    * TODO: Revisit the privacy issues here.
    * Account keys are are needed e.g to verify sigs and allocate partitions
    * Can we break this down into two methods: anonymous, and not?
    */
  def recorded[F[_]: Foldable: Monad: SemigroupK](
      fs: Map[Folio.Key, Folio.Value],
      accounts: Map[Account.Key, Account.Value]
  ): Execution => F[Map[Folio.Key, Folio.Value]] = ???
}
