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

import keyval._, time._, money._, pricing._
import capital.Instrument, refinements.Mic, Currency.USD

import cats._
import cats.data.{ EitherT, Kleisli, NonEmptySet }
import cats.implicits._

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

  /**
    * Price all the things.
    * TODO: Revisit the implicit binding between market data (quotes) and `Exchanges`.
    *   - this isn't how data typically comes, especially cheap data.
    *   - smart routers (and internalizers!) introduce another layer of conceptual complexity
    *   - nonetheless, these are the semantics any sane developer needs... if best effort
    * isn't good enough, don't offer it.
    */
  sealed trait Market { def entity: Option[LegalEntity.Key] }

  object Market extends WithOpaqueKey[Long, Market] {
    implicit def eq: cats.Eq[Market] = cats.Eq by (_.entity)

    def quote[F[_]: Monad, C: Currency](
        m: Market
    )(
        id: Instrument.Key
    ): F[Money[MonetaryAmount, C]] = ???
    // Currency[C] apply (Financial[MonetaryAmount] from quotedIn(m)(id).mid)

    def quotedIn[C: Currency](m: Market)(id: Instrument.Key): Instrument.Key QuotedIn C = ???

    /**
      * Single effective counterparty: the `Exchange` itself.
      *   - [[refinements.Mic]]s are unique.
      *   - seller for all buyers and vice versa.
      *   - activity recorded in a `contra account`
      */
    sealed abstract case class Exchange private (
        final val mic: Mic,
        final val contra: Account.Key,
        final val entity: Option[LegalEntity.Key]
    ) extends Market

    /** Fluent constructors. */
    object Exchange {
      def fromMic(mic: Mic): Exchange = ??? // make new contra account

      /** */
      def withEntity(lek: LegalEntity.Key): Exchange => Exchange =
        x => new Exchange(x.mic, x.contra, lek.some) {}
    }

    /**
      * Models a direct deal facing a private party.
      *
      * Their [[Ledger]] [[Account]] is
      * assumed to be "real" (not a contra account) although it can assume those characteristics
      * when the `Counterparty` is sourcing `Instruments` from private flows.
      *   - (eg exempt Securities for accredited individuals or qualified institutions)
      */
    sealed abstract case class Counterparty(val key: LegalEntity.Key) extends Market
    object Counterparty {}

    implicit def freshMarketKey: Fresh[Key] = Fresh.zeroBasedIncr

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
    def currency = Currency[C]
  }

  /** */
  object Order extends WithOpaqueKey[Long, Order[USD]] {

    def market[C: Currency](market: Market.Key, trade: Trade): Order[C] =
      new Order(market, instant, trade, none) {}

    def limit[C: Currency](market: Market.Key, trade: Trade, limit: Money[MA, C]): Order[C] =
      new Order(market, instant, trade, limit.some) {}

    /** `Market` orders */
    def buy[C: Currency]: Order[C]  = ???
    def sell[C: Currency]: Order[C] = ???

    /** `Limit` orders */
    def buy[C: Currency](bid: Money[MonetaryAmount, C]): Order[C]  = ???
    def sell[C: Currency](ask: Money[MonetaryAmount, C]): Order[C] = ???

    implicit def freshOrderKey: Fresh[Key] = Fresh.zeroBasedIncr

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
  object Execution extends WithOpaqueKey[Long, Execution] {
    implicit def freshExecutionKey: Fresh[Key] = Fresh.zeroBasedIncr
  }

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
    * FIXME: Maybe make this a full MTL thing because SemigroupK for Kleisli is restrictive.
    * OTOH it *is* a pipeline and so the Kleisli modeling has fidelity.
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

    final def process[C: Currency, A](p: Account.Key, a: Allocation)(
        block: => A
    ): FK[A, Transaction] =
      riskCheck[C, A](p)(block) andThen trade[C](p) andThen allocate[C](a) andThen settle[C](p)

    def riskCheck[C: Currency, A](p: Account.Key)(a: A): FK[A, Order[C]] =
      ???

    def trade[C](p: Account.Key): FK[Order[C], Execution] = ???

    private final def allocate[C: Currency](
        a: Allocation
    ): FK[Execution, Execution] =
      ???

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

    private def newContraAccount: Account.Key = ???
  }

  /** top level package methods */
  def quoteLeg[C: Currency](market: Market)(leg: Leg): Money[MonetaryAmount, C] =
    leg match {
      case (security, quantity) => Market.quote[cats.Id, C](market)(security) * quantity
    }

  def quote[C: Currency](market: Market)(trade: Trade): Money[MonetaryAmount, C] =
    trade.toList foldMap quoteLeg[C](market)

  /**
    * TODO: Revisit the privacy issues here.
    * Account keys are are needed e.g to verify sigs and allocate partitions
    * Can we break this down into two methods: anonymous, and not?
    */
  def recorded[F[_]: Foldable: Monad: SemigroupK](
      fs: Folio.Table,
      accounts: Account.Table
  ): Execution => F[Folio.Table] = ???
}
