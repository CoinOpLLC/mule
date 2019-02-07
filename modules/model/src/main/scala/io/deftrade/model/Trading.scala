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

import kves._
import time._
import money._, pricing._
import repos._
import Currency.USD

import cats.{ Eq, Foldable, Monad, MonoidK }
import cats.data.{ Kleisli, NonEmptySet }
import cats.implicits._

import eu.timepit.refined
import refined.auto._

import scala.collection.immutable.SortedSet
import scala.language.higherKinds

// final case class Fail(val msg: String)

/**
  * What does "double entry bookkeeping" mean in the context of a shared distributed ledger with
  * OMS gateways to external markets?
  *
  * It means this:
  * - we keep contra accounts per OMS gateway
  * - we debit that account when we "buy shares" (creates negative balance)
  * - we credit that account when settlement happens (zeros out the balance)
  * - we "reverse polarity" when we enter a short position.
  * - we can accumulate settled positions for reconcilliation
  *
  * TODO - provisional:
  * we depend on `Balances` because it makes no sense
  * to trade blind with respect to account sums - these calcs are intrinsic to margin (I think)
  */
abstract class Trading[MA: Financial, Q: Financial] extends Balances[MA, Q] { api =>

  /** `Leg` := `Position` in motion */
  type Leg = Position
  lazy val Leg = Position

  /** `Trade` := `Folio` in motion */
  type Trade = Folio
  lazy val Trade = Folio

  type PricedTrade[C] = (Trade, Money[MonetaryAmount, C])
  object PricedTrade {

    /**
      * Used to convert to the currency as `Instrument` convention.
      * Consider the set of `Instrument`s which represent bank account balances in dollars.
      * What is the set of "payable on demand" dollar instruments?
      * This dictates the normalization.
      */
    def normalize[C](pt: PricedTrade[C])(implicit ci: CashInstruments): Trade = ???
  }

  /**
    *`OMS` := Order Management System. Ubiquitous acronym in the domain.
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
    * FIXME: Maybe make this a full MTL thing because MonoidK for Kleisli is restrictive.
    * OTOH it *is* a pipeline and so the Kleisli modeling has fidelity.
    */
  final case class OMS[F[_]: Monad: MonoidK: Foldable] private (
      eid: Entity.Id,
      contra: Account.Id,
      markets: NonEmptySet[Market]
  ) {

    import OMS.{ Allocation, Execution, Order }

    /** He stacc. He refacc. */
    type FK[T, R] = Kleisli[F, T, Either[Fail, R]]

    final def process[C: Currency, A](p: Account.Id, a: Allocation)(
        block: => A
    ): FK[A, Execution] = {
      val risk: FK[A, Order[C]]           = riskCheck(block)
      val tr: FK[Order[C], Execution]     = trade
      val alloc: FK[Execution, Execution] = allocate(p, a)
      // FIXME this used to work
      // and by work I mean compile ;)
      // risk andThen tr andThen alloc
      ???
    }

    def riskCheck[C: Currency, A](a: A): FK[A, Order[C]] =
      ???

    def trade[C]: FK[Order[C], Execution] = ???

    private final def allocate[C: Currency](
        p: Account.Id,
        a: Allocation
    ): FK[Execution, Execution] = // UM... this is recursive, given account structure. Fine!
      ???
  }

  // TODO: revisit, systematize
  implicit def omsEq[F[_]: Monad] = Eq.fromUniversalEquals[OMS[F]]

  /**
    * Where do Order Management Systems come from? (Here.)
    */
  object OMS extends IdC[Long, OMS[cats.Id]] {

    type Allocation = Partition[Account.Id, Quantity]

    /**
      * TODO: augment/evolve creation pattern.
      */
    def apply[F[_]: Monad: MonoidK: Foldable](eid: Entity.Id, market: Market, ms: Market*): OMS[F] =
      OMS[F](eid, newContraAccount, NonEmptySet(market, SortedSet(ms: _*)))

    private def newContraAccount: Account.Id = ???

    /**
      * Minimum viable `Order` type. What the client would _like_ to have happen.
      */
    final case class Order[C: Currency](
        market: Market,
        auth: AccountAuth,
        ts: Instant,
        trade: Trade,
        limit: Option[Money[MA, C]]
    ) {
      def currency = Currency[C]
    }
    implicit def orderEq[C: Currency] = Eq.fromUniversalEquals[Order[C]]

    /** */
    object Order extends IdC[Long, Order[USD]] {

      /** `Market` orders */
      def buy[C: Currency]: Order[C]  = ???
      def sell[C: Currency]: Order[C] = ???

      /** `Limit` orders */
      def buy[C: Currency](bid: Money[MonetaryAmount, C]): Order[C]  = ???
      def sell[C: Currency](ask: Money[MonetaryAmount, C]): Order[C] = ???
    }

    /**
      *
      *  this is something of an abuse of the original PiT concept,
      * which models slowly evolving entities *with identity (key) which survives updates.
      *
      *  `Orders` is exactly the opposite.
      *
      *  But the open date range for "current `Table`" models the "open orders" concept perfectly.
      *
      *  TODO: is this really worthwhile?
      *
      */
    type Orders = Orders.Table
    object Orders extends SimplePointInTimeRepository[cats.Id, OMS.Order.Id, OMS.Order[USD]]

    /**
      * What actually happened.
      */
    final case class Execution(ts: Instant, oms: OMS.Id, oid: Order.Id, tx: Transaction)

    /** Executions are sorted first by Order.Id, and then by timestamp – that should do it! ;) */
    implicit def catsOrderExecution: Eq[Execution] = cats.Order by (x => x.oid) // FIXME add ts
    // FIXME here's your problem right here cats.Order[Instant] |> discardValue
    object Execution extends IdC[Long, Execution]

    type Executions = Executions.Table
    object Executions extends MemAppendableRepository[cats.Id, Execution.Id, Execution]
  }

  /**
    * Price all the things.
    * TODO: Revisit the implicit binding between market data (quotes) and `Exchanges`.
    * - this isn't how data typically comes, especially cheap data.
    * - smart routers (and internalizers!) introduce another layer of conceptual complexity
    * - nonetheless, these are the semantics (bound) any sane developer needs... if best effort
    * isn't good enough, don't offer it.
    */
  sealed trait Market { def eid: Entity.Id }
  implicit def marketCatsOrder: cats.Order[Market] = cats.Order by (_.eid)
  object Market extends IdC[Long, Market] {

    def quote[F[_]: Monad, C: Currency](
        m: Market
    )(
        id: Instrument.Id
    ): F[Money[MonetaryAmount, C]] = ???
    // Currency[C] apply (Financial[MonetaryAmount] from quotedIn(m)(id).mid)

    def quotedIn[C: Currency](m: Market)(id: Instrument.Id): Instrument.Id QuotedIn C = ???

    /**
      * Single effective counterparty: the `Exchange` itself.
      * - seller for all buyers and vice versa.)
      * - activity recorded in a `contra account`
      */
    final case class Exchange(eid: Entity.Id, contraAid: Account.Id) extends Market
    object Exchange {}

    /**
      * Models a direct deal facing a private `Counterparty`. Their `Ledger` `Account` is
      * assumed to be "real" (not a contra account) although it can assume those characteristics
      * when the `Counterparty` is sourcing `Instruments` from private flows.
      * - (e.g. exempt Securities for accredited individuals or qualified institutions)
      */
    final case class Counterparty(val eid: Entity.Id) extends Market
    object Counterparty {}

    implicit def eqMarket: Eq[Market] = Eq by (_.eid) // FIXME: fuck this shit
  }

  lazy val Markets: Repository[cats.Id, Market.Id, Market] =
    SimplePointInTimeRepository[cats.Id, Market.Id, Market]()
  type Markets = Markets.Table

  /** top level methods */
  def quoteLeg[C: Currency](market: Market)(leg: Leg): Money[MonetaryAmount, C] =
    leg match {
      case (security, quantity) => Market.quote[cats.Id, C](market)(security) * quantity
    }

  def quote[C: Currency](market: Market)(trade: Trade): Money[MonetaryAmount, C] =
    trade.toList foldMap quoteLeg[C](market)

  def ordered = ???
  sealed trait Ordered

  def executed = ???
  sealed trait Executed

  sealed trait Settled
  def settled = ???

  def error = ???

  /**
    * FIXME Accounts are needed to verify sigs and allocate partitions
    * BUT can we break this down into two methods (anonymous, and not)
    */
  def recorded[F[_]: Foldable: Monad: MonoidK](
      fs: Folios,
      accounts: Accounts
  ): OMS.Execution => F[Folios] = ???
}
