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

import time._
import time.implicits._

import money._
import Currency.USD

import opaqueid._
import OpaqueId.Fresh

import repos._

import reference.InstrumentIdentifier

import cats.{ Eq, Foldable, Hash, Invariant, Monad, Monoid, MonoidK, Order }
import cats.kernel.CommutativeGroup
import cats.data.Kleisli
import cats.implicits._
import feralcats.instances._

import eu.timepit.refined
import refined.{ cats => refinedCats, _ }
import refined.api.Refined
import refined.numeric._
import refined.string._
import refined.auto._

import io.circe.Json

import scala.language.higherKinds

/** FIXME wtf is this even, besides eponymous */
sealed abstract class Fail extends Product with Serializable { def msg: String }
object Fail {
  final case class Impl(val msg: String) extends Fail
  def apply(msg: String): Fail = Impl(msg)
}

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
  */
abstract class Trading[MA: Financial, Q: Financial] extends Ledger[MA, Q] { api =>

  import Quantity.{ fractional => QF, commutativeGroup => QCG }
  import MonetaryAmount.{ fractional => MAF, commutativeGroup => MACG }


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
      */
    final case class OMS private (eid: Entity.Id, contra: Account.Id, markets: Set[Market]) {

      import OMS.{ Allocation, Execution, Order }

      def riskCheck[F[_]: Monad: MonoidK, C, X](x: X): Kleisli[F, X, Order[C]] =
        ???

      def trade[F[_]: Monad: MonoidK, C]: Kleisli[F, Order[C], Execution] =
        ???

      def allocate[F[_]: Monad: MonoidK, C](
          p: Account.Id,
          a: Allocation
      ): Kleisli[F, Execution, Execution] =
        ???

      def process[F[_]: Monad: MonoidK, C](
          p: Account.Id,
          a: Allocation
      ): Kleisli[F, Order[C], Execution] = ???
      // riskCheck(()) and then trade andThen allocate(p, a) FIXME: divirging implicits wtf???
    }
    object OMS {

      implicit def omsEq = Eq.fromUniversalEquals[OMS]

      type Allocation = Partition[Account.Id]

      /**
        * TODO: augment/evolve creation pattern.
        */
      def apply(eid: Entity.Id, ms: Market*): OMS = OMS(eid, newContraAccount, ms.toSet)
      private def newContraAccount: Account.Id    = ???

      /**
        * Minimum viable `Order` type. What the client would _like_ to have happen.
        */
      type Order[C] = (Market, AccountAuth, LocalDateTime, Trade, Option[Money[MA, C]])
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
      type Execution = (LocalDateTime, OMS, Order.Id, Transaction)
      object Execution extends IdC[Long, Execution]

      type Executions = Executions.Table
      object Executions extends MemAppendableRepository[cats.Id, Execution.Id, Execution]

    }
  }

  /**
    * Price all the things.
    * TODO: Revisit the implicit binding between market data (quotes) and `Exchanges`.
    * - this isn't how data typically comes, especially cheap data.
    * - smart routers (and internalizers!) introduce another layer of conceptual complexity
    * - nonetheless, these are the semantics (bound) any sane developer needs... if best effort
    * isn't good enough, don't offer it.
    */
  sealed abstract class Market { def eid: Entity.Id }
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
  //   {
  //   case (oid, _, trade, _, _) =>
  //     (Orders get oid) match {
  //       case Some((aid, _, _, _, _, _)) =>
  //         accounts(aid) match {
  //           case Account(roster, vault) =>
  //             // TODO: check roster permissions
  //             roster |> discardValue
  //             vault match {
  //               case Vault.Folio(folioId) =>
  //                 Monad[IO] pure { fs.updated(folioId, (fs get folioId).fold(Folio.empty)(_ |+| trade)) }
  //               case Vault.SubAccounts(subs) =>
  //                 subs |> discardValue
  //                 ??? // wut
  //             }
  //         }
  //       case _ => error
  //     }
  // }


}
