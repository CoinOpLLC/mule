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

import keys.Instrument
import keyval._, repos._, time._, money._, pricing._
import Currency.USD

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
  * - we keep contra accounts per OMS gateway
  * - we debit that account when we "buy shares" (creates negative balance)
  * - we credit that account when settlement happens (zeros out the balance)
  * - we "reverse polarity" when we enter a short position.
  * - we can indexAndSum settled positions for reconcilliation
  *
  * TODO - provisional:
  * we depend on `Balances` because it makes no sense
  * to trade blind with respect to account sums - these calcs are intrinsic to margin (I think)
  */
abstract class Trading[MA: Financial, Q: Financial] extends Balances[MA, Q] { api =>

  /** All things financial begin with an allocation. */
  type Allocation = UnitPartition[Account.Key, Quantity]

  /** Namespace placeholder */
  object Allocation

  /**
    * Price all the things.
    * TODO: Revisit the implicit binding between market data (quotes) and `Exchanges`.
    * - this isn't how data typically comes, especially cheap data.
    * - smart routers (and internalizers!) introduce another layer of conceptual complexity
    * - nonetheless, these are the semantics (bound) any sane developer needs... if best effort
    * isn't good enough, don't offer it.
    */
  sealed trait Market { def key: LegalEntity.Key }
  implicit def marketCatsOrder: cats.Order[Market] = cats.Order by (_.key)
  object Market extends WithOpaqueKey[Long, Market] {

    def quote[F[_]: Monad, C: Currency](
        m: Market
    )(
        id: Instrument.Key
    ): F[Money[MonetaryAmount, C]] = ???
    // Currency[C] apply (Financial[MonetaryAmount] from quotedIn(m)(id).mid)

    def quotedIn[C: Currency](m: Market)(id: Instrument.Key): Instrument.Key QuotedIn C = ???

    /**
      * Single effective counterparty: the `Exchange` itself.
      * - seller for all buyers and vice versa.)
      * - activity recorded in a `contra account`
      */
    final case class Exchange(key: LegalEntity.Key, contraAk: Account.Key) extends Market
    object Exchange {}

    /**
      * Models a direct deal facing a private `Counterparty`. Their `Ledger` `Account` is
      * assumed to be "real" (not a contra account) although it can assume those characteristics
      * when the `Counterparty` is sourcing `Instruments` from private flows.
      * - (e.g. exempt Securities for accredited individuals or qualified institutions)
      */
    final case class Counterparty(val key: LegalEntity.Key) extends Market
    object Counterparty {}

    implicit def eqMarket: Eq[Market] = Eq by (_.key) // FIXME: fuck this shit

    implicit def freshMarketKey: Fresh[Key] = Fresh.zeroBasedIncr

  }

  /**
    * Minimum viable `Order` type. What the client would _like_ to have happen.
    * TODO: revisit parent/child orders
    * FIXME: Eliminate `C`. Normalize the trade. MA and Currency as separate fields.
    */
  final case class Order[C: Currency](
      market: Market.Key,
      // auth: AccountAuth,
      ts: Instant,
      trade: Trade,
      limit: Option[Money[MA, C]]
  ) {
    def currency = Currency[C]
  }

  /** */
  object Order extends WithOpaqueKey[Long, Order[USD]] {

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
  final case class Execution(ts: Instant, oms: OMS.Key, orderKey: Order.Key, tx: Transaction)

  object Execution extends WithOpaqueKey[Long, Execution] {
    implicit def freshExecutionKey: Fresh[Key] = Fresh.zeroBasedIncr
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
    * FIXME: Maybe make this a full MTL thing because SemigroupK for Kleisli is restrictive.
    * OTOH it *is* a pipeline and so the Kleisli modeling has fidelity.
    */
  final case class OMS[F[_]: Monad: SemigroupK: Foldable] private (
      key: LegalEntity.Key,
      contra: Account.Key,
      markets: NonEmptySet[Market]
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
        market: Market,
        ms: Market*
    ): OMS[F] =
      OMS[F](key, newContraAccount, NonEmptySet(market, SortedSet(ms: _*)))

    private def newContraAccount: Account.Key = ???
  }

  /** */
  object Repos extends WithOpaqueKey[Long, OMS[cats.Id]] {

    /** */
    type LegalEntities = LegalEntity.Table

    /** */
    implicit def w00t: Fresh[LegalEntity.Key] = ??? // to compile duh
    /** */
    object LegalEntities extends SimplePointInTimeRepository[cats.Id, LegalEntity.Key, LegalEntity]

    /**
      * `CashInstruments`:
      * - is a configuration parameter only.
      * - is not as a repository, or store.
      * - shall never have F[_] threaded through it.
      *
      * All `Currency` instances in scope are required to have a `CashInstruments` instance.
      */
    object CashInstruments {

      def apply[C: Currency]: Wallet[C] = (cash get Currency[C]).fold(???) { x =>
        Wallet apply [C] Folios(x)
      }

      private lazy val cash: Map[CurrencyLike, Folio.Key] = Map.empty
    }

    /** */
    type CashInstruments = Wallet.Table

    /** */
    object Instruments extends MemInsertableRepository[cats.Id, Instrument.Key, Instrument]

    /** */
    type Instruments = Instrument.Table

    /** */
    object Folios extends SimplePointInTimeRepository[cats.Id, Folio.Key, Folio] {
      def apply(id: Folio.Key): Folio = get(id).fold(Folio.empty)(identity)
    }

    /** */
    type Folios = Folio.Table

    /** */
    implicit def freshAccountNo: Fresh[Account.Key] = ??? // to compile duh
    /** */
    object Accounts extends SimplePointInTimeRepository[cats.Id, Account.Key, Account]

    /** */
    type Accounts = Account.Table

    /** FIXME: this is just a placeholder - needs to reference [[Transaction]] */
    type Transactions[F[_]] = Foldable[F]

    /** FIME this becomes a stream like repo (???)      */
    object Transactions {}

    /** */
    lazy val Markets: Repository[cats.Id, Market.Key, Market] =
      SimplePointInTimeRepository[cats.Id, Market.Key, Market]()

    /** */
    type Markets = Markets.Table

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

    /** */
    object Orders extends SimplePointInTimeRepository[cats.Id, Order.Key, Order[USD]]

    /**  n.b. `Exectutions` are recorded as [[Transactions]] this completing the life cycle */
    type Executions = Executions.Table

    /** */
    object Executions extends MemAppendableRepository[cats.Id, Execution.Key, Execution]

  }

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

  type Folios   = Folio.Table
  type Accounts = Account.Table

  /**
    * FIXME Accounts are needed to verify sigs and allocate partitions
    * BUT can we break this down into two methods (anonymous, and not)
    */
  def recorded[F[_]: Foldable: Monad: SemigroupK](
      fs: Folios,
      accounts: Accounts
  ): Execution => F[Folios] = ???
}
