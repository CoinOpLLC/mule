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

import enums._

import repos._

import opaqueid._

import time._
import time.implicits._

import money._
import Monetary.USD

import cats.{ Eq, Foldable, Hash, Invariant, Monad, Monoid, MonoidK }
import cats.implicits._
import cats.kernel.CommutativeGroup
import feralcats.instances._

import eu.timepit.refined
import refined.{ cats => refinedCats, _ }
import refined.api.Refined
import refined.numeric._
import refined.string._
import refined.auto._

import io.circe.Json

import scala.language.higherKinds

sealed abstract class Fail extends Product with Serializable { def msg: String }
object Fail {
  final case class Impl(val msg: String) extends Fail
  def apply(msg: String): Fail = Impl(msg)
}

/**
  * This shall be the law of the Api: A `type Foo` may not depend on a `type FooId`.
  * This shall be another: only member names whose appearence cannot be helped may appear here.
  *
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
abstract class Api[MA: Financial, Quantity: Financial] { api =>
  import io.deftrade.reference._

  type MonetaryAmount = MA

  /** Domain specific tools for dealing with `Quantity`s */
  val Quantity = Financial[Quantity]
  import Quantity.{ fractional => QF, commutativeGroup => QCG }

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  val MonetaryAmount = Financial[MonetaryAmount]
  import MonetaryAmount.{ fractional => MAF, commutativeGroup => MACG }

  /********************************************************************************************
    * Folio space
  *******************************************************************************************/
  type UII = UniversalInstrumentIdentifyer

  /** TODO: use the XBRL definitions for these, a la OpenGamma */
  final case class Instrument(meta: Json)
  object Instrument {
    type Id = InstrumentIdentifier
    val Id                          = InstrumentIdentifier
    implicit def eq: Eq[Instrument] = Eq by (_.meta)
  }
  object Instruments extends MemInsertableRepository[cats.Id, Instrument.Id, Instrument]
  type Instruments = Instruments.Table

  /**
  How much of a given `Instrument` is held.
  Can also be thought of as a `Leg` at rest.
    */
  type Position = (Instrument.Id, Quantity)
  object Position

  /** `Leg` := `Position` in motion */
  type Leg = Position
  lazy val Leg = Position

  /**
  A `Folio` is a set of `Position`s
  Can also be thought of as a `Trade` at rest.
    */
  type Folio = Map[Instrument.Id, Quantity]
  object Folio extends IdC[Long, Folio] {
    def empty: Folio                = Map.empty
    def apply(ps: Position*): Folio = accumulate(ps.toList)
  }

  /** `Trade` := `Folio` in motion */
  type Trade = Folio
  lazy val Trade = Folio

  object Folios extends SimplePointInTimeRepository[cats.Id, Folio.Id, Folio] {
    def apply(id: Folio.Id): Folio = get(id).fold(Folio.empty)(identity)
  }
  type Folios = Folios.Table

  /** The `Ledger` is just the universe of `Folio`s*/
  type Ledger = Folios
  lazy val Ledger = Folios

  type Signature = String      // placeholder
  type Sha256    = Array[Byte] // ditto
  /**
    * For `Ledger` changes, the `Transaction` is the concrete record of record, so to speak.
    */
  final case class Transaction(
      /**
        * A `LocalDateTime` is required of all `Recorded Transaction`s, assigned by the `Recorder`
        * - the transaction is provisional until dated, returned as a receipt
        * The exact semantics can vary depending on the higher level context
        * - (e.g. booking a trade vs receiving notice of settlement).
        */
      recorded: Option[LocalDateTime],
      /**
        * *Exactly* two parties to a `Transaction`.
        * - Use `AllOrNone` to compose multiparty `Transaction`s
        */
      parties: (Folio.Id, Folio.Id),
      /**
        * Note: cash payment is reified in currency-as-instrument.
        */
      trade: Trade,
      /**
        * In the `Ledger`, store the _cryptographic hash_ of whatever metadata there is.
        */
      metaSha: Sha256
  )
  object Transaction {
    implicit def order: Eq[Transaction] = Eq.fromUniversalEquals[Transaction]
  }

  /** Support for multiple contingent deal legs */
  final case class AllOrNone(xs: List[Transaction])

  /********************************************************************************************
    * `Account` space
    *******************************************************************************************/
  sealed trait Entity extends Product with Serializable { def meta: Json }
  object Entity extends IdC2[Long, Entity] {

    object SSN {
      type Pattern = W.`"[0-9]{3}-[0-9]{2}-[0-9]{4}"`.T
      type Regex   = MatchesRegex[Pattern]
    }
    type SSN = String Refined SSN.Regex

    object EIN {
      type Pattern = W.`"[0-9]{2}-[0-9]{7}"`.T
      type Regex   = MatchesRegex[Pattern]
    }
    type EIN = String Refined EIN.Regex

    final case class Person(val ssn: SSN, val meta: Json)      extends Entity
    final case class Corporation(val ein: EIN, val meta: Json) extends Entity

    implicit def eq = Eq.fromUniversalEquals[Entity]
  }

  object Entities extends SimplePointInTimeRepository[cats.Id, Entity.Id, Entity]
  type Entities = Entities.Table

  /**
    * Note that Roster encodes the notion of a `CapitalKey` – per `Role`!
    * The meaning of this `Partition` will depend upon the role.
    * For the beneficial owner(s) of the assets in question the meaning is obvious.
    * For a `Manager`, the `Partition` could encode their voting rights – which may differ from
    * their ownership rights.
    */
  type Partition[K] = Map[K, Quantity]
  object Partition {

    import QF._
    // import Quantity._

    def apply[K: Hash](shares: (K, Quantity)*): Result[Partition[K]] = {
      val s = shares.toList
      if ((s.map(_._2).fold(zero)(plus(_, _)) == one) && (s forall { case (_, q) => validate(q) })) s.toMap.asRight
      else ???
    }

    def apply[K: Hash](totalShares: Long)(ps: (K, Long)*): Partition[K] = ps.toList.toMap map {
      case (k, l) => (k, div(one, fromLong(totalShares)))
    }

    /**
      * The portion of the total accorded to the identified `Entity`.
      */
    def validate(q: Quantity): Boolean          = zero <= q && q <= one
    def validate(total: Long)(n: Long): Boolean = 0L <= n && n <= total
    // def apply[K](shares: Share[K]*): Either[String, Partition[K]] = {
    //   val p = accumulate(shares.toList)
    //   p.values.toList foldMap identity match {
    //     case sum if sum === QF.one => p.asRight
    //     case badsum                => s"Partition doesn't add up: $badsum != 1.0".asLeft
    //   }
    // }
    // def single[K: Hash](k: K): Partition[K] = Map.empty + Share.single[K](k)

    def proRata[K: Hash](capTable: Map[K, Quantity]): Partition[K] = {
      import spire.implicits._
      val totalShares = (capTable map { case (_, shares) => shares }).toList |> QF.sum
      capTable map { case (k, mySlice) => (k, mySlice / totalShares) }
    }

    def pariPassu[K: Hash](ks: Set[K]): Partition[K] = {
      import spire.implicits._
      val n = ks.size
      (ks.toList zip List.fill(n)(one / fromInt(n))).toMap
    }

    object Single {

      def unapply[K](p: Partition[K]): Option[K] = p.toList match {
        case (k, Quantity.One) :: Nil => k.some
        case _                        => none
      }
    }
  }

  /**
    * justification: can't know / can't be responsible for Partition on the entities (i.e. is
  it 50-50? 98-1-1? etc...). This is especially true for non-principle roles like `Regulator`.
    */
  type Roster = Map[Role, Set[Entity.Id]]
  object Roster {
    def apply(eid: Entity.Id): Roster =
      Map.empty + (Role.Principle -> Set(eid))
  }

  /**
    * `Vault` is a sum type used in its capacity as an obfuscator ;)
    */
  sealed trait Vault
  object Vault {

    case class SubAccounts(subs: Set[Account.Id]) extends Vault
    case class Folio(fid: api.Folio.Id)           extends Vault

    def empty: Vault = SubAccounts(Set.empty) // idiom
  }

  /**
    * How composition of `Roster`s and `Vault::SubAccount`s works:
    * - conjuction.
    * - that's it (you're welcome.)
    */
  case class Account(roster: Roster, vault: Vault)
  object Account {
    type ValidRange = Interval.Closed[W.`100000100100L`.T, W.`999999999999L`.T]
    type Id         = Long Refined ValidRange
    object Id {
      implicit def order: cats.Order[Id] = cats.Order by { _.value }
      implicit lazy val fresh: Fresh[Id] =
        StrictFresh[Id](100000100100L, id => refineV[ValidRange](id + 1L).fold(_ => ???, identity))
    }

    def empty(eid: Entity.Id) = Account(Roster(eid), Vault.empty)

    def simple(eid: Entity.Id, fid: Folio.Id) = Account(Roster(eid), Vault.Folio(fid))

    implicit def eq = Eq.fromUniversalEquals[Account]
  }

  import Account.Id._ // for implicits

  object Accounts extends SimplePointInTimeRepository[cats.Id, Account.Id, Account]
  type Accounts = Accounts.Table

  /********************************************************************************************
    * make stuff happen space
    *******************************************************************************************/
  /**
    * TODO: this scheme is adequate for my purposes, but a ZK validation scheme which didn't expose
    * the `Account.Id`'s would be ideal.
    * another approoach would be to merkelize the `Roster` to expose only the relevant bits.
    */
  type AccountAuth           = (Account.Id, Signature)
  type FolioAuth             = (Folio.Id, AccountAuth)
  type FolioAuths            = (FolioAuth, FolioAuth)
  type AuthorizedTransaction = (Transaction, FolioAuths)

  /**
    * Make sure I can plug in fs2.Stream[cats.effect.IO, ?] etc here
    */
  type Journal[F[_], A] = F[A]
  object Journal { // non empty by def?

    def empty[F[_]: Monad: Foldable: MonoidK, A]: Journal[F, A] =
      MonoidK[F].empty[A]

    /** fold over me */
    final case class Entry(
        ax: AuthorizedTransaction,
        meta: Json
    )
  }

  /**
    *`OMS` := Order Management System. Ubiquitous acronym in the domain.
    */
  final case class OMS(eid: Entity.Id, contraAid: Account.Id) {

    import OMS.{ Allocation, Execution, Order }

    def trade[F[_]: Foldable: Monad: MonoidK, C <: Currency]: Order[C] => F[Execution] =
      ???

    def allocate[F[_]: Foldable: Monad: MonoidK, C <: Currency](
        parent: Account.Id,
        a: Allocation
    ): Execution => F[Execution] =
      ???
  }
  object OMS {

    type Allocation = Partition[Account.Id]

    /**
      *
      */
    type Order[C <: Currency] = (AccountAuth, LocalDateTime, Trade, Option[Money[MA, C]])
    object Order extends IdC[Long, Order[USD]] {

      /** Market orders */
      def buy[C <: Currency: Monetary]: Order[C]  = ???
      def sell[C <: Currency: Monetary]: Order[C] = ???

      /** limit orders */
      def buy[C <: Currency: Monetary](bid: Money[MonetaryAmount, C]): Order[C]  = ???
      def sell[C <: Currency: Monetary](ask: Money[MonetaryAmount, C]): Order[C] = ???
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
    object Orders extends SimplePointInTimeRepository[cats.Id, OMS.Order.Id, OMS.Order[USD]]

    type Execution = (Market, LocalDateTime, Order.Id, Transaction)
    object Execution extends IdC[Long, Execution]

    object Executions extends MemAppendableRepository[cats.Id, Execution.Id, Execution]
  }

  type PricedTrade[C <: Currency] = (Trade, Money[MonetaryAmount, C])
  object PricedTrade {

    /** Used to convert to the currency as `Instrument` convention */
    def apply[C <: Currency](pt: PricedTrade[C]): Trade = ???
  }

  implicit def eqMarket: Eq[Market] = Eq by (_.eid) // FIXME: fuck this shit
  sealed abstract class Market { def eid: Entity.Id }
  object Market extends IdC[Long, Market] {

    /** Price all the things. */
    def quote[C <: Currency: Monetary](m: Market)(id: Instrument.Id): Money[MonetaryAmount, C] =
      Monetary[C] apply (Financial[MonetaryAmount] from quotedIn(m)(id).mid)

    def quotedIn[C <: Currency: Monetary](m: Market)(id: Instrument.Id): Instrument.Id QuotedIn C = ???

    /**
      * Single effective counterparty: the `Exchange` itself.
      * - seller for all buyers and vice versa.)
      */
    final case class Exchange(eid: Entity.Id, contraAccount: Account.Id) extends Market
    object Exchange

    /**
      * Models a direct deal facing a private `Counterparty`. Their `Ledger` `Account` is
      * assumed to be "real" (not a contra account) although it can assume those characteristics
      * when the `Counterparty` is sourcing `Instruments` from private flows.
      * - (e.g. exempt Securities for accredited individuals or qualified institutions)
      */
    final case class Counterparty(val eid: Entity.Id) extends Market
    object Counterparty
  }

  lazy val Markets: Repository[cats.Id, Market.Id, Market] =
    SimplePointInTimeRepository[cats.Id, Market.Id, Market]()
  type Markets = Markets.Table

  /********************************************************************************************
    * `Accounting` space
    *******************************************************************************************/
  /**
    * Recall the fundamental equation of accounting:
    *
    *  `Debit` === `Credit`
    *  `Assets` + `Expenses` === `Liabilities` + `Equity` + `Revenues`
    *
    *  Balance(Assets, LOQs)
    *  Balance(XOP, Revenues)  // !!!
    *
    */
  final type AccountMap[A <: AccountType] = Map[A, MonetaryAmount]
  object AccountMap {
    def empty[A <: AccountType]: AccountMap[A] = Map.empty[A, MonetaryAmount]
  }

  final type Debits  = AccountMap[Debit]
  final type Credits = AccountMap[Credit]

  final type XOPs = AccountMap[XOP]
  final type LOQs = AccountMap[LOQ]

  final type Assets      = AccountMap[Asset]
  final type Expenses    = AccountMap[Expense]
  final type Liabilities = AccountMap[Liability]
  final type Equities    = AccountMap[Equity]
  final type Revenues    = AccountMap[Revenue]

  sealed abstract class Balance[D <: Debit, C <: Credit](val ds: AccountMap[D], val cs: AccountMap[C]) extends Product with Serializable {

    // def updatedGrow(asset: Asset, liability: Liability)(amt: MonetaryAmount): BalanceSheet =
    //   BalanceSheet(assets + (asset -> amt), liabilities + (liability -> amt))
    //
    // def updated(from: Asset, to: Asset)(amt: MonetaryAmount): BalanceSheet =
    //   copy(assets = assets + (from -> amt.inverse) + (to -> amt))
    // def updated(dc: (D, C), amt: MonetaryAmount): Self = dc match {
    //   case (d, c) =>
    //     _mk((ds + (d -> amt), cs + (c -> amt.inverse)))
    // }
  }
  object Balance {}

  final case class TrialBalance private (
      override val ds: Debits,
      override val cs: Credits
  ) extends Balance(ds, cs) {

    lazy val partition: (IncomeStatement, BalanceSheet) = {

      def collect[T <: AccountType, R <: T](as: AccountMap[T]): AccountMap[R] = as collect {
        case (k: R, v) => (k, v)
      }
      val assets: AccountMap[Asset] = collect(ds)
      val loqs: AccountMap[LOQ]     = collect(cs)

      val xops: AccountMap[XOP]         = collect(ds)
      val revenues: AccountMap[Revenue] = collect(cs)

      (IncomeStatement(xops, revenues), BalanceSheet(assets, loqs))
    }

    def updated(uk: UpdateKey, amt: MonetaryAmount): TrialBalance =
      copy(ds + (uk.debit -> amt), cs + (uk.credit -> amt))

    def swapped[T <: AccountType](sk: SwapKey[T], amt: MonetaryAmount): TrialBalance = sk match {
      case AssetSwapKey(d1, d2) => copy(ds = ds + (d1 -> amt) + (d2 -> amt.inverse))
      case LOQSwapKey(c1, c2)   => copy(cs = cs + (c1 -> amt) + (c2 -> amt.inverse))
    }
  }
  object TrialBalance {
    implicit lazy val trialBalanceCommutativeGroup: CommutativeGroup[TrialBalance] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Debits, Credits)]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  final case class IncomeStatement private (
      val xops: XOPs,
      val revenues: Revenues
  ) extends Balance(xops, revenues)
  object IncomeStatement {
    implicit lazy val incomeStatementCommutativeGroup: CommutativeGroup[IncomeStatement] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(XOPs, Revenues)]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }
  }

  /**
  `BalanceSheet` forms a commutative group
    all balance sheet ops are double entry
    */
  final case class BalanceSheet private (
      val assets: Assets,
      val loqs: LOQs
  ) extends Balance(assets, loqs)
  object BalanceSheet {
    implicit lazy val balanceCommutativeGroup: CommutativeGroup[BalanceSheet] =
      Invariant[CommutativeGroup].imap(CommutativeGroup[(Assets, LOQs)]) {
        case (ds, cs) => apply(ds, cs)
      } {
        unapply(_).fold(???)(identity)
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  //  global functions yay
  //////////////////////////////////////////////////////////////////////////////////////////////////

  def quoteLeg[CCY <: Currency: Monetary](market: Market)(leg: Leg): Money[MonetaryAmount, CCY] =
    leg match {
      case (security, quantity) => Market.quote[CCY](market)(security) * quantity
    }

  def quote[CCY <: Currency: Monetary](market: Market)(trade: Trade): Money[MonetaryAmount, CCY] =
    trade.toList foldMap quoteLeg[CCY](market)

  def groupBy[F[_]: Foldable, A, K](as: F[A])(f: A => K): Map[K, List[A]] =
    as.foldLeft(Map.empty[K, List[A]]) { (acc, a) =>
      (acc get f(a)).fold(acc + (f(a) -> List(a))) { as =>
        acc + (f(a) -> (a +: as))
      }
    }

  def index[F[_]: Foldable, K, V](kvs: F[(K, V)]): Map[K, List[V]] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs map (_._2))
    }

  def accumulate[F[_]: Foldable, K, V: Monoid](kvs: F[(K, V)]): Map[K, V] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs foldMap (_._2))
    }

  def trialBalance[F[_]: Foldable: Monad: MonoidK](jes: Journal[F, Journal.Entry]): TrialBalance =
    ???

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

  def ordered = ???
  sealed trait Ordered

  def executed = ???
  sealed trait Executed

  sealed trait Settled
  def settled = ???

  def error = ???

  /**
    * FIXME: depends on mapping between `Folio`s and `Balance`s
    */
  final case class DoubleEntryKey(debit: Folio.Id, credit: Folio.Id)
  object DoubleEntryKey

}
