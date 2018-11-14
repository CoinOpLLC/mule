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
import Monetary.{ Monetary, Money }

import cats.{ Foldable, Invariant, Monad, Monoid }
import cats.implicits._
import cats.kernel.CommutativeGroup
import feralcats.instances._

import enumeratum.{ Enum, EnumEntry }

import eu.timepit.refined.{ cats => refinedCats, _ }
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric._
import eu.timepit.refined.auto._

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

  type UII = UniversalInstrumentIdentifyer

  /** TODO: use the XBRL definitions for these, a la OpenGamma */
  case class Instrument(json: String)
  object Instrument {
    type Id = InstrumentIdentifier
    val Id                               = InstrumentIdentifier
    implicit def eq: cats.Eq[Instrument] = cats.Eq by (_.json)
  }
  object Instruments extends MemInsertableRepository[cats.Id, Instrument.Id, Instrument]
  type Instruments = Instruments.Table

  type Position = (Instrument.Id, Quantity)
  object Position

  abstract class PositionSet[F[_]: Foldable: Monad] // ???

  type Folio = Map[Instrument.Id, Quantity] // n.b algebraic relationship Position <=> Folio
  object Folio extends IdC[Long, Folio] {
    def empty: Folio                = Map.empty
    def apply(ps: Position*): Folio = accumulate(ps.toList)
  }
  object Folios extends SimplePointInTimeRepository[cats.Id, Folio.Id, Folio] {
    def apply(id: Folio.Id): Folio = get(id).fold(Folio.empty)(identity)
  }
  type Folios = Folios.Table

  // Need to express `Foldable` relation between `Position` and `Folio`
  // (also, and indentically, between `Leg` and `Trade`)
  //

  // need some kind of map from folio (account) Roles (AR, Inventory, Revenue, JoeThePlumber, Cash) to concrete FolioIds

  case class LedgerKey(debit: Folio.Id, credit: Folio.Id)
  object LedgerKey

  object FolioTypes extends MemAppendableRepository[cats.Id, Folio.Id, AccountType]
  type FolioTypes = FolioTypes.Table
  // Map[AccountType, Set[FolioId]]

  /**
    * (Runtime) invariant: `Trade`s must balance across all the `Folio.Id`s in the
    * `LedgerEntry`.
    */
  type JournalEntry = (LocalDateTime, Map[Folio.Id, Trade], Money[MonetaryAmount, Monetary.USD])
  object JournalEntry

  type Transaction = JournalEntry
  val Transaction = JournalEntry

  /**
    */
  type Journal[F[_]] = F[JournalEntry]
  object Journal { // non empty by def?
    def apply[F[_]: Monad: Foldable](je: JournalEntry): F[JournalEntry] = Monad[F] pure je
  }

  type Person      = Json
  type Corporation = Json

  type Entity = Either[Corporation, Person]
  object Entity extends IdC[Long, Entity]

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

    /**
      * The portion of the total accorded to the identified `Entity`.
      */
    type Share[K] = (K, Quantity)
    object Share {
      def validate[K](p: Share[K]): Boolean = {
        val (_, q) = p
        zero <= q && q <= one
      }
      def apply[K](k: K, q: Quantity): Share[K] = (k, q) ensuring { p =>
        validate(p)
      }
      def single[K](k: K) = Share(k, QF.one)
    }

    def apply[K](shares: Share[K]*): Either[String, Partition[K]] = {
      val p = accumulate(shares.toList)
      p.values.toList.foldMap(identity) match {
        case sum if sum === QF.one => p.asRight
        case badsum                => s"Partition doesn't add up: $badsum != 1.0".asLeft
      }
    }
    def single[K](k: K): Partition[K] = Map.empty + Share.single[K](k)

    def proRata[K](totalShares: Quantity)(capTable: Map[K, Quantity]): Partition[K] = {
      import spire.implicits._
      capTable map { case (k, mySlice) => (k, mySlice / totalShares) }
    }

    def pariPassu[K](ks: Set[K]): Partition[K] = {
      import spire.implicits._
      val n = ks.size
      (ks.toList zip List.fill(n)(one / fromInt(n))).toMap
    }

    object Single {

      /** TODO: deal with other cases lol */
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

  case class Account(roster: Roster, vault: Vault)
  object Account {
    type IdRefinement = Interval.Closed[W.`100000100100L`.T, W.`999999999999L`.T]
    type Id           = Long Refined IdRefinement
    object Id {
      implicit def order: cats.Order[Id] = cats.Order by { _.value }
      implicit lazy val fresh: Fresh[Id] =
        StrictFresh[Id](100000100100L, id => refineV[IdRefinement](id + 1L).fold(_ => ???, identity))
    }
    def simple(fid: Folio.Id, eid: Entity.Id): Account = Account(Roster(eid), Vault.empty)
    implicit def eq                                    = cats.Eq.fromUniversalEquals[Account]
  }

  import Account.Id._ // Huh. OK...

  object Accounts extends SimplePointInTimeRepository[cats.Id, Account.Id, Account]

  /**
    * aliases.
    */
  type Ledger = Folios
  lazy val Ledger = Folios

  type Leg = Position // `Leg` := `Position` in motion
  lazy val Leg = Position

  type Trade = Folio // `Trade` := `Folio` in motion
  lazy val Trade = Folio // gives you `apply`: Trade(legs: Leg*): Trade

  // type Order = (AccountId, Entity.Id, Trade)
  type Order = (Account.Id, Entity.Id, LocalDateTime, Trade, money.MonetaryLike, Option[MonetaryAmount])
  object Order extends IdC[Long, Order] {
    def buy[CCY: Monetary]: Order                                   = ??? // market order
    def buy[CCY: Monetary](ask: Money[MonetaryAmount, CCY]): Order  = ???
    def sell[CCY: Monetary]: Order                                  = ???
    def sell[CCY: Monetary](ask: Money[MonetaryAmount, CCY]): Order = ???
  }

  /**

  this is something of an abuse of the original PiT concept, which models slowly evolving entities with identity (key) which survives updates.

  `Orders` is exactly the opposite.

  But the open date range for "current `Table`" models the "open orders" concept perfectly.

  TODO: is this really worthwhile?

    */
  object Orders extends SimplePointInTimeRepository[cats.Id, Order.Id, Order]

  type Allocation = Partition[Account.Id]
  object Allocation {

    def allocate(allocation: Allocation)(ex: Execution): List[Execution] = ???

  }

  type Execution = (Order.Id, LocalDateTime, Trade, MonetaryAmount, MonetaryLike)
  object Execution extends IdC[Long, Execution]

  object Executions extends MemAppendableRepository[cats.Id, Execution.Id, Execution]

  type PricedTrade[CCY] = (Trade, Money[MonetaryAmount, CCY])
  object PricedTrade

  case class Exchange(name: String) // a multiparty nexus
  object Exchange {
    implicit def eq: cats.Eq[Exchange] = cats.Eq by (_.name)
  }

  type Counter = Unit // a single counterparty or market maker
  object Counter

  type Market = Either[Counter, Exchange]
  object Market extends IdC[Long, Market] {

    /** Price all the things. */
    def quote[C: Monetary](m: Market)(id: Instrument.Id): Money[MonetaryAmount, C] =
      Monetary[C] apply (Financial[MonetaryAmount] from quotedIn(m)(id).mid)

    def quotedIn[C: Monetary](m: Market)(id: Instrument.Id): Instrument.Id QuotedIn C = ???

    // idea: use phantom types to capture sequencing constraint?
    def trade[IO[_]: Monad](m: Market): Order => IO[Seq[Execution]] = _ => ???

  }

  lazy val Markets: Repository[cats.Id, Market.Id, Market] =
    SimplePointInTimeRepository[cats.Id, Market.Id, Market]()
  type Markets = Markets.Table

  /** Recall the fundamental equation of accounting:
  `Debit` === `Credit`
  `Assets` + `Expenses` === `Liabilities` + `Equity` + `Revenues`

  Balance(Assets, LOQs)
  Balance(XOP, Revenues)  // !!!

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

  def quoteLeg[CCY: Monetary](market: Market)(leg: Leg): Money[MonetaryAmount, CCY] =
    leg match {
      case (security, quantity) => Market.quote[CCY](market)(security) * quantity
    }

  def quote[CCY: Monetary](market: Market)(trade: Trade): Money[MonetaryAmount, CCY] =
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

  def trialBalance = ???

  def recorded[IO[_]: Monad, CCY: Monetary](fs: Folios, accounts: Accounts.Table): Execution => IO[Folios] = {
    case (oid, _, trade, _, _) =>
      (Orders get oid) match {
        case Some((aid, _, _, _, _, _)) =>
          accounts(aid) match {
            case Account(roster, vault) =>
              // TODO: check roster permissions
              roster |> discardValue
              vault match {
                case Vault.Folio(folioId) =>
                  Monad[IO] pure { fs.updated(folioId, (fs get folioId).fold(Folio.empty)(_ |+| trade)) }
                case Vault.SubAccounts(subs) =>
                  subs |> discardValue
                  ??? // wut
              }
          }
        case _ => error
      }
  }

  def ordered = ???
  sealed trait Ordered

  def executed = ???
  sealed trait Executed

  sealed trait Settled
  def settled = ???

  def error = ???

}
