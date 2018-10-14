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

import opaqueid.{ OpaqueId, OpaqueIdC }
import time._
import money.{ Financial, Monetary, QuotedIn }, Monetary.{ Monetary, Money }

import cats.{ Foldable, Monad, Monoid }
import cats.implicits._
import cats.kernel.CommutativeGroup
import feralcats.instances._

import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
import refined.numeric._
// import refined.auto._

// import io.circe.Json

import scala.language.higherKinds

/**
  * This shall be the law of the Api: A `type Foo` may not depend on a `type FooId`.
  * This shall be another: only member names whose appearence cannot be helped may appear here.
  */
abstract class Api[MonetaryAmount: Financial, Quantity: Financial] {

  /** Domain specific tools for dealing with `Quantity`s */
  val Quantity = Financial[Quantity]

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  val MonetaryAmount = Financial[MonetaryAmount]

  type UII = enums.UniversalInstrumentIdentifyer
  val UII = enums.UniversalInstrumentIdentifyer

  // TODO: use the XBRL definitions for these, a la OpenGamma
  type Instrument = enums.Instrument
  val Instrument = enums.Instrument

  type InstrumentId = OpaqueId[Long, Instrument]
  object InstrumentId extends OpaqueIdC[InstrumentId]

  type Instruments = Map[InstrumentId, Instrument]

  type Position = (InstrumentId, Quantity)
  object Position

  type Folio = Map[InstrumentId, Quantity] // n.b algebraic relationship Position <=> Folio
  object Folio {
    def empty: Folio                = Map.empty
    def apply(ps: Position*): Folio = accumulate(ps.toList)
  }

  type FolioId = OpaqueId[Long, Folio]
  object FolioId extends OpaqueIdC[FolioId]

  // Need to express `Foldable` relation between `Position` and `Folio`
  // (also, and indentically, between `Leg` and `Trade`)
  //

  // FIXME: if we use instances of this type a parameters for recording trades, we're doing double entry! ;)

  // need some kind of map from folio (account) Roles (AR, Inventory, Revenue, JoeThePlumber, Cash) to concrete FolioIds

  case class LedgerKey(debit: FolioId, credit: FolioId)

  case class AccountTypeKey(debit: enums.AccountType, credit: enums.AccountType)
  object AccountTypeKey {
    import enums.{ Asset, Liability }
    lazy val PurchaseInstrument = AccountTypeKey(Asset.OtherInvestments, Asset.Cash)
    lazy val PayBills           = AccountTypeKey(Liability.AccountsPayable, Asset.Cash)
    // etc.
  }

  /**
    * (Runtime) invariant: `Trade`s must balance across all the `FolioId`s in the
    * `LedgerEntry`.
    * FIXME: where do the dates enter in?
    * FIXME: recording valuations of priced trades
    */
  type JournalEntry = Map[FolioId, Trade]
  object JournalEntry

  type Transaction = JournalEntry
  val Transaction = JournalEntry

  /**
    * FIXME: What is the `Ledger`, anyway?
    It's a `Log` structure, essentially – `Foldable[JournalEntry]`
    */
  type Journal = List[JournalEntry]
  object Journal

  type Folios = Map[FolioId, Folio] // = LedgerEntry

  trait Person
  trait Corporation

  type Entity = Either[Corporation, Person]
  object Entity

  type EntityId = OpaqueId[Long, Entity]
  object EntityId extends OpaqueIdC[EntityId]

  type Entities = Map[EntityId, Entity]

  /**
    * Note that Roster encodes the notion of a `CapitalKey` – per `Role`!
    * The meaning of this `Partition` will depend upon the role.
    * For the beneficial owner(s) of the assets in question the meaning is obvious.
    * For a `Manager`, the `Partition` could encode their voting rights – which may differ from
    * their ownership rights.
    */
  type Partition[K] = Map[K, Quantity]
  object Partition {

    /**
      * The portion of the total accorded to the identified `Entity`.
      */
    type Share[K] = (K, Quantity)
    object Share {
      import Quantity._
      def validate[K](p: Share[K]): Boolean = {
        val (_, q) = p
        zero <= q && q <= one
      }
      def apply[K](k: K, q: Quantity): Share[K] = (k, q) ensuring { p =>
        validate(p)
      }
      def single[K](k: K) = Share(k, one)
    }

    import Quantity._
    def apply[K](shares: Share[K]*): Either[String, Partition[K]] = {
      val p = accumulate(shares.toList)
      p.values.toList.sum match {
        case sum if sum == one => p.asRight
        case badsum            => s"Partition doesn't add up: $badsum != 1.0".asLeft
      }
    }
    def single[K](k: K): Partition[K] = Map.empty + Share.single[K](k)

    def proRata[K](totalShares: Quantity)(capTable: Map[K, Quantity]): Partition[K] =
      capTable map { case (k, mySlice) => (k, mySlice / totalShares) }

    def pariPassu[K](ks: Set[K]): Partition[K] = {
      val n = ks.size
      (ks.toList zip List.fill(n)(one / fromInt(n))).toMap
    }

    object Single {
      def unapply[K](p: Partition[K]): Option[K] = p.toList match {
        case (k, One) :: Nil => k.some
        case _               => none
      }
    }
  }

  type Role = enums.Role
  val Role = enums.Role

  /**
    * justification: can't know / can't be responsible for Partition on the entities (i.e. is
  it 50-50? 98-1-1? etc...). This is especially true for non-principle roles like `Regulator`.
    */
  type Roster = Map[Role, Set[EntityId]]
  object Roster {
    def apply(eid: EntityId): Roster =
      Map.empty + (Role.Principle -> Set(eid))
  }

  sealed trait Vault
  object Vault {

    case class SubAccounts(subs: Set[AccountId]) extends Vault
    case class Folio(fid: FolioId)               extends Vault

    def empty: Vault = SubAccounts(Set.empty)
  }

  case class Account(roster: Roster, vault: Vault)
  object Account {
    def simple(fid: FolioId, eid: EntityId): Account =
      Account(Roster(eid), Vault.empty)
  }

  type AccountId = Long Refined Interval.Closed[W.`100000100100`, W.`999999999999`]
  object AccountId {}

  type Accounts = Map[AccountId, Account]

  /**
    * aliases.
    */
  type Ledger = Folios
  val Ledger = Folios

  type Leg = Position // `Leg` := `Position` in motion
  val Leg = Position

  type Trade = Folio // `Trade` := `Folio` in motion
  val Trade = Folio // gives you `apply`: Trade(legs: Leg*): Trade

  type Order = (AccountId, EntityId, Trade)
  // type Order[MA, CCY] = (AccountId, EntityId, OffsetDateTime, Trade, Option[Money[MA, CCY]])
  // implied buy / sell, limit / market
  // FIXME: need a date time field
  // FIXME: need a price field (Option) ()
  object Order {
    import io.deftrade.money.Monetary.USD
    def legacy(bd: BigDecimal, q: Long): PricedTrade[USD] =
      (
        Map(opaqueid.LongId.reserved -> (Financial[Quantity] fromBigDecimal BigDecimal(q))),
        USD(Financial[MonetaryAmount] fromBigDecimal bd)
      )
  }

  type OrderId = OpaqueId[Long, Order]
  object OrderId extends OpaqueIdC[OrderId]

  type Orders = Map[OrderId, Order]

  type Allocation = Partition[AccountId]
  object Allocation {

    def allocate[C](allocation: Allocation)(ex: Execution[C]): List[Execution[C]] = ???

  }

  // FIXME: need to "back up" CCY parameterization thru `Order`
  // FIXME: need to generalize `BalanceSheet` for any T account

  // TODO: type constructors all the way?
  // how to capture the fact that `[CCY: Monetary]`
  case class Execution[CCY: Monetary](oid: OrderId, ldt: LocalDateTime, pt: PricedTrade[CCY])
  // object Execution

  type ExecutionId[CCY] = OpaqueId[Long, Execution[CCY]]
  class ExecutionIdC[CCY] extends OpaqueIdC[ExecutionId[CCY]]
  // TODO this requires completion and refactoring.
  object ExecutionIdCusd extends ExecutionIdC[Monetary.USD]
  object ExecutionIdCeur extends ExecutionIdC[Monetary.EUR]

  type Executions[CCY] = Map[ExecutionId[CCY], Execution[CCY]]

  type PricedTrade[CCY] = (Trade, Money[MonetaryAmount, CCY])
  object PricedTrade

  sealed trait Exchange // a multiparty nexus
  object Exchange

  sealed trait Counter // a single counterparty or market maker
  object Counter

  type Market = Either[Counter, Exchange]
  object Market {

    /** Price all the things. */
    def quote[C: Monetary](m: Market)(id: InstrumentId): Money[MonetaryAmount, C] =
      Monetary[C] apply (Financial[MonetaryAmount] from Markets.quotedIn(m)(id).mid)
  }

  type MarketId = OpaqueId[Long, Market]
  object MarketId extends OpaqueIdC[MarketId]

  type Markets = Map[MarketId, Market]

  /** `BalanceSheet` forms a commutative group */
  case class BalanceSheet private (val assets: BalanceSheet.Assets, val liabilities: BalanceSheet.Liabilities) {

    /** IRS Form 1065 Schedule L ontology */
    import BalanceSheet._

    def updated(asset: Asset, liability: Liability)(amt: MonetaryAmount): BalanceSheet =
      BalanceSheet(assets + (asset -> amt), liabilities + (liability -> amt))

    def updated(from: Asset, to: Asset)(amt: MonetaryAmount): BalanceSheet =
      copy(assets = assets + (from -> amt.inverse) + (to -> amt))

    def updated(from: Liability, to: Liability)(amt: MonetaryAmount): BalanceSheet =
      copy(liabilities = liabilities + (from -> amt.inverse) + (to -> amt))

  }
  object BalanceSheet {

    type Asset = enums.Asset
    val Asset = enums.Asset

    type Liability = enums.Liability
    val Liability = enums.Liability

    type Assets      = Map[Asset, MonetaryAmount]
    type Liabilities = Map[Liability, MonetaryAmount]

    implicit lazy val balanceSheetCommutativeGroup: CommutativeGroup[BalanceSheet] =
      new CommutativeGroup[BalanceSheet] {

        override def empty = BalanceSheet(Map.empty, Map.empty)

        override def combine(a: BalanceSheet, b: BalanceSheet) =
          BalanceSheet(a.assets combine b.assets, a.liabilities combine b.liabilities)

        override def inverse(a: BalanceSheet) = BalanceSheet(a.assets.inverse, a.liabilities.inverse)
      }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  //  repos lol
  // FIXME: this is hacked in to compile for now.
  //////////////////////////////////////////////////////////////////////////////////////////////////

  object Markets {
    def quotedIn[C: Monetary](m: Market)(id: InstrumentId): InstrumentId QuotedIn C = ???

    // idea: use phantom types to capture sequencing constraint?
    def trade[F[_]: Monad, CCY: Monetary](m: Market): PricedTrade[CCY] => F[Seq[Execution[CCY]]] =
      ???
  }

  object Instruments {
    def all: Instruments = ???
  }

  object Folios {

    def empty: Folios = Map.empty
    def all: Folios   = ???

    def get(folioId: FolioId): Option[Folio] = ???
    def apply(folioId: FolioId): Folio       = get(folioId).fold(Folio.empty)(identity)
  }

  object Entities {
    def all: Entities = ???
  }

  object Accounts {
    def empty: Accounts = Map.empty
    def all: Accounts   = ???

    def get(accountId: AccountId): Option[Account] = ???
  }

  object Orders {
    def empty: Orders                    = Map.empty
    def all: Orders                      = ???
    def get(oid: OrderId): Option[Order] = all get oid

    def open: Orders = ???
  }

  import money.Monetary.USD

  object Executions extends SimpleRepository[cats.Id, Execution[USD], ExecutionId[USD]]

  abstract class Repository[IO[_]: Monad, A, I] {

    lazy val IO = Monad[IO]

    type Id    = OpaqueId[I, A]
    type Rows  = List[(Id, A)]
    type Table = Map[Id, A]
    object Table { def empty: Table = Map.empty }

    /** Simple Queries */
    final def empty: IO[Table] = IO pure { Table.empty }
    def rows: IO[Rows]
    def table: IO[Table]
    def get(id: Id): IO[Option[A]]

    /** mutators record timestamp */
    def upsert(row: (Id, A)): IO[Result[Unit]]
    def upsert(table: Table): IO[Result[Unit]] = (IO pure table) flatMap { upsert(_) }
  }

  trait RepositoryImpl[IO[_], A, I] { self: Repository[IO, A, I] =>
    def rows: IO[Rows]             = ???
    def table: IO[Table]           = ???
    def get(id: Id): IO[Option[A]] = ???

    /** mutators record timestamp */
    def upsert(row: (Id, A)): IO[Result[Unit]] = ???

  }

  class SimpleRepository[IO[_]: Monad, A, I] extends Repository[IO, A, I] with RepositoryImpl[IO, A, I]

  abstract class PointInTimeRepository[IO[_]: Monad, A, I] extends Repository[IO, A, I] {

    type LocalDateTimeRange
    object LocalDateTimeRange { def allOfTime: LocalDateTimeRange = ??? }

    /** Simple Queries */
    final def rows: IO[Rows]             = rowsBetween(LocalDateTimeRange.allOfTime)
    final def table: IO[Table]           = tableAt(localDateTime)
    final def get(id: Id): IO[Option[A]] = getAt(localDateTime)(id)

    /** Point In Time Queries */
    def tableAt(pit: LocalDateTime): IO[Table]
    def rowsBetween(range: LocalDateTimeRange): IO[Rows]
    def getAt(pit: LocalDateTime)(id: Id): IO[Option[A]]
    def getBetween(range: LocalDateTimeRange)(id: Id): IO[Rows]

    /** mutators record timestamp */
    def upsertAt(pit: LocalDateTime)(row: (Id, A)): IO[Result[Unit]]
    final def upsert(row: (Id, A)): IO[Result[Unit]] = upsertAt(localDateTime)(row)

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

  def recorded[CCY: Monetary](fs: Folios, accounts: Accounts): Execution[CCY] => Folios = {
    case Execution(oid, _, (trade, _)) =>
      (Orders.open get oid) match {
        case Some((aid, _, _)) =>
          accounts(aid) match {
            case Account(roster, vault) =>
              // TODO: check roster permissions
              roster |> discardValue
              vault match {
                case Vault.Folio(folioId) =>
                  fs.updated(folioId, (fs get folioId).fold(Folio.empty)(_ |+| trade))
                case Vault.SubAccounts(subs) =>
                  subs |> discardValue
                  ??? // wut
              }
          }
        case _ => error
      }
  }

  def error = ???

}
