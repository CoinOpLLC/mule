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

import cats.{ Monad, Monoid }
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

  type Position = (InstrumentId, Quantity)
  object Position

  type Leg = Position // `Leg` := `Position` in motion
  val Leg = Position

  type Folio = Map[InstrumentId, Quantity] // n.b algebraic relationship Position <=> Folio
  object Folio {
    def empty: Folio                = Map.empty
    def apply(ps: Position*): Folio = accumulate(ps.toList)
  }

  type Trade = Folio // `Trade` := `Folio` in motion
  val Trade = Folio // gives you `apply`: Trade(legs: Leg*): Trade

  type FolioId = OpaqueId[Long, Folio]
  object FolioId extends OpaqueIdC[FolioId]

  // Need to express `Foldable` relation between `Position` and `Folio`
  // (also, and indentically, between `Leg` and `Trade`)
  //

  // FIXME: if we use instances of this type a parameters for recording trades, we're doing double entry! ;)

  // need some kind of map from folio (account) Roles (AR, Inventory, Revenue, JoeThePlumber, Cash) to concrete FolioIds

  case class LedgerKey(debit: FolioId, credit: FolioId)

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

  type Ledger = Folios
  val Ledger = Folios

  trait Person
  trait Corporation

  type Entity = Either[Corporation, Person]
  object Entity

  type EntityId = OpaqueId[Long, Entity]
  object EntityId extends OpaqueIdC[EntityId]

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

  /**
    * Note that Roster encodes the notion of a `CapitalKey` – per `Role`!
    * The meaning of this `Partition` will depend upon the role.
    * For the beneficial owner(s) of the assets in question the meaning is obvious.
    * For a `Manager`, the `Partition` could encode their voting rights – which may differ from
    * their ownership rights.
    */
  type Partition[K] = Map[K, Quantity]
  object Partition {
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

  type Roster = Map[Role, Set[EntityId]]
  object Roster {
    def apply(eid: EntityId): Roster =
      Map.empty + (Role.Principle -> Set(eid))
  }

  case class Account(folioId: FolioId, roster: Roster, subs: Partition[AccountId])

  type AccountId = Long Refined Interval.Closed[W.`100000100100`, W.`999999999999`]
  object AccountId {}

  type Accounts = Map[AccountId, Account]

  def recorded[C: Monetary](accounts: Accounts)(fs: Folios, ex: Execution[C]): Folios =
    ex match {
      case Execution(oid, _, (trade, _)) =>
        (Orders getOpen oid) match {
          case Some((aid, _, _)) =>
            accounts(aid) match {
              case Account(folioId, roster, _) =>
                // TODO: check roster permissions
                roster |> discardValue
                fs.updated(folioId, (fs get folioId).fold(Folio.empty)(_ |+| trade))
            }
          case _ => error
        }
    }

  def error = ???

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

  type Allocation = Partition[AccountId]
  object Allocation {

    def allocate[C](allocation: Allocation)(ex: Execution[C]): List[Execution[C]] = ???

  }

  // TODO: type constructors all the way?
  // how to capture the fact that `[CCY: Monetary]`
  case class Execution[CCY: Monetary](oid: OrderId, ldt: LocalDateTime, pt: PricedTrade[CCY])
  // object Execution

  type ExecutionId[CCY] = OpaqueId[Long, Execution[CCY]]
  class ExecutionIdC[CCY] extends OpaqueIdC[ExecutionId[CCY]]
  // TODO this requires completion and refactoring.
  object ExecutionIdCusd extends ExecutionIdC[Monetary.USD]
  object ExecutionIdCeur extends ExecutionIdC[Monetary.EUR]

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

  def quoteLeg[CCY: Monetary](market: Market)(leg: Leg): Money[MonetaryAmount, CCY] =
    leg match {
      case (security, quantity) => Market.quote[CCY](market)(security) * quantity
    }

  def quote[CCY: Monetary](market: Market)(trade: Trade): Money[MonetaryAmount, CCY] =
    trade.toList foldMap quoteLeg[CCY](market)

  def index[K, V](kvs: Traversable[(K, V)]): Map[K, Traversable[V]] =
    kvs groupBy (_._1) map {
      case (k, kvs) => (k, kvs map (_._2))
    }

  /** N.B. that `accumulate` requires a List parameter; `index` does not. */
  def accumulate[K, V: Monoid](kvs: List[(K, V)]): Map[K, V] =
    for {
      (k, kvs) <- kvs groupBy (_._1)
    } yield (k, kvs foldMap (_._2))

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

  /**
    * Repositories.
    * FIXME: this is hacked in to compile for now.
    */
  object Orders {
    def get(oid: OrderId): Option[Order]     = ???
    def getOpen(oid: OrderId): Option[Order] = ???
    def all: Stream[Order]                   = ???
  }

  object Executions {}

  object Accounts {}

  object Folios {
    def empty: Folios = Map.empty
    // def apply(folioId: FolioId): Folio       = get(folioId).fold(Folio.empty)(identity)
    // def get(folioId: FolioId): Option[Folio] = ???
  }
  object Markets {
    def quotedIn[C: Monetary](m: Market)(id: InstrumentId): InstrumentId QuotedIn C = ???

    // idea: use phantom types to capture sequencing constraint?
    def trade[F[_]: Monad, CCY: Monetary](m: Market): PricedTrade[CCY] => F[Seq[Execution[CCY]]] =
      ???
  }

}
