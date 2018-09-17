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

import io.deftrade.money.{ Financial, Monetary, QuotedIn }, Monetary.{ Monetary, Money }
// import io.deftrade.money._, Monetary._
import io.deftrade.time._

import cats.{ Eq, Monad, Monoid }
import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
import refined.numeric._
// import refined.auto._

// import io.circe.Json

import scala.language.higherKinds

import opaqueid.{ OpaqueId, OpaqueIdC }

/**
  * This shall be the law of the Api: A `type Foo` may not depend on a `type FooId`.
  * This shall be another: only member names whose appearence cannot be helped may appear here.
  */
abstract class Api[MonetaryAmount: Financial, Quantity: Financial] {

  /** Domain specific tools for dealing with `Quantity`s */
  val Quantity = Financial[Quantity]

  /** Domain specific tools for dealing with `MonetaryAmount`s */
  val MonetaryAmount = Financial[MonetaryAmount]

  type USI = enums.UniversalSecurityIdentifyer
  val USI = enums.UniversalSecurityIdentifyer

  // TODO: use the XBRL definitions for these, a la OpenGamma
  type Security = enums.Security
  val Security = enums.Security

  type SecurityId = OpaqueId[Long, Security]
  object SecurityId extends OpaqueIdC[SecurityId]

  type Position = (SecurityId, Quantity)
  object Position

  type Leg = Position // `Leg` := `Position` in motion
  val Leg = Position

  type Folio = Map[SecurityId, Quantity] // n.b algebraic relationship Position <=> Folio
  object Folio {
    def apply(ps: Position*): Folio = accumulate(ps.toList)
  }

  type Trade = Folio // `Trade` := `Folio` in motion
  val Trade = Folio // gives you `apply`: Trade(legs: Leg*): Trade

  type FolioId = OpaqueId[Long, Folio]
  object FolioId extends OpaqueIdC[FolioId]

  /**
    * (Runtime) invariant: `Trade`s must balance across all the `FolioId`s in the
    * `LedgerEntry`.
    */
  type LedgerEntry = Map[FolioId, Trade]
  object LedgerEntry

  type Transaction = LedgerEntry
  val Transaction = LedgerEntry

  /**
    * What is the `Ledger`, anyway? It's a `Log` structure, essentially – Foldable[LedgerEntry]
    */
  type Ledger = List[LedgerEntry] // if you must - but reserving the name is a good idea regardless
  object Ledger

  /** Nota Bene: looks like LedgerState and LedgerEntry are the same thing (and form a monoid) */
  type LedgerState = Map[FolioId, Folio]
  object LedgerState

  trait Person
  trait Corporation

  type Entity = Either[Corporation, Person]
  object Entity

  type EntityId = OpaqueId[Long, Entity]
  object EntityId extends OpaqueIdC[EntityId]

  /**
    * The portion of the total accorded to the identified `Entity`.
    */
  type Share = (EntityId, Quantity)
  object Share {
    import Quantity._
    def validate(p: Share): Boolean = {
      val (_, q) = p
      zero <= q && q <= one
    }
    def apply(eid: EntityId, q: Quantity): Share = (eid, q) ensuring { p =>
      validate(p)
    }
    def single(eid: EntityId) = Share(eid, one)
  }

  /**
    * Note that Roster encodes the notion of a `CapitalKey` – per `Role`!
    * The meaning of this `Partition` will depend upon the role.
    * For the beneficial owner(s) of the assets in question the meaning is obvious.
    * For a `Manager`, the `Partition` could encode their voting rights – which may differ from
    * their ownership rights.
    */
  type Partition = Map[EntityId, Quantity]
  object Partition {
    def apply(shares: Share*): Either[String, Partition] = {
      val p = accumulate(shares.toList)
      p.values.toList.sum match {
        case sum if sum == Quantity.one => p.asRight
        case badsum                     => s"Partition doesn't add up: $badsum != 1.0".asLeft
      }
    }
    def single(eid: EntityId): Partition = Map.empty + Share.single(eid)
  }

  type Role = enums.Role
  val Role = enums.Role

  type Roster = Map[Role, Partition]
  object Roster {
    def apply(eid: EntityId): Roster =
      Map.empty + (Role.Principle -> Partition.single(eid))
  }

  type Account = Map[FolioId, Roster]
  object Account {
    def updatedProRata(a: Account, newProRata: Map[EntityId, Quantity]): Account = ???
  }

  type AccountId = Long Refined Interval.Closed[W.`100000100100`, W.`999999999999`]
  object AccountId

  type LedgerKey = Map[AccountId, Account]
  object LedgerKey {

    def recorded(lk: LedgerKey)(ls: LedgerState)(ao: AllocatedOrder): LedgerState = ao match {
      case (_, transaction) =>
        transaction.toList foldMap {
          case (folioId, trade) =>
            (ls get folioId).fold(
              ls + (folioId -> trade)
            ) { folio =>
              ls + (folioId -> (folio |+| trade))
            }
        }
    }
  }

  type LedgerId = OpaqueId[Long, LedgerKey]
  object LedgerId extends OpaqueIdC[LedgerId]

  type Allocation = Map[FolioId, Quantity] // must sum to 1... TODO typesafe normalization?
  object Allocation {

    import Quantity._

    def allocate(allocation: Allocation)(order: Order): AllocatedOrder = ???

    def pariPassu(fids: Set[FolioId]): Allocation = {
      val n = fids.size
      (fids.toList zip List.fill(n)(one / fromInt(n))).toMap
    }
    def proRata(totalShares: Long)(capTable: Map[FolioId, Long]): Allocation =
      capTable map {
        case (folioId, shares) =>
          val mySlice  = fromLong(shares)
          val wholePie = fromLong(totalShares)
          (folioId, mySlice / wholePie)
      }

    object Single {
      def apply(folioId: FolioId): Allocation = Map(folioId -> one)
      def unapply(a: Allocation) = a.toList match {
        case (folioId, One) :: Nil => folioId.some
        case _                     => none
      }
    }
  }

  type Order = (AccountId, EntityId, Trade)
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

  type AllocatedOrder = (AccountId, Transaction)
  object AllocatedOrder

  // TODO: type constructors all the way?
  // how to capture the fact that `[CCY: Monetary]`
  type Execution[CCY] = (OrderId, LocalDateTime, PricedTrade[CCY])
  object Execution

  type ExecutionId[CCY] = OpaqueId[Long, Execution[CCY]]
  class ExecutionIdC[CCY] extends OpaqueIdC[ExecutionId[CCY]]
  // FIXME this requires custom fuzting

  type PricedTrade[CCY] = (Trade, Money[MonetaryAmount, CCY])
  object PricedTrade

  sealed trait Exchange // a multiparty nexus
  object Exchange

  sealed trait Counter // a single counterparty or market maker
  object Counter

  type Market = Either[Counter, Exchange]
  object Market {

    /** Price all the things. TODO: this feels like a repository */
    def quote[C: Monetary](m: Market)(id: SecurityId): Money[MonetaryAmount, C] =
      Monetary[C] apply (Financial[MonetaryAmount] from quotedIn(m)(id).mid)

    def quotedIn[C: Monetary](m: Market)(id: SecurityId): SecurityId QuotedIn C = ???

    // idea: use phantom types to capture sequencing constraint?
    def trade[F[_]: Monad, CCY: Monetary](m: Market): PricedTrade[CCY] => F[Seq[Execution[CCY]]] =
      ???
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

}
