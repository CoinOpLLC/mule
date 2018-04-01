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

import io.deftrade.money.{ Financial, Monetary }, Monetary._

import cats.{ Eq, Monoid }
import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
import refined.numeric._
// import refined.auto._

import enumeratum._
// import enumeratum.values._

/**
  * This shall be the law of the Api: A `type Foo` may not depend on a `type FooId`.
  * This shall be another: only member names whose appearence cannot be helped may appear here.
  */
abstract class Api[MonetaryAmount: Financial, Quantity: Financial] {

  import impl._

  sealed trait Role extends EnumEntry
  object Role extends Enum[Role] {

    /**
      * The the entity(s) whose names are listed on the account.
      */
    case object Principle extends Role

    /**
      * Person(s) responsible for the disposition of assets in the Folio.
      */
    case object Manager extends Role

    /**
      * Semantics for `Subject` are conditioned on the status of account:
      * - responsible party (liability)
      * - beneficial owner (asset)
      */
    case object Subject extends Role

    /** The `findValues` macro collects all `value`s in the order written. */
    lazy val values: IndexedSeq[Role] = findValues
  }

  private[model] sealed trait Id[T] extends Any { def id: T }

  type Entity   = Nothing // Yet
  type EntityId = Long Refined Interval.Closed[W.`100100100100`, W.`999999999999`]
  // type EntityId = OpaqueId[Long, Entity]
  implicit def EntityIddHasEq = Eq.fromUniversalEquals[EntityId]

  type Asset   = Nothing // yet
  type AssetId = OpaqueId[Long, Asset]
  implicit def AssetIdHasEq = Eq.fromUniversalEquals[AssetId]

  type Position = (AssetId, Quantity)
  type Folio    = Map[AssetId, Quantity]
  type FolioId  = OpaqueId[Long, Folio]
  implicit def folioMonoid  = Monoid[Folio]
  implicit def FolioIdHasEq = Eq.fromUniversalEquals[FolioId]

  type Roster    = Map[Role, Set[EntityId]]
  type Account   = Map[FolioId, (Folio, Roster)]
  type AccountId = OpaqueId[Long, Account]
  implicit def AccountIdHasEq = Eq.fromUniversalEquals[AccountId]

// n.b the algebraic relationship between Position and Folio

  type Ledger = Map[AccountId, Account]

  type EntityRoles  = Map[EntityId, Map[AccountId, Set[Role]]]
  type AccountRoles = Map[AccountId, Map[EntityId, Set[Role]]]

  type Allocation = Map[FolioId, Quantity] // must sum to 1... TODO typesafe normalization?
  type Order      = (AccountId, Allocation, Folio)

  type Execution = Nothing // Yet

  type Transaction = Folio
  // `Transaction` := `Folio` in motion
  // `Folio` := `Transaction` at rest
  // both are just sets of Positions, really

  def index[K, V](kvs: Traversable[(K, V)]): Map[K, Traversable[V]] =
    kvs groupBy (_._1) map {
      case (k, kvs) => (k, kvs map (_._2))
    }

  def accumulate[K, V: Monoid](kvs: List[(K, V)]): Map[K, V] =
    for {
      (k, kvs) <- kvs groupBy (_._1)
    } yield (k, kvs foldMap (_._2))

  object Allocation {

    lazy val Q = Financial[Quantity]
    import Q._

    def pariPassu(fids: Set[FolioId]): Allocation = {
      val n = fids.size
      (fids.toList zip List.fill(n)(Quantity.One / (Q fromInt n))).toMap
    }
    def proRata(totalShares: Long)(capTable: Map[FolioId, Long]): Map[FolioId, Quantity] =
      capTable map {
        case (folioId, shares) =>
          val mySlice  = Q fromLong shares
          val wholePie = Q fromLong totalShares
          (folioId, mySlice / wholePie)
      }

    object Single {
      def apply(folioId: FolioId): Allocation = Map(folioId -> Quantity.One)
      def unapply(a: Allocation): Option[FolioId] = a.toList match {
        case (folioId, Quantity.One) :: Nil => Some(folioId)
        case _                              => None
      }
    }
  }

  // TODO: `Folio`, `Account` objects with apply / unapply methods

  type OrderId        = OpaqueId[Long, Order]
  type QuotedOrder[C] = (Transaction, Money[MonetaryAmount, C])

  def record(o: Order, ledger: Ledger): Ledger = o match {
    case (accountId, Allocation.Single(folioId), transaction) =>
      ledger(accountId)(folioId) match {
        case (folio, roster) =>
          val updated = ((folio |+| transaction), roster)
          ledger + (accountId -> Map(folioId -> updated)) // FIXME: + vs |+|...
      }
    case _ => ??? // FIXME
  }

  // TODO: this feels like a repository
  def price[C: Monetary](id: AssetId): Money[MonetaryAmount, C] = ???

  def price[C: Monetary](position: Position): Money[MonetaryAmount, C] = position match {
    case (asset, quantity) => price[C](asset) * quantity
  }

  implicit def qoMonoid[C: Monetary] = Monoid[QuotedOrder[C]]

  /** Holds constants (stable values) useful for pattern matching */
  object Quantity {
    val Zero: Quantity = Financial[Quantity].zero
    val One: Quantity  = Financial[Quantity].one
  }

  object Order {
    import io.deftrade.money.Monetary.USD
    def legacy(bd: BigDecimal, q: Long): QuotedOrder[USD] =
      (
        Map(LongId.reserved -> (Financial[Quantity] fromBigDecimal BigDecimal(q))),
        USD(Financial[MonetaryAmount] fromBigDecimal bd)
      )
  }

}

object impl {

  final case class OpaqueId[T, P](val id: T) extends AnyVal with Id[T]

  object OpaqueId {
    implicit def eq[T: Eq, P]: Eq[OpaqueId[T, P]] = Eq by (_.id)
  }

  object LongId {
    def reserved[P] = OpaqueId[Long, P](Long.MinValue)
  }

  object IntId {
    def reserved[P] = OpaqueId[Int, P](Int.MinValue)
  }

}
