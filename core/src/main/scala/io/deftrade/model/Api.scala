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
  */
trait Api {

  import impl._

  sealed trait Asset

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

  trait Id[T] extends Any { def id: T }

  type EntityId = Long Refined Interval.Closed[W.`100100100100`, W.`999999999999`]
  implicit def EntityIddHasEq = Eq.fromUniversalEquals[EntityId]

  type AccountId = OpaqueId[Long, Account]
  implicit def AccountIdHasEq = Eq.fromUniversalEquals[AccountId]

  type AssetId = OpaqueId[Long, Asset]
  implicit def AssetIdHasEq = Eq.fromUniversalEquals[AssetId]

  type Quantity = Double
  implicit lazy val QuantityIsFinancial: Financial[Quantity] =
    Financial.DoubleIsFinancial

  type MonetaryAmount = BigDecimal
  implicit lazy val MonetaryAmountIsFinancial: Financial[MonetaryAmount] =
    Financial.BigDecimalIsFinancial

// n.b the algebraic relationship between Position and Folio
  type Position = (AssetId, Quantity)
  type Folio    = Map[AssetId, Quantity]
  implicit def folioMonoid = Monoid[Folio]

  type FolioId = OpaqueId[Long, Folio]
  implicit def FolioIdHasEq = Eq.fromUniversalEquals[FolioId]

  type Roster  = Map[Role, Set[EntityId]]
  type Account = Map[FolioId, (Folio, Roster)]
  type Ledger  = Map[AccountId, Account]

  type EntityRoles  = Map[EntityId, Map[AccountId, Set[Role]]]
  type AccountRoles = Map[AccountId, Map[EntityId, Set[Role]]]

  type Allocation = Map[FolioId, Quantity] // must sum to 1... TODO typesafe normalization?
  type Order      = (AccountId, Allocation, Folio)

  type Execution = Nothing // Yet

  type Transaction = Folio
  // `Transaction` := `Folio` in motion
  // `Folio` := `Transaction` at rest
  // both are just sets of Positions, really

  // FIXME: this should be tidyFold or something. "Push" this hard.
  def rollUp(ps: List[Position]): Folio = tidy(ps) map { case (k, vs) => (k, vs.sum) }

  object SingleFolioAllocation {

    def apply(folioId: FolioId): Allocation = Map(folioId -> Quantity.One)

    def unapply(a: Allocation): Option[FolioId] = a.toList match {
      case (folioId, Quantity.One) :: Nil => Some(folioId)
      case _                              => None
    }
  }

  // FIXME: do also: SingleFolio, SingleAccount

  type OrderId        = OpaqueId[Long, Order]
  type QuotedOrder[C] = (Transaction, Money[MonetaryAmount, C])

  def combine(o: Order, ledger: Ledger): Ledger = o match {
    case (accountId, SingleFolioAllocation(folioId), transaction) =>
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
  // implicit def qoEq[C: Monetary]     = Eq.fromUniversalEquals[QuotedOrder[C]]

  lazy val Order = impl.Order

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

  object Quantity {
    val Zero: Quantity = QuantityIsFinancial.zero
    val One: Quantity  = QuantityIsFinancial.one
  }

  object Order {
    import io.deftrade.money.Monetary.USD
    def legacy(bd: BigDecimal, q: Long): QuotedOrder[USD] =
      (
        Map(LongId.reserved -> (QuantityIsFinancial fromBigDecimal BigDecimal(q))),
        USD(MonetaryAmountIsFinancial fromBigDecimal bd)
      )
  }

}
