package io.deftrade

import cats.{ Eq, Monoid }
import cats.implicits._
// import enumeratum._
// import enumeratum.values._

import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
import refined.numeric._
// import refined.auto._

import io.deftrade.money.{ Financial, Monetary }, Monetary._

package object model extends model.Api {

  import impl._

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

  type Transaction = Folio // think about it

  object SimpleAllocation {
    def unapply(a: Allocation): Option[FolioId] = a.toList match {
      case (folioId, Quantity.One) :: Nil => Some(folioId)
      case _                              => ??? // FIXME
    }
  }

  type OrderId        = OpaqueId[Long, Order]
  type QuotedOrder[C] = (Transaction, Money[MonetaryAmount, C])

  def combine(o: Order, ledger: Ledger): Ledger = o match {
    case (accountId, SimpleAllocation(folioId), transaction) =>
      ledger(accountId)(folioId) match {
        case (folio, roster) =>
          val updated = ((folio |+| transaction), roster)
          ledger + (accountId -> Map(folioId -> updated))
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

  object Order {
    def legacy(bd: BigDecimal, q: Long): QuotedOrder[USD] =
      (
        Map(LongId.reserved -> (QuantityIsFinancial fromBigDecimal BigDecimal(q))),
        USD(MonetaryAmountIsFinancial fromBigDecimal bd)
      )
  }
}
