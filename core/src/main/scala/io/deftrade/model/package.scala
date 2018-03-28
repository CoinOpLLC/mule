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

  type EntityId = Long Refined Interval.Closed[W.`100100100100`, W.`100000999999`]
  implicit def EntityIddHasEq = Eq.fromUniversalEquals[EntityId]

  type AccountId = GenericId[Long, Account]
  implicit def AccountIdHasEq = Eq.fromUniversalEquals[AccountId]

  type AssetId = GenericId[Long, Asset]
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

  type FolioId = GenericId[Long, Folio]
  implicit def FolioIdHasEq = Eq.fromUniversalEquals[FolioId]

  type Crew         = Map[Role, Set[EntityId]]
  type CrewedFolio  = (Crew, Folio)
  type BoundFolio   = (FolioId, CrewedFolio)
  type Account      = Map[FolioId, CrewedFolio]
  type BoundAccount = (AccountId, Account)
  type Firm         = Map[AccountId, Account]

  type EntityRoles  = Map[EntityId, Map[AccountId, Set[Role]]]
  type AccountRoles = Map[AccountId, Map[EntityId, Set[Role]]]

  type Order          = Folio
  type OrderId        = GenericId[Long, Order]
  type QuotedOrder[C] = (Order, Money[MonetaryAmount, C])

  def combine(o: Order, folio: Folio): Folio = folio |+| o

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
