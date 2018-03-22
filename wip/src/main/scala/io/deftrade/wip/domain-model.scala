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
package wip
package model

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

import io.deftrade.money.{ Financial, PhantomTypePerCurrency }, PhantomTypePerCurrency.Monetary, Monetary._
trait Api {

  type ClientId
  implicit def ClientIdHasEq: Eq[ClientId]

  type AccountId
  implicit def AccountIdHasEq: Eq[AccountId]

  type AssetId
  implicit def AssetIdHasEq: Eq[AssetId]

  type Quantity
  implicit def QuantityIsFinancial: Financial[Quantity]

  type MonetaryAmount
  implicit def MonetaryAmountIsFinancial: Financial[MonetaryAmount]

  final type Position = (AssetId, Quantity)

  // FIXME: this smells like a repository
  def price[C: Monetary](id: AssetId): Money[MonetaryAmount, C] = ???

  def price[C: Monetary](position: Position): Money[MonetaryAmount, C] = position match {
    case (asset, quantity) => price[C](asset) * quantity
  }

  // n.b the algebraic relationship between Position and Folio
  final type Folio = Map[AssetId, Quantity]
  final implicit def folioMonoid = Monoid[Folio]

  final type Account = Map[AccountId, Folio]
  final implicit def accountMonoid = Monoid[Account]

  type AccountRole
  final type Client = Map[ClientId, Map[AccountRole, AccountId]]
}

object Api extends Api {

  type ClientId = Long Refined Interval.Closed[W.`100000100000`, W.`100000999999`]
  implicit def ClientIdHasEq = Eq.fromUniversalEquals[ClientId]

  final case class AccountId(val id: Long) extends AnyVal
  object AccountId {
    def reserved = AccountId(-1L)
  }
  override implicit def AccountIdHasEq = Eq.fromUniversalEquals[AccountId]

  final case class AssetId(val id: Long) extends AnyVal
  object AssetId {
    def reserved = AssetId(-1L)
  }
  implicit def AssetIdHasEq = Eq.fromUniversalEquals[AssetId]

  type Quantity = Double
  override implicit lazy val QuantityIsFinancial: Financial[Quantity] =
    Financial.DoubleIsFinancial

  type MonetaryAmount = BigDecimal
  override implicit lazy val MonetaryAmountIsFinancial: Financial[MonetaryAmount] =
    Financial.BigDecimalIsFinancial

  type AccountRole = String // or an enum!

  // reader, writer, state monad transformer

  final case class OrderId(val id: Long) extends AnyVal

  type Order          = (OrderId, AccountId, Folio)
  type Orders         = Map[OrderId, Map[AccountId, Folio]]
  type QuotedOrder[C] = (Orders, Money[MonetaryAmount, C])

  def combine(o: Order, account: Account): Account = o match {
    case (_, accountId, folio) => account |+| Map(accountId -> folio)
  }

  implicit def qoMonoid[C: Monetary] = Monoid[QuotedOrder[C]]
  // implicit def qoEq[C: Monetary]     = Eq.fromUniversalEquals[QuotedOrder[C]]

  object Order {

    def legacy(bd: BigDecimal, q: Long): QuotedOrder[USD] =
      (
        Map(
          OrderId(42L) ->
            Map(
              AccountId.reserved ->
                Map(AssetId.reserved -> (QuantityIsFinancial fromBigDecimal BigDecimal(q)))
            )
        ),
        USD(MonetaryAmountIsFinancial fromBigDecimal bd)
      )
  }
}
