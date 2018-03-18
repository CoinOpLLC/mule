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

import io.deftrade.wip.{ Financial, PhantomTypePerCurrency }, PhantomTypePerCurrency.Monetary, Monetary._
trait Api {

  type ClientId
  implicit def ClientIdHasEq: Eq[ClientId]

  type AccountId
  implicit def AccountIdHasEq: Eq[AccountId]

  type AssetId
  implicit def AssetIdHasEq: Eq[AssetId]

  type Quantity
  def QuantityIsFinancial: Financial[Quantity]

  final type Position = AssetId => Quantity

  final type Positions = Map[AssetId, Quantity]

  type MonetaryAmount
  implicit def MonetaryAmountIsFinancial: Financial[MonetaryAmount]

  // this smells like a repository
  def price[C: Monetary](id: AssetId): Money[MonetaryAmount, C] = ???

  def price[C: Monetary](position: Position): AssetId => Money[MonetaryAmount, C] =
    asset => {
      // FIXME this is the true expression of what I was thinking. Not sure it's what we want.
      val q = MonetaryAmountIsFinancial fromBigDecimal (
        QuantityIsFinancial toBigDecimal position(asset)
      )
      val p = price[C](asset)
      p * q
    }

  final type Folio   = Map[AssetId, Position]
  final type Account = Map[AccountId, Folio]
  implicit def accountMonoid: Monoid[Account]

  type AccountRole
  final type Client = Map[ClientId, Map[AccountRole, AccountId]]
}

object Api extends Api {

  type ClientId = Long Refined Interval.Closed[W.`100000`, W.`100099`]

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

  override implicit lazy val accountMonoid = Monoid[Account]

  // reader, writer, state,

  final case class OrderId(val id: Long) extends AnyVal

  type Order          = OrderId => Map[AccountId, Positions]
  type QuotedOrder[C] = (Order, Money[MonetaryAmount, C])
  implicit def qoMonoid[C: Monetary] = Monoid[QuotedOrder[C]]
  implicit def qoEq[C: Monetary]     = Eq.fromUniversalEquals[QuotedOrder[C]]

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
