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

import cats.{ Eq, Monoid, Order => CatsOrder }
import cats.implicits._
// import enumeratum._
// import enumeratum.values._

import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
import refined.numeric._
import refined.auto._

// import squants.{ Each, market => sm }
// import sm.Money

import io.deftrade.wip.{ Financial, PhantomTypePerCurrency }, PhantomTypePerCurrency.Monetary, Monetary._
trait Api {

  type ClientId
  final implicit def ClientIdHasEq: Eq[ClientId] = implicitly

  type AccountId
  final implicit def AccountIdHasEq: Eq[AccountId] = implicitly

  type AssetId
  final implicit def AssetIdHasEq: Eq[AssetId] = implicitly

  type Quantity
  implicit def QuantityIsFinancial: Financial[Quantity]

  final type Position = (AssetId, Quantity)

  type MonetaryAmount
  implicit def MonetaryAmountIsFinancial: Financial[MonetaryAmount]

  // this smells like a repository
  def price[C: Monetary](id: AssetId): Money[MonetaryAmount, C] = ???

  def price[C: Monetary](pos: Position): Money[MonetaryAmount, C] = pos match {
    case (asset, quantity) =>
      // FIXME this is the true expression of what I was thinking. Not sure it's what we want.
      val q = MonetaryAmountIsFinancial fromBigDecimal (QuantityIsFinancial toBigDecimal quantity)
      val p = price[C](asset)
      p * q
  }

  final type Folio   = AssetId Map Position
  final type Account = AccountId Map Folio
  final implicit lazy val accountMonoid: Monoid[Account] = implicitly

  type AccountRole
  final type Client = Map[ClientId, Map[AccountRole, AccountId]]

  type Remark = String
}

object Api extends Api {

  type ClientId = Long Refined Interval.Closed[W.`100000`, W.`100099`]

  object ClientId {
    implicit lazy val eq = Eq.fromUniversalEquals[ClientId]
  }
  type AccountId = Long
  type AssetId   = Long

  type Quantity = Double
  override implicit lazy val QuantityIsFinancial: Financial[Quantity] =
    Financial.DoubleIsFinancial

  type MonetaryAmount = BigDecimal
  override implicit lazy val MonetaryAmountIsFinancial: Financial[MonetaryAmount] =
    Financial.BigDecimalIsFinancial

  type AccountRole = String // or an enum!

  case class Order(
      account: AccountId,
      asset: AssetId,
      quantity: Quantity,
      totalCost: MonetaryAmount,
      remark: Option[Remark]
  )
  object Order {

    type Tpl = (AccountId, AssetId, Quantity, MonetaryAmount, Option[Remark])

    def legacy(bd: BigDecimal, q: Long): Order =
      apply(
        -1,
        -1,
        (QuantityIsFinancial fromBigDecimal BigDecimal(q)),
        MonetaryAmountIsFinancial fromBigDecimal bd,
        None
      )

    val tuple: Order => Tpl   = Order.unapply(_).fold(???)(identity) // honest, at least...
    val untuple: Tpl => Order = tpl => tpl |> (Order.apply(_, _, _, _, _)).tupled

    // implicit val eq       = Eq.fromUniversalEquals[Order]
    implicit val ordering = CatsOrder by tuple

    implicit val monoid = new Monoid[Order] {

      def combine(a: Order, b: Order): Order = tuple(a) |+| tuple(b) |> untuple

      lazy val empty: Order = Monoid[Tpl].empty |> untuple
    }
  }
}
