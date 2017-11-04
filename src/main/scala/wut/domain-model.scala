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

package wut
package model

import cats.{ Eq, Monoid }
import cats.syntax.all._
import cats.{ instances => ci }
import ci.int._, ci.string._, ci.double._, ci.boolean._, ci.bigDecimal._, ci.long._
import ci.option._, ci.tuple._, ci.list._, ci.set._, ci.map._

import enumeratum._
import enumeratum.values._

import eu.timepit.refined
import refined.api.Refined
import refined.W
import refined.collection._
import refined.numeric._
import refined.auto._

import squants.{ Each, market => sm }
import sm.Money

object api {

  type ItemId    = Long
  type AccountId = Long
  type Comment   = String

  implicit val moneyContext = sm.defaultMoneyContext

  val doubleEagle = sm.Money(20)

  type AssetId = Long Refined Interval.Closed[W.`100000`, W.`100099`]
  // type AssetId     = String
  type AccountRole = String
  type ClientId    = String
  // type Quantity = Long Refined NonNegative
  type Quantity = Double
  // type Price    = sm.Price[squants.Dimensionless]
  type Price = Double
  // price and quantity need `Monoids`.
  type Position = (Quantity, Price)
  type Folio    = Map[AssetId, Position]
  type Account  = Map[AccountId, Folio]
  type Client   = Map[ClientId, Map[AccountRole, AccountId]]

  implicit val accountMonoid = Monoid[Account]

}

import api._

final case class AnotherWayToDoTypesafeId(val value: Long) extends AnyVal

case class Order(
    account: AccountId,
    item: ItemId,
    quantity: Long,
    totalCost: BigDecimal,
    comment: Option[Comment]
)
object Order {

  type Tpl = (AccountId, ItemId, Long, BigDecimal, Option[Comment])

  def legacy(bd: BigDecimal, q: Long): Order = apply(-1, -1, q, bd, None)

  val tuple: Order => Tpl   = Order.unapply(_).fold(???)(identity) // honest, at least...
  val untuple: Tpl => Order = tpl => tpl |> (Order.apply(_, _, _, _, _)).tupled

  implicit val eq    = Eq.fromUniversalEquals[Order]
  implicit val order = cats.Order by tuple
  implicit val monoid = new Monoid[Order] {

    def combine(a: Order, b: Order): Order = tuple(a) |+| tuple(b) |> untuple

    lazy val empty: Order = Monoid[Tpl].empty |> untuple
  }
}

object MuhDomain {
  lazy val db = Db(
    Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    ),
    Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )
  )

  lazy val fd = Map(
    "Alice"   -> "37",
    "Bob"     -> "23",
    "Carol"   -> "42",
    "Dave"    -> "27",
    "Jaimie"  -> "33",
    "Kerry"   -> "18",
    "Leslie"  -> "31",
    "Terry"   -> "19",
    "Mallory" -> "67"
  )
}

sealed trait Denomination extends Any {
  def symbol: String
  def code: String
  // etc
}

object Denomination {}

final case class USD(val amount: BigDecimal) extends AnyVal with Denomination {
  override def symbol: String = "$"
  override def code: String   = "USD"
}
