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

/**
  * try this, why not
  */
object TypeclassStuff {

  import cats.{ Eq, Monoid, Show }
  import cats.implicits._

  import model.Api._

  import Kats._

  import Printable._
  import PrintableSyntax._

  /*
   * Exercises 1.2.5:  Cat Show
   */
  //
  // another lame comment

  implicit val kittehShow = Show show [Kitteh] (_.format)

  implicit val kittehEq = Eq.fromUniversalEquals[Kitteh]

  123 === 123 |> assert

  1.some =!= None |> assert

  // Note you get tuples for free.
  (maru === ara, maru =!= ara) === ((false, true)) |> assert

  Monoid[String].combine("foo", "bar") === "foobar" |> assert

  def sum[A: Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].empty)(_ |+| _)

  val o1 = Order.legacy(555.550001, 78345)
  val o2 = Order.legacy(168.020660, 186283)

  (o1 |+| o2) === Order.legacy(BigDecimal(723.570661), 264628) |> assertOrElse((o1 |+| o2).toString)

  val map1 = Map("a" -> 1, "b" -> 2)
  val map2 = Map("b" -> 3, "d" -> 4)
  val mm   = map1 |+| map2
  mm === Map("a" -> 1, "b" -> (2 + 3), "d" -> 4) |> assert

  val m1   = Map(1337 -> o1)
  val m1_a = Map(1337 -> o2)
  val m2   = Map(4958 -> Order.legacy(666.880033, 123456))
  val mmm  = m1 |+| m1_a |+| m2
  mmm === Map(
    4958 -> Order.legacy(666.880033, 123456),
    1337 -> Order.legacy(723.570661, 264628)
  ) |> assertOrElse(mmm.toString)

  import Kats.maru
  import Printable.format

  // Printable print "hello"
  format("hello") === "\"hello\""                                      |> assert
  format(true) === "yes"                                               |> assert
  format(Box(maru)) === "\"Box(Kitteh(Maru,9,Scottish Fold,Kibble))\"" |> assert

  import Codec.{ decode, encode }
  encode(Box(123)) === "123"                |> assert
  decode[Box[Int]]("618") === Box(618).some |> assert

}
