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

package object model {
  type ItemId    = Long
  type AccountId = Long
  type Comment   = String

  def captureReturn[A, B](f: A => B) = new { type Return = B }
}

package model {

  import cats.{ Eq, Monoid }
  import cats.syntax.all._
  import cats.{ instances => ci }
  import ci.int._, ci.string._, ci.double._, ci.boolean._, ci.bigDecimal._, ci.long._
  import ci.option._, ci.tuple._

  case class Order(
      account: AccountId,
      item: ItemId,
      quantity: Long,
      totalCost: BigDecimal,
      comment: Option[Comment]
  )
  object Order {

    def legacy(bd: BigDecimal, q: Long): Order = apply(-1, -1, q, bd, None)

    type Tpl = (AccountId, ItemId, Long, BigDecimal, Option[Comment])
    val tuple: Order => Tpl   = Order.unapply(_).fold(???)(identity)
    val untuple: Tpl => Order = tpl => tpl |> (Order.apply(_, _, _, _, _)).tupled

    implicit val eq    = Eq.fromUniversalEquals[Order]
    implicit val order = cats.Order by tuple
    implicit val monoid = new Monoid[Order] {

      def combine(a: Order, b: Order): Order = tuple(a) |+| tuple(b) |> untuple

      lazy val empty: Order = Monoid[Tpl].empty |> untuple
    }
  }

  /**
    * Domain model class. The only domain that matters tbh.
    *
    * In a better example, these would be in their own file(s).
    */
// @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Kitteh(name: String, age: Int, color: String, favFood: String)
  object Kitteh {
    def apply(name: String, age: Int, color: String): Kitteh = Kitteh(name, age, color, "Kibble")
    implicit val printable = new Printable[Kitteh] {
      override def format(k: Kitteh) = {
        import k._
        s"OH HAI $name DESU HAZ $age YAERZ AM $color EATZ $favFood K THX BYE"
      }
    }

    import cats.data.Reader
    type KittehReader[A] = Reader[Kitteh, A]
    lazy val nameReader: KittehReader[String] = Reader(_.name)

    val add = (_: Int) + (_: Int)
    val not = ~(_: Int)

    val inc = add(_: Int, 1)
    val neg = not andThen inc
    val dec = add(_: Int, (1 |> neg))

  }

  /** Distinguished cats! */
  object Kats {
    val maru = Kitteh(name = "Maru", color = "Scottish Fold", age = 9)
    val ara  = Kitteh("Ara", 8, "Tuxedo")
  }

  final case class Box[A](value: A)
  object Box {
    implicit def eq[A: Eq] = Eq.fromUniversalEquals[Box[A]]

    implicit def boxPrintable[A: Printable] = Printable[String].contramap { (b: Box[A]) =>
      b.toString
    }
    implicit def boxCodec[A: Codec]: Codec[Box[A]] = Codec[A] imap (Box[A], _.value)
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
}
