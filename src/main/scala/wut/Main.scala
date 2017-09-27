/*
 * Copyright 2017 47 Degrees, LLC. <http://www.47deg.com>
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

import cats.{ Eq, Eval, Id, Monoid, Show }

// import cats.syntax.show._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.semigroup._

import cats.instances.int._
import cats.instances.string._
import cats.instances.option._
import cats.instances.tuple._
import cats.instances.boolean._
import cats.instances.vector._

/**
  * try this, why not
  */
object TypeclassStuff {

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

  import cats.instances.map._

  val o1 = Order(555.550001, 78345)
  val o2 = Order(168.020660, 186283)

  (o1 |+| o2) === Order(723.570661, 264628) |> assert

  val map1 = Map("a" -> 1, "b" -> 2)
  val map2 = Map("b" -> 3, "d" -> 4)
  val mm   = map1 |+| map2
  mm === Map("a" -> 1, "b" -> (2 + 3), "d" -> 4) |> assert

  val m1   = Map(1337 -> o1)
  val m1_a = Map(1337 -> o2)
  val m2   = Map(4958 -> Order(666.880033, 123456))
  val mmm  = m1 |+| m1_a |+| m2
  mmm === Map(
    4958 -> Order(666.880033, 123456),
    1337 -> Order(723.570661, 264628)
  ) |> assert

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

object TreeStuff {

  import cats.syntax.all._

  1.pure[Option] === 1.some |> assert

  for {
    l <- 1: Id[Int]
    s <- 20: Id[Int]
    d <- 12: Id[Int]
  } yield l * s * d === 240 |> assert

  val foo = Eval.now((0xfeedface * 0x2badbabe + 7) % 13)
  // println(foo)

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail => Eval defer foldRight(tail, fn(head, acc))(fn)
      case Nil          => Eval now acc
    }

  val n        = 1000
  val longList = List.fill(n)(1)
  foldRight(longList, 0) { (a, b) =>
    b + a
  }.value === n |> assert

  import cats.data.Writer
  // type Writer[W, A] = WriterT[Id, W, A]
  type Logged[A] = Writer[Vector[String], A]

  Monoid[String] |> discardValue

  val loggedInt = 618.pure[Logged]

  val dogMsgs = Vector(
    "me: _Hello_",
    "dog: _Yes this is dog_",
    "me: *Woof*"
  )

  val loggedDogMsgsUnit: Writer[Vector[String], Unit] = dogMsgs.tell

  val loggedDogMsgsDouble: Writer[Vector[String], Double] = 6.18 writer dogMsgs

  val exNihiloNihiloFit: Writer[Vector[String], Unit] = () writer Vector.empty[String]

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  type LI = Logged[Int]

  // my version. which is lame.
  def factoRoyale(ln: LI): LI = ln flatMap { n =>
    slowly {
      (if (n === 0) 1.pure[Logged]
       else factoRoyale(ln map (_ - 1)) map (n * _)) mapBoth { (log, ans) =>
        (log :+ s"fact $n $ans", ans)
      }
    }
  }

  def faktoreal(n: Int): Logged[Int] =
    for {
      ans <- n match {
              case 0 => 1.pure[Logged]
              case _ => slowly { (n - 1 |> faktoreal) map (_ * n) }
            }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly { (n - 1 |> factorial) map (_ * n) }
      _   <- Vector(s"fact $n $ans").tell
    } yield ans

  import Db.checkLogin
  import MuhDomain.db

  db === Db(
    usernames = Map(1      -> "dade", 2          -> "kate", 3           -> "margo"),
    passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
  ) |> assert

  checkLogin(1, "zerocool").run(db) |> assert
  // res8: cats.Id[Boolean] = true

  !checkLogin(4, "davinci").run(db) |> assert
  // res9: cats.Id[Boolean] = false

  import Tree.{ branch, leaf }

  val (ll, rl) = (leaf(22), leaf(33))
  val tr       = branch(ll, rl)

  tr === Branch(Leaf(22), Leaf(33)) |> assert

  val tr7 = tr map (_ + 7)

  val trtr = for {
    i <- tr
    j <- tr7
  } yield i + j

  trtr === Branch(Branch(Leaf(51), Leaf(62)), Branch(Leaf(62), Leaf(73))) |> assert

}

/**
  * Do: all the things.
  */
object Main extends App {

  /*
   *I wrote a little kata it goes like this...
   */
  val xs = List(1, 2, 3)
  // val oxs = List(1.some, None, 2.some, 3.some, None)
  val oxs = List(1.some, 2.some, 3.some)

  val izSaem = (xs zip oxs) forall {
    case (r, Some(l)) => r === l
    case _            => ??? // yeah, I know, right?
  }

  izSaem |> assert

  /*
   * [cracks knuckles] OK now do all the things.
   */
  TypeclassStuff        |> discardValue
  TreeStuff             |> discardValue
  StaetMoanad           |> discardValue
  MonadTransformerStuff |> discardValue
  CartesianStuff        |> discardValue
  TraverseStuff         |> discardValue
  PureConfigExample     |> discardValue
  SpireExamples         |> discardValue
}
