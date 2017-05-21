/*
 * Copyright 2017 Fairfax Technologies LLC
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

import cats.{ Eq, Show }

// import cats.syntax.show._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.semigroup._

import cats.instances.int._
import cats.instances.string._
import cats.instances.option._
import cats.instances.tuple._
import cats.instances.boolean._

/*
 * ##### Chapter 1
 */

trait Printable[A] { self =>
  def format(a: A): String

  /**
    * Create a new `Printable` instance from an existing instance, and a function to map a new type to the type `format`ed by the existing `Printable` instance.
    */
  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    override def format(value: B): String = self format func(value)
  }
}

object Printable {

  /**
    * Ideomatic use of `apply` supresses `implicitly` noise.
    */
  def apply[A: Printable]: Printable[A]  = implicitly
  def format[A: Printable](a: A): String = Printable[A] format a
  def print[A: Printable](a: A): Unit    = println(format(a))
}

trait PrintableInstances {
  implicit val intPrintable = new Printable[Int] {
    override def format(a: Int) = a.toString
  }
  implicit val stringPrintable = new Printable[String] {
    override def format(a: String) = a
  }
  implicit val kittehPrintable = new Printable[Kitteh] {
    override def format(k: Kitteh) = {
      import k._
      s"OH HAI ${name} DESU HAZ ${age} YAERZ AM ${color} K BYE"
    }
  }
}

object PrintableInstances extends PrintableInstances

trait PrintableSyntax {
  implicit class PrintOps[A: Printable](a: A) {
    def format: String = Printable[A] format a
    def print(): Unit  = println(format)
  }
}

object PrintableSyntax extends PrintableSyntax

/**
  * Do: all the things.
  */
object Main extends App {

  // I wrote a little kata it goes like this...
  val xs = List(1, 2, 3)
  // val oxs = List(1.some, None, 2.some, 3.some, None)
  val oxs = List(1.some, 2.some, 3.some)

  val izSaem = (xs zip oxs) forall {
    case (r, Some(l)) => r === l
    case _            => ??? // yeah, I know, right?
  }

  izSaem |> assert

  OneMain |> discardValue
  TwoMain |> discardValue
  ThreeMain |> discardValue
  FourMain |> discardValue

}

final case class Kitteh(name: String, age: Int, color: String)

object Kittez {

  val maru = Kitteh(name = "Maru", color = "Scottish Fold", age = 9)
  val ara  = Kitteh("Ara", 8, "Tuxedo")
}

/**
  * try this, why not
  */
object OneMain extends PrintableInstances with PrintableSyntax {

  import Kittez._
  /*
   * Exercises 1.2.5:  Cat Show
   */
  //
  // another lame comment

  implicit val kittehShow = Show show [Kitteh] { k =>
    kittehPrintable format k
  }

  implicit val kittehEq = Eq.fromUniversalEquals[Kitteh]

  assert(123 === 123)

  assert(1.some =!= None)
  // res10: Boolean = true”

  assert((maru === ara, maru =!= ara) === ((false, true)))

}

object TwoMain {
  import cats.Monoid

  val s = Monoid[String].combine("foo", "bar")
  s === "foobar" |> assert

  def sumz(xs: List[Int]): Int        = xs.foldLeft(Monoid[Int].empty)(_ |+| _)
  def suem[A: Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].empty)(_ |+| _)

  import cats.instances.{ bigDecimal, long, map }
  import bigDecimal._, long._, map._
  case class Oardur(totalCost: BigDecimal, quantity: Long)

  /**
    * Objective here was to try to be as "generic" as possible.
    * I can start to see the motivation for shapless...
    */
  implicit val oardurMonoid = new Monoid[Oardur] {

    def combine(a: Oardur, b: Oardur): Oardur = {
      val l :: r :: Nil = List(a, b) map (Oardur unapply _)
      val Some(o)       = l |+| r
      (Oardur.apply _) tupled o
    }
    
    lazy val empty: Oardur = (Oardur.apply _) tupled Monoid[(BigDecimal, Long)].empty
  }

  implicit val oardurEq = Eq.fromUniversalEquals[Oardur]

  val o1 = Oardur(555.550001, 78345)
  val o2 = Oardur(168.020660, 186283)

  (o1 |+| o2) === Oardur(723.570661, 264628) |> assert

  val map1 = Map("a" -> 1, "b" -> 2)
  val map2 = Map("b" -> 3, "d" -> 4)
  val mm   = map1 |+| map2
  mm === Map("a" -> 1, "b" -> (2 + 3), "d" -> 4) |> assert

  val m1   = Map(1337 -> o1)
  val m1_a = Map(1337 -> o2)
  val m2   = Map(4958 -> Oardur(666.880033, 123456))
  val mmm  = m1 |+| m1_a |+| m2
  mmm === Map(
    4958 -> Oardur(666.880033, 123456),
    1337 -> Oardur(723.570661, 264628)
  ) |> assert
}
