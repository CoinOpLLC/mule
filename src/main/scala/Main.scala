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

import cats.{ Eq, Show }

import cats.syntax.show._
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
final case class Kitteh(name: String, age: Int, color: String)

trait Printable[A] {
  def format(a: A): String
}

object Printable {

  /**
    * Ideomatic use of `apply` supresses `implicitly` noise.
    */
  def apply[A: Printable]: Printable[A]  = implicitly[Printable[A]]
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
  * What the actual fuck, people.
  */
object Main extends App {
  OneMain
  println(TwoMain.s.show)

  import TwoMain.{sumz, suem}

  val xs = List(1,2,3)
  val oxs = List(1.some, None, 2.some, 3.some, None)

  println(s"muh sumz: ${sumz(xs)}")
  println(s"muh suem: ${suem(oxs)}")

}

object TwoMain {
  import cats.Monoid

  val s = Monoid[String].combine("foo", "bar")

  def sumz(xs: List[Int]): Int = xs.foldLeft(Monoid[Int].empty)(_ |+| _)
  def suem[A: Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].empty)(_ |+| _)

  case class Order(totalCost: BigDecimal, quantity: Long)
  object Order {
    def combine = ??? // strategy: use unapply / tuple shit somehow
  }


}

/**
  * try this, why not
  */
object OneMain extends PrintableInstances with PrintableSyntax {

  val maru = Kitteh(name = "Maru", color = "Scottish Fold", age = 9)
  // maru.print()
  val ara = Kitteh("Ara", 8, "Tuxedo")

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
