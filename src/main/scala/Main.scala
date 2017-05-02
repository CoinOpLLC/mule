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
  def apply[A: Printable]: Printable[A] = implicitly[Printable[A]]
  def format[A: Printable](a: A): String = Printable[A] format a
  def print[A: Printable](a: A): Unit = println(format(a))
}

trait PrintableInstances {
  implicit val intPrintable = new Printable[Int] { override def format(a: Int) = a.toString }
  implicit val stringPrintable = new Printable[String] { override def format(a: String) = a }
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
    def print(): Unit = println(format)
  }
}

object PrintableSyntax extends PrintableSyntax


/**
* What the actual fuck, people.
*/

object Main extends App with PrintableInstances with PrintableSyntax {

  val maru = Kitteh(name="Maru", color="Scottish Fold", age=9)
  // maru.print()

  /*
  * Exercises 1.2.5:  Cat Show
  */
  //
  import cats.Show
  import cats.syntax.show._

  import cats.instances.int._

  implicit val kittehShow = Show show[Kitteh] { k => kittehPrintable format k }

  println(555.show)
  println(maru.show)

}
