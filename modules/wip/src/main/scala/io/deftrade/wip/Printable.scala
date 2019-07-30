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
  * `Printable` is our stab at `CoinOp` domain typeclasses.
  * We take conventions from [[cats]], except we'll try the import tax free solutions.
  * We can `Print` an `A`. Now what?
  * Upshot: [[contramap]] lets us make `Print`able any `B` with an `A => B`.
  */
trait Printable[A] { self =>

  /**
    * Renders an `A` as a String.
    */
  def format(a: A): String

  /**
    * Create a new `Printable` instance from an existing instance, and a function to map a new type * to the type `format`ed by the existing `Printable` instance.
    * <p />
    * How to understand the terminology:
    * - `map`       -> map _from_ type `A` (_to_   type `B`)
    * - `contramap` -> map _to_   type `A` (_from_ type `B`)
    * @see [[cats.Show.catsContravariantForShow]] for an example of how [[cats]] "really" handles this.
    */
  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    override def format(value: B): String = self format (value |> func)
  }
}

/**
  * Companion object best practices, practiced. Sorta.
  */
object Printable {

  /**
    * Idiomatic use of `apply` supresses `implicitly` noise.
    */
  def apply[A: Printable]: Printable[A] = implicitly

  // nb all this is terrible naming imho
  def format[A: Printable](a: A): String = Printable[A] format a
  def print[A: Printable](a: A): Unit    = println(format(a))

  implicit val int = new Printable[Int] {
    override def format(a: Int) = a.toString
  }
  implicit val string = new Printable[String] {
    override def format(a: String) = s""""$a""""
  }
  implicit val boolean = new Printable[Boolean] {
    override def format(a: Boolean): String = if (a) "yes" else "no"
  }
}

/**
  * With this convention, syntax ops are imported directly and explicitly rather than via the
  * "no import tax" method...
  */
trait PrintableSyntax {
  implicit class PrintOps[A: Printable](a: A) {
    def format: String = Printable[A] format a
    def print(): Unit  = println(format)
  }
}

/**
  * Premixed PrintableSyntax instance, ready for import.
  */
object PrintableSyntax extends PrintableSyntax
