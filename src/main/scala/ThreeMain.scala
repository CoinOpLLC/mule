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

// import scala.language.higherKinds

import cats.Functor
import cats.syntax.all._
import cats.instances.string._

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Box[A](value: A)

object ThreeMain extends MyWay {

  import Kittez._
  import PrintableInstances.kittehPrintable

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(left map f, right map f)
      case Leaf(value)         => Leaf(f(value))
    }
  }
  def format[A](value: A)(implicit p: Printable[A]): String = p format value

  implicit val stringPrintable = new Printable[String] {
    def format(value: String): String = "\"" + value + "\""
  }

  implicit val booleanPrintable = new Printable[Boolean] {
    def format(value: Boolean): String = if (value) "yes" else "no"
  }

  implicit def boxPrintable[A: Printable] = Printable[String].contramap { (b: Box[A]) =>
    b.toString
  }

  format("hello") === """"hello"""" |> assert

  format(true) === "yes" |> assert

  format(Box(maru)) === "\"Box(Kitteh(Maru,9,Scottish Fold))\"" |> assert

  trait Codec[A] {
    def encode(value: A): String
    def decode(value: String): Option[A]

    def imap[B](dec: A => B, enc: B => A): Codec[B] = ???
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): Option[A] =
    c.decode(value)
}
