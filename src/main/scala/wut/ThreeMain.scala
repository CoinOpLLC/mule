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

// import scala.language.postfixOps
// import scala.language.higherKinds

import cats.{ Eq, Functor }
import cats.syntax.all._
import cats.instances.string._
import cats.instances.option._

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A)                        extends Tree[A]

final case class Box[A](value: A)

object ThreeMain {

  import Kittez.maru
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

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): Option[A]

    final def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String         = enc(value) |> self.encode
      override def decode(value: String): Option[B] = self decode value map dec
    }
  }
  object Codec {
    def apply[A: Codec]: Codec[A] = implicitly[Codec[A]]
  }

  def encode[A: Codec](value: A): String = Codec[A] encode value

  def decode[A: Codec](value: String): Option[A] = Codec[A] decode value

  implicit val intCodec = new Codec[Int] {
    override def encode(value: Int): String         = value.toString
    override def decode(value: String): Option[Int] = scala.util.Try(value.toInt).toOption
  }
  implicit def boxCodec[A: Codec]: Codec[Box[A]] = Codec[A] imap (Box[A], _.value)
  encode(Box(123)) === "123" |> assert

  implicit def boxEq[A] = Eq.fromUniversalEquals[Box[A]]
  decode[Box[Int]]("618") === Box(618).some |> assert

}
