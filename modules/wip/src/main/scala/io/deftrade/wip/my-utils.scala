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
  * We are of the opinion that `This is fine.`
  * (For a `utils` collection here.)
  */


import cats.{ Eq, Functor, Key, Monad }
import cats.syntax.all._

/**
  * This might be of general use.
  */
case class Db(usernames: Map[Int, String], passwords: Map[String, String])

object Db {

  import cats.data.Reader
  type DbReader[R] = Reader[Db, R]

  implicit lazy val eq = Eq.fromUniversalEquals[Db]

  lazy val usernameReader: DbReader[Map[Int, String]]     = Reader(_.usernames)
  lazy val passwordsReader: DbReader[Map[String, String]] = Reader(_.passwords)

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(_.usernames get userId)

  /**
    * Yes, this is an unususally phat interface. For instructional purposes only.
    */
  def findYoozername(userId: Int): DbReader[Option[String]] =
    for {
      uns <- usernameReader
    } yield uns get userId

  import cats.instances.string._
  import cats.instances.option._
  import cats.syntax.eq._

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    for { pws <- passwordsReader } yield pws get username contains password

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      ou <- findUsername(userId)
      ok <- ou.fold(false.pure[DbReader])(checkPassword(_, password))
    } yield ok
}

/**
  * Almost certainly of pedagogic interest only.
  */
sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A)                        extends Tree[A]

object Tree {

  implicit def eq[T <: Tree[_]] = Eq.fromUniversalEquals[T]

  implicit lazy val functor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(left map f, right map f)
      case Leaf(value)         => Leaf(f(value))
    }
  }

  implicit lazy val treeMonad = new Monad[Tree] {
    def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] = tree match {
      case Branch(l, r) => Branch(flatMap(l)(fn), flatMap(r)(fn))
      case Leaf(a)      => a |> fn
    }

    def pure[A](a: A): Tree[A] = Leaf(a)

    // @annotation.tailrec
    def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] = fn(a) match {
      case Branch(l, r) =>
        Branch(
          flatMap(l) {
            case Left(a)  => tailRecM(a)(fn)
            case Right(b) => b |> pure
          },
          flatMap(r) {
            case Left(a)  => tailRecM(a)(fn)
            case Right(b) => b |> pure
          }
        )
      case Leaf(Left(a1)) => tailRecM(a1)(fn)
      case Leaf(Right(b)) => Leaf(b)
    }
  }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

}

/**
  * Of pedagogic interest only.
  */
trait MuhMonad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  /**
    * Making [[map]] `final` would preclude optimization. This could be desirable, or not.
    */
  override def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(_ |> func |> pure)
}
object MuhMonadInstances {
  implicit val id = new MuhMonad[Key] {
    override def pure[A](a: A): Key[A]                                   = a
    override def flatMap[A, B](value: Key[A])(func: A => Key[B]): Key[B] = value |> func

    /**
      * Note we don't need to override here; but for Key we can optimize...
      */
    override def map[A, B](value: Key[A])(func: A => B): Key[B] = value |> func // |> pure
  }
}

object GhoshReader {

  case class Reader[R, A](run: R => A) {
    def map[B](f: A => B): Reader[R, B] = Reader(run andThen f)

    def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => run andThen f apply r run r)
  }
}

// /**
//   * Transformations from `camelCase` to other case conventions.
//   *
//   * TODO: this is only speculatively useful
//   */
  // object camelfake { outer =>
//
//   def camelToSnake(camelName: String): String  = camelTo("_")(camelName)
//   def camelToHyphen(camelName: String): String = camelTo("-")(camelName)
//   def camelToDot(camelName: String): String    = camelTo(".")(camelName)
//   def camelToWord(camelName: String): String   = camelTo(" ")(camelName)
//
//   implicit class CamelOps(val camelName: String) extends AnyVal {
//     def camelToSnake: String  = outer camelToSnake camelName
//     def camelToHyphen: String = outer camelToHyphen camelName
//     def camelToDot: String    = outer camelToDot camelName
//     def camelToWord: String   = outer camelToWord camelName
//   }
//
//   object camelTo {
//
//     def apply(sep: String)(name: String): String = {
//       val osc                        = maybeSepFrom(sep)
//       val bh: Seq[Char] => Seq[Char] = bustHumps(osc)(_)
//       val sc: String => Seq[Char]    = splitCaps(osc)(_)
//       (sc andThen bh)(name).mkString
//     }
//
//     private val uppers    = 'A' to 'Z'
//     private val nonUppers = ('a' to 'z') ++ ('0' to '9') :+ '_' :+ '$'
//
//     @SuppressWarnings(Array("org.wartremover.warts.Any"))
//     private def splitCaps(sep: Option[Char])(name: String): Seq[Char] =
//       name
//         .foldLeft(Seq.empty[Char]) { (b, a) =>
//           (a, b) match { // yeah just flip your head around, it's easier, trust self
//             case (c, h +: g +: t)
//                 if (uppers contains g) &&
//                   (uppers contains h) &&
//                   (nonUppers contains c) => // sep between g and h
//               sep.fold(c +: h +: g +: t)(c +: h +: _ +: g +: t)
//             case _ => a +: b
//           }
//         }
//         .reverse
//
//     @SuppressWarnings(Array("org.wartremover.warts.Any"))
//     private def bustHumps(sep: Option[Char])(name: Seq[Char]): Seq[Char] =
//       name.foldRight(Seq.empty[Char]) { (a, b) =>
//         (a, b) match {
//           case (c, h +: _) if (nonUppers contains c) && (uppers contains h) =>
//             sep.fold(a +: b)(a +: _ +: b)
//           case _ =>
//             a +: b
//         }
//       }
//
//     private def maybeSepFrom(s: String): Option[Char] = s match {
//       case "_" => Some('_')
//       case "-" => Some('-')
//       case _   => None
//     }
//   }
// }


// /** */
// sealed abstract case class CountryCode(
//     alpha2: Alpha2,
//     alpha3: Alpha3,
//     countryCode: Num3,
//     name: String Refined NonEmpty,
//     regionCode: Option[Num3],
//     subRegionCode: Option[Num3],
//     intermediateRegionCode: Option[Num3],
//     region: VarChar,
//     subRegion: VarChar,
//     intermediateRegion: VarChar,
//     // iso_3166_2: NonEmptyVarChar,
// )
//
// /** */
// object CountryCode extends WithOpaqueKey[Int, CountryCode] {
//
//   def regions: Map[Num3, VarChar]             = ???
//   def intermediateRegions: Map[Num3, VarChar] = ???
//   def subRegions: Map[Num3, VarChar]          = ???
//
//   def apply(alpha2: Alpha2,
//             alpha3: Alpha3,
//             countryCode: Num3,
//             name: String Refined NonEmpty,
//             regionCode: Option[Num3],
//             subRegionCode: Option[Num3],
//             intermediateRegionCode: Option[Num3]): CountryCode =
//     new CountryCode(
//       alpha2,
//       alpha3,
//       countryCode,
//       name,
//       regionCode,
//       subRegionCode,
//       intermediateRegionCode,
//       region = (regions get countryCode).fold(VarChar.empty)(identity),
//       subRegion = (regions get countryCode).fold(VarChar.empty)(identity),
//       intermediateRegion = (regions get countryCode).fold(VarChar.empty)(identity),
//       // s"ISO 3166-2:$alpha2",
//     ) {}
// }
