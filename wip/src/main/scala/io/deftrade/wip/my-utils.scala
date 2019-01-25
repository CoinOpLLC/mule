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
import scala.language.higherKinds

import cats.{ Eq, Functor, Id, Monad }
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
  implicit val id = new MuhMonad[Id] {
    override def pure[A](a: A): Id[A]                                 = a
    override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = value |> func

    /**
      * Note we don't need to override here; but for Id we can optimize...
      */
    override def map[A, B](value: Id[A])(func: A => B): Id[B] = value |> func // |> pure
  }
}

object GhoshReader {

  case class Reader[R, A](run: R => A) {
    def map[B](f: A => B): Reader[R, B] = Reader(run andThen f)

    def flatMap[B](f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => run andThen f apply r run r)
  }
}
