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

import scala.language.higherKinds
// import scala.language.implicitConversions

import cats.{ Eq, Key, Monad, Semigroupal }

import cats.data.{ Validated, NonEmptyList => NEL }

import cats.implicits._
// import cats.instances.int._
// import cats.instances.string._
// import cats.instances.boolean._
// import cats.instances.tuple._
// import cats.instances.option._
// import cats.instances.either._
//
// import cats.syntax.cartesian._
// import cats.syntax.validated._
// import cats.syntax.either._
// import cats.syntax.option._
// import cats.syntax.eq._

object SemigroupalStuff {

  val p = Semigroupal tuple3 (23.some, "oh hai".some, true.some)
  val q = (23.some, "oh hai".some, true.some)

  p === q.tupled |> assert

  def product[M[_]: Monad, A, B](
      fa: M[A],
      fb: M[B]
  ): M[(A, B)] = {
    val m = Monad[M]
    m.flatMap(fa) { a =>
      m.map(fb) { b =>
        (a, b)
      }
    }
  }

  {
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    def pr[M[_]: Monad, A, B](fa: M[A], fb: M[B]): M[(A, B)] = for { a <- fa; b <- fb } yield (a, b)
    pr[Key, Unit, Unit] _
  } |> discardValue

  def vx(s: String) = Validated catchNonFatal { s.toInt }

  val vx2 = 23.valid[Throwable]

  val s23 = "23"

  val result = {
    implicit val eqThrowable = Eq.allEqual[Throwable]
    vx(s23) === vx2
  }
  result |> assert

  FormValidation |> discardValue
}

/**
  * FormValidation illustrates `Validation` by combining all errors in a NEL.
  */
object FormValidation {

  import scala.util.Try

  case class User(name: String, age: Int)

  type FormData       = String Map String
  type ErrorsOr[A]    = NEL[String] Either A
  type AllErrorsOr[A] = NEL[String] Validated A

  def getValue(name: String)(fd: FormData): ErrorsOr[String] =
    (fd get name) toRight (NEL of s"$name: no such name")

  def parseInt(name: String)(s: String): ErrorsOr[Int] =
    Try(s.toInt).toEither leftMap { t =>
      NEL of s"$name: can't parse $s as an int: caught $t"
    }

  def nonBlank(name: String)(s: String): ErrorsOr[String] =
    s.some filter (_ =!= "") toRight (NEL of s"$name not blank")

  def nonNegative(name: String)(n: Int): ErrorsOr[Int] =
    n.some filter (_ >= 0) toRight (NEL of s"$name is negative")

  def readName(fd: FormData): ErrorsOr[String] =
    for {
      name   <- getValue("name")(fd)
      nbName <- nonBlank("name")(name)
    } yield nbName

  def readAge(fd: FormData): ErrorsOr[Int] =
    for {
      age   <- getValue("age")(fd)
      nbAge <- nonBlank("age")(age)
      i     <- parseInt("age")(nbAge)
    } yield i

  def toAllErrorsOr[A](eoi: ErrorsOr[A]): AllErrorsOr[A] = Validated fromEither eoi

  def readForm(fd: FormData): AllErrorsOr[User] =
    (toAllErrorsOr(readName(fd)), toAllErrorsOr(readAge(fd))) mapN User.apply

//   def readPhørm(fd: FormData): AllErrorsOr[User] = // #FIXME: #wart: inferred `Any`
//     (readName(fd).toValidated, readAge(fd).toValidated) mapN User.apply

}
