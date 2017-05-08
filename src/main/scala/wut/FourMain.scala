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

import scala.language.higherKinds

import cats.{ Eval, Functor, Id /*, Monad */ }

import cats.syntax.eq._
import cats.syntax.option._

import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._

import cats.instances.int._
import cats.instances.option._

object FourMain {
  trait MuhMonad[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    override def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(_ |> func |> pure)
  }

  val muhIdMonadInstance = new MuhMonad[Id] {
    override def pure[A](a: A): Id[A]                                 = a
    override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = value |> func
    override def map[A, B](value: Id[A])(func: A => B): Id[B]         = value |> func // |> pure
  }

  1.pure[Option] === 1.some |> assert

  for {
    l <- 1: Id[Int]
    s <- 20: Id[Int]
    d <- 12: Id[Int]
  } yield l * s * d === 240 |> assert

  val foo = Eval.now((0xfeedface * 0x2badbabe + 7) % 13)
  println(foo)

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
}
