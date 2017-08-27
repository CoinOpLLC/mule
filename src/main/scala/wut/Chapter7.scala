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

import scala.language.higherKinds
import scala.concurrent.Future

import cats.Applicative
import cats.instances.future._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.cartesian._

object Chapter7 {

  // Combining an accumulator and a hostname using an Applicative:

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum |@| func(item)).map(_ :+ _)
    }

  /**
    * To understand the use of `identity`, consider the mapping of type parameters:
    * A gets mapped to F[B]
    * which infers F and B to be F and B!
    * So `identity` makes perfect sense to use.
    * @return the list within a context.
    */
  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  import cats.data.Validated
  import cats.instances.list._ // Applicative[ErrorsOr] needs a Monoid[List]

  type ErrorsOr[A] = Validated[List[String], A]

  def vprocess(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) Validated valid n else Validated invalid List(s"$n is not even")
    }
}
