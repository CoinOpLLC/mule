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

package io

import cats._
import cats.kernel.CommutativeGroup
import cats.data.{ NonEmptyChain, Validated }
import cats.implicits._

import spire.math.Fractional

import scala.language.higherKinds

/** House rules. */
package object deftrade {

  /**
    * `Result` types
    */
  type Result[T]     = Either[Fail, T]
  type ResultV[T]    = Validated[Fail, T]
  type ResultVnec[T] = Validated[NonEmptyChain[Fail], T]

  def groupBy[F[_]: Applicative: Foldable: MonoidK, K, A](
      as: F[A]
  )(
      f: A => K
  ): Map[K, F[A]] = {

    val MA = MonoidK[F].algebra[A]
    import MA.{ combine, empty }

    as.foldLeft(Map.empty[K, F[A]]) { (acc, a) =>
      val key    = f(a)
      def asForK = (acc get key).fold(empty)(identity)
      acc + (key -> combine(asForK, a.pure[F]))
    }
  }

  def index[F[_]: Applicative: Foldable: MonoidK, K, V](
      kvs: F[(K, V)]
  ): Map[K, F[V]] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs map (_._2))
    }

  def accumulate[F[_]: Applicative: Foldable: MonoidK, K, V: CommutativeGroup](
      kvs: F[(K, V)]
  ): Map[K, V] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs foldMap (_._2))
    }

  /**
    *
    */
  implicit class SweetMap[K, V](val m: Map[K, V]) extends AnyVal {

    def getWithZero(k: K)(implicit V: Fractional[V]): V = (m get k).fold(V.zero)(identity)

    def sumValues(implicit V: Fractional[V]): V = m.map(_._2).fold(V.zero)(V.plus)

  }

  /**
    * Bind a message to an assertion function.
    * Handy for development. If you write trading algos, this is basically "forever".
    */
  def assertOrElse(msg: String): Boolean => Unit = assert(_, msg)

  /**
    * Informs wart remover that the value is intentionally discarded.
    * Useful for checking whether a thing compiles at all. Hard to miss on a code review.
    */
  val discardValue: Any => Unit = (_: Any) => ()

  /**
    * I hear nice things about OCaml. So I stole something from it.
    * The |> operator pairs nicely with `discardValues`.
    */
  implicit final class PipeToFunction1[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }

  /**
    * Make `Seq` immutable. See:
    * - [this post](https://hseeberger.wordpress.com/2013/10/25/attention-seq-is-not-immutable/),
    * and also
    * - [these comments](https://disqus.com/home/discussion/heikosblog/attention_seq_is_not_immutable_heikos_blog/).
    */
  type Seq[+A] = scala.collection.immutable.Seq[A]
  val Seq = scala.collection.immutable.Seq

  /** IndexedSeq is also made immutable */
  type IndexedSeq[+A] = scala.collection.immutable.IndexedSeq[A]
  val IndexedSeq = scala.collection.immutable.IndexedSeq
}
