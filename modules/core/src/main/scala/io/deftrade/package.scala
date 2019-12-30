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

import cats.implicits._
import cats._
import cats.kernel.CommutativeGroup

/**
  * All abstract everything only.
  */
package object deftrade extends deftrade.results {

  /** */
  type Label   = refinements.Label
  type IsLabel = refinements.IsLabel

  /**  */
  def groupBy[F[_]: Applicative: Foldable: SemigroupK, K, A](
      as: F[A]
  )(
      f: A => K
  ): Map[K, F[A]] = {

    val SA = SemigroupK[F].algebra[A]
    import SA.combine

    as.foldLeft(Map.empty[K, F[A]]) { (acc, a) =>
      val key = f(a)
      val v   = a.pure[F]
      acc + (key -> (acc get key).fold(v)(vs => combine(vs, v)))
    }
  }

  /**  */
  def index[F[_]: Applicative: Foldable: SemigroupK, K, V](
      kvs: F[(K, V)]
  ): Map[K, F[V]] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs map (_._2))
    }

  /**  */
  def indexAndSum[F[_]: Applicative: Foldable: SemigroupK, K, V: CommutativeGroup](
      kvs: F[(K, V)]
  ): Map[K, V] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs foldMap (_._2))
    }

  /**
    * Informs wart remover that the value is intentionally discarded.
    *
    * Useful for checking whether a thing compiles at all. Hard to miss on a code review.
    */
  val discardValue: Any => Unit = (_: Any) => ()

  /**
    * Bind a message to an assertion function.
    *
    * Handy for development. If you write trading algos, development is basically "forever".
    */
  def assertOrElse(message: String): Boolean => Unit = assert(_, message)
}

package deftrade {

  import money.Currency
  import model.capital.Instrument

  /**
    * The answer to this question: "in what we price things?"
    *
    * `Numéraire` is formal finance term which, contrary to what a naive anglophone might think,
    * signifies the ''denominator'' for contracts and transactions.
    */
  sealed trait Numéraire

  /** */
  object Numéraire {

    /** non-sealed extension point */
    trait InCurrency extends Numéraire

    /** */
    object InCurrency {

      /** */
      def unapply(n: Numéraire): Option[InCurrency] = n match {
        case Currency(c) => c.some
        case _           => none
      }
    }

    /** non-sealed extension point */
    trait InKind extends Numéraire

    /** */
    object InKind {

      /** */
      def unapply(n: Numéraire): Option[InKind] = n match {
        case instrument @ Instrument(_, _, _, _, _) => instrument.some
        case _                                      => none
      }
    }
  }
}
