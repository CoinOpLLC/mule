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
package model

import cats.implicits._
import cats.{ Order }
import cats.kernel.CommutativeGroup
import cats.data.{ NonEmptyList, NonEmptyMap }

import scala.collection.immutable.SortedMap

/**
  * Records and computations defining a layered set of financial domain models and services.
  */
package object slices {

  /**
    */
  final type UnitPartition[K, N] = Partition[K, N, Partition.IsNormalized]

  /**
    */
  final type ScaledPartition[K, N] = Partition[K, N, Partition.IsPositive]

  /**
    */
  def groupBy[K: Order, A](as: A*)(f: A => K): Map[K, List[A]] =
    as.foldLeft(SortedMap.empty[K, List[A]]) { (acc, a) =>
      val k = f(a)
      acc + (k -> (acc get k).fold(List.empty[A])(a :: _))
    }

  /**
    */
  def indexAndSum[K: Order, V: CommutativeGroup](
      kvs: (K, V)*
  ): Map[K, V] =
    groupBy(kvs: _*)(_._1) map { case (k, kvs) => (k, kvs map (_._2) foldMap identity) }

  /**
    */
  def groupBy[K: Order, A](a: A, as: A*)(f: A => K): NonEmptyMap[K, NonEmptyList[A]] =
    as.foldLeft(NonEmptyMap.one(f(a), NonEmptyList one a)) { (acc, a) =>
      val k = f(a)
      acc add k -> acc(k).fold(NonEmptyList one a)(a :: _)
    }

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def indexAndSum[K: Order, V: CommutativeGroup](
      kv: (K, V),
      kvs: (K, V)*
  ): NonEmptyMap[K, V] =
    groupBy(kv, kvs: _*)(_._1) map (_ foldMap (_._2))

}
