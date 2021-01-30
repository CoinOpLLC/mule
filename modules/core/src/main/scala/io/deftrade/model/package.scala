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

import model.layers._
import model.augments._

import cats.implicits._
import cats.{ Applicative, Foldable, Order, SemigroupK }
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.kernel.CommutativeGroup

/**
  * Records and computations defining a layered set of financial domain models and services.
  */
package object model
    extends ModuleTypes.Aux[
      /* type MonetaryAmount = */ BigDecimal,
      /* type Quantity       = */ Double
    ]
    //
    // the full stack of layered capabilities
    //
    with Ledger          // possibly distributed
    with Accounting      // debits, credits, and all that
    with Balances        // depends only on generic Accounting
    with MarketData      // WIP; IBRK will be first integration
    with OrderManagement // WIP; IBRK will be first integration
    //
    // PII firewalling simplified by eliminating dependencies:
    // `Accounts` layer can be commented out!
    //
    with Accounts // binding of legal entities to sets of positions */
    //
    // necessary package level augmentation
    //
    with IRS1065 { // replace or enhance as necessary

  /**
    */
  final type UnitPartition[K, N] = Partition[K, N, Partition.IsNormalized]

  /**
    */
  final type ScaledPartition[K, N] = Partition[K, N, Partition.IsPositive]

  /**
    */
  def groupBy[
      F[_]: Applicative: Foldable: SemigroupK,
      K,
      A
  ](fas: F[A])(f: A => K): Map[K, F[A]] = {

    val SA = SemigroupK[F].algebra[A]
    import SA.combine

    fas.foldLeft(Map.empty[K, F[A]]) { (acc, a) =>
      val key = f(a)
      val v   = a.pure[F]
      acc + (key -> (acc get key).fold(v)(vs => combine(vs, v)))
    }
  }

  /**
    */
  def groupBy[
      F[_]: Applicative: Foldable: SemigroupK,
      K: Order,
      A
  ](a: A, fas: F[A])(f: A => K): NonEmptyMap[K, F[A]] = {

    val SA = SemigroupK[F].algebra[A]
    import SA.combine

    val k = f(a)

    fas.foldLeft(NonEmptyMap.one(k, a.pure[F])) { (acc, a) =>
      val k = f(a)
      val v = a.pure[F]
      acc add k -> acc(k).fold(v)(vs => combine(vs, v))
    }
  }

  /**
    */
  def groupBy[K: Order, A](a: A, as: A*)(f: A => K): NonEmptyMap[K, NonEmptyList[A]] =
    as.foldLeft(NonEmptyMap.one(f(a), NonEmptyList one a)) { (acc, a) =>
      val k = f(a)
      acc add k -> acc(k).fold(NonEmptyList one a)(a :: _)
    }

  /**
    */
  def index[F[_]: Applicative: Foldable: SemigroupK, K, V](
      kvs: F[(K, V)]
  ): Map[K, F[V]] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs map (_._2))
    }

  /**
    */
  def indexAndSum[F[_]: Applicative: Foldable: SemigroupK, K: Order, V: CommutativeGroup](
      kvs: F[(K, V)]
  ): Map[K, V] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs foldMap (_._2))
    }

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def indexAndSum[K: Order, V: CommutativeGroup](
      kv: (K, V),
      kvs: (K, V)*
  ): NonEmptyMap[K, V] =
    groupBy(kv, kvs: _*)(_._1) map (_ foldMap (_._2))

  implicit def catsFeralStdCommutativeGroup[K, V: CommutativeGroup]: CommutativeGroup[Map[K, V]] =
    new MapCommutativeGroup[K, V]

///////////////////////////////////////////////////////////////////////////////////

  // object Accounts {`
  //
  //   val Right((accounts, rosters, contacts)) = for {
  //     accounts <- keyValueStore[IO] at "accounts.csv" ofChained Account
  //     rosters  <- keyValueStore[IO] at "rosters.csv" ofChained Roster
  //     parties  <- keyValueStore[IO] at "parties.csv" ofChained Party
  //     contacts <- valueStore[IO] at "contacts.csv" ofContentAddressed Contact
  //   } yield (accounts, rosters, parties, contacts)
  // }

}
