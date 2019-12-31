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
import cats._
import cats.kernel.CommutativeGroup

/**
  * Records and computations defining the domain models and services
  * for '''financial market participants'''.
  *
  * What financial market participants? Banks, possibly,
  * but also Credit Unions, CDFIs, loan funds, CRE funds,
  * private equity funds, hedge funds, crypto exchanges, and private capital pools generally.
  *
  * Also, the CFO of any ''financially sophisticated'' but ''non-financial'' firm is typically
  * considered a financial market participant by the nature of the transactions they engage in,
  * particularly if the firm is sizeable.
  *
  * This package object is where the policy decision to use [[scala.math.BigDecimal]]
  * for [[money.Money]], and [[scala.Double]] for other [[layers.ModuleTypes.Quantity]]s, is made.
  *
  * Also, here we choose generic tax accounting for entities treated as partnerships.
  *
  * Different objects, package or otherwise, could make different policy decisions.
  */
package object model
/*
  All layers and augments need to agree on certain types:
     */
    extends ModuleTypes.Aux[
      /* type MonetaryAmount = */ BigDecimal,
      /* type Quantity       = */ Double
    ]
    //
    // the full stack of layered capabilitities
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

  /**  */
  def groupBy[
      F[_]: Applicative: Foldable: SemigroupK,
      K,
      A
  ](as: F[A])(f: A => K): Map[K, F[A]] = {

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
  def indexCG[F[_]: Applicative: Foldable: SemigroupK, K, V: CommutativeGroup](
      kvs: F[(K, V)]
  ): Map[K, V] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs foldMap (_._2))
    }
}
