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

import model.layers._
import model.augments._

import cats.implicits._
import cats.{ Applicative, Foldable, Order, SemigroupK }
import cats.kernel.CommutativeGroup
import cats.kernel.instances.MapMonoid
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.effect.{ ContextShift, IO }

/**
  * Records and computations defining a layered set of financial domain models and services.
  */
package object pillars {

  /**
    */
  final type UnitPartition[K, N] = Partition[K, N, Partition.IsNormalized]

  /**
    */
  final type ScaledPartition[K, N] = Partition[K, N, Partition.IsPositive]

}
