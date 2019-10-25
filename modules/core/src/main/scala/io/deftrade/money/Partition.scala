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
package money

import implicits._

import cats.implicits._
import cats.Order
import cats.data.{ NonEmptyMap, NonEmptySet }

import spire.math.Fractional
import spire.syntax.field._

import eu.timepit.refined
import refined.refineV
import refined.api.Refined
import refined.numeric._
import refined.auto._

import scala.collection.immutable.SortedMap

/** A pot divided. (Or something like it.) */
sealed trait PartitionLike {

  type Key

  type Value

  final type PositiveValue = Value Refined Positive

  /** */
  def kvs: NonEmptyMap[Key, PositiveValue]

  /** */
  final def total(implicit V: Financial[Value]): V.Positive = kvs reduce V.positiveSemigroup.combine

  /** */
  final def keys: NonEmptySet[Key] = kvs.keys

  /** */
  final def toSortedMap: SortedMap[Key, PositiveValue] = kvs.toSortedMap

  /** */
  final def scaled(n: PositiveValue)(implicit K: Order[Key], V: Financial[Value]): Partition[Key, Value] =
    Partition unsafe (toSortedMap mapValues (_.value * n))

  /** */
  final def scaled[N: Financial, C: Currency](
      amount: Money[N, C]
  )(
      implicit V: Financial[Value]
  ): Map[Key, Money[N, C]] =
    toSortedMap mapValues (amount * _.value)

  /** Creates a [[UnitPartition]] from this one. */
  final def normalized(implicit K: Order[Key], V: Financial[Value]): UnitPartition[Key, Value] =
    UnitPartition unsafe [Key, Value] (toSortedMap mapValues (_.value / total))

  /** FIXME: implement and replace normalize */
  final def carmelized(
      implicit
      K: Order[Key],
      V: Financial[Value]
  ): UnitPartition[Key, V.`(0,1]`] = ???
}

/** Modelling the total equity stake, and the stakeholders thereof. */
sealed abstract case class Partition[K, V] private (
    kvs: NonEmptyMap[K, V]
) extends PartitionLike {

  final type Key   = K
  final type Value = V

  /** Creates a `Partition` of total `n`, proportional to self. */
  def proRated(
      n: PositiveValue
  )(
      implicit
      K: Order[Key],
      V: Financial[Value]
  ): Partition[Key, Value] =
    normalized scaled n // TODO feels half assed

  /** share acquired from each according to their proportion */
  def dilutedBy(key: K, share: V)(implicit K: Order[K], V: Financial[V]): Result[Partition[K, V]] =
    ???

  /** share returned to each according to their proportion */
  def retiredKey(key: K)(implicit K: Order[K], V: Financial[V]): Result[Partition[K, V]] =
    ???

}

/** */
object Partition {

  /** */
  private def apply[K: Order, V: Financial](kvs: NonEmptyMap[K, V]) = new Partition(kvs) {}

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: Order, V: Financial](shares: (K, V)*): Result[Partition[K, V]] =
    if (shares.toList.nonEmpty) unsafe(shares |> SortedMap.apply[K, V]).asRight
    else Result fail "must be non-empty"

  /**
    * Creates an allocation function (`V => Partition`) from a set of tranches,
    * with a designated key for the remainder (equity).
    */
  def waterfall[K: Order, V: Financial](
      tranches: Map[K, V],
      equity: K
  ): V => Partition[K, V] = ???

  /** def ipsa loquitur */
  def currified[K: Order, V: Financial](
      compute: NonEmptySet[K] => V => NonEmptyMap[K, V]
  )(
      keys: NonEmptySet[K]
  )(
      amount: V
  ) = compute(keys)(amount) |> apply[K, V]

  //
  private[deftrade] def unsafe[K: Order, V: Financial](sm: SortedMap[K, V]): Partition[K, V] =
    Partition(NonEmptyMap fromMapUnsafe sm)
}

/**
  * Models the equity of an entity as a single Unit.
  *
  * (Not to be confused with Scala Unit.)
  * TODO use refined to restrict V to be between 0 and 1
  */
sealed abstract case class UnitPartition[K, V] private (
    kvs: NonEmptyMap[K, V]
) extends PartitionLike {

  final type Key   = K
  final type Value = V

  /** Share acquired from each, according to their proportion. */
  def buyIn(key: K, share: V)(implicit K: Order[K], V: Financial[V]): Result[UnitPartition[K, V]] = ???
  // if (!(toSortedMap contains key) && refineV[Positive](share).isRight)
  //   Partition.unsafe(toSortedMap + (key -> share)).normalized.asRight
  // else
  //   Result fail s"bad params: key=$key, share=$share, kvs=$kvs"

  /** Share returned to each according to their proportion. */
  def sellOut(key: K)(implicit K: Order[K], V: Financial[V]): Result[UnitPartition[K, V]] = ???
  // UnitPartition.fromShares((toSortedMap - key).toList: _*)
}

/** Conventional creation patterns. */
object UnitPartition {

  /** Whole pie for me. */
  def single[K: Order, V: Financial](k: K): UnitPartition[K, V] =
    unsafe(SortedMap(k -> Financial[V].one))

  /** What every pizza slicer aims for; their's is a fine example. */
  def fair[K: Order, V: Financial](ks: NonEmptySet[K]): UnitPartition[K, V] = {
    val V        = Fractional[V]; import V._
    val oneSlice = one / (V fromLong ks.size)
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

  /** Total shares outstanding computed from the sum of `shares`. */
  def fromShares[K: Order, V: Financial](shares: (K, V)*): Result[UnitPartition[K, V]] =
    Partition.fromShares(shares: _*) map (_.normalized)

  /** `n` shares issued; sum of slices must equal whole pie */
  def fromTotalShares[K: Order, V: Financial](
      n: V
  )(
      ps: (K, V)*
  ): Result[UnitPartition[K, V]] = {
    val V = Fractional[V]; import V._

    val computedShares = ps.map(_._2).fold(zero)(plus)
    if (computedShares === n && inRangeForallShares(n, ps)) fromShares(ps: _*)
    else Result fail s"$computedShares != $n"

  }

  /** `exact` slices are claimed by the caller; this is checked. */
  def exact[K: Order, V: Financial](shares: (K, V)*): Result[UnitPartition[K, V]] = {
    val VF        = Fractional[V]; import VF._
    def isUnitary = one === shares.map(_._2).fold(zero)(plus)
    if (isUnitary && inRangeForallShares(one, shares))
      unsafe(SortedMap(shares: _*)).asRight
    else
      Result fail s"UnitPartition: invalid creation parms: $shares"
  }

  /** For the extractor. */
  object Single {

    /** Extracts a single key, if that's what's there. */
    def unapply[K: Order, V: Financial](p: UnitPartition[K, V]): Option[K] = {
      val VF = Fractional[V]; import VF._
      p.kvs.toNel.toList match {
        case (k, v) :: Nil if v === one => k.some
        case _                          => none
      }
    }
  }

  private def inRangeForallShares[K: Order, V: Financial](n: V, ps: Seq[(K, V)]) = {
    val V = Financial[V]
    ps forall { case (_, q) => V.zero <= q && q <= n * V.one }
  }

  private[deftrade] def unsafe[K: Order, V: Financial](kvs: SortedMap[K, V]): UnitPartition[K, V] =
    new UnitPartition((NonEmptyMap fromMap kvs).fold(???)(identity)) {}
}
