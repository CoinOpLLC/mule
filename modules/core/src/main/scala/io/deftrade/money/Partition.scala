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
import refined.api.{ Refined, Validate }
import refined.numeric._
import refined.auto._

import scala.collection.immutable.SortedMap

/** A pot divided. (Or something like it.) */
sealed trait PartitionLike {

  protected implicit def HACK: Validate[Value, Positive] = ???

  /** self type */
  type Repr <: PartitionLike

  /** */
  type Key

  /** */
  implicit val K: Order[Key]

  /** */
  type Value

  /** */
  implicit val V: Financial[Value]

  /** */
  final type PositiveValue = Value Refined Positive

  /** */
  final type NormalizedValue = Value Refined Financial.IsUnitInterval.`(0,1]`

  /** */
  type RefinedValue <: Refined[Value, _]

  /** */
  val kvs: NonEmptyMap[Key, RefinedValue]

  // TODO: "do the math" within the `Refined` type.
  // use the `refined.cats` forces to get a Semigroup for RefinedValues sumN
  def total: RefinedValue

  /** */
  // final def total(implicit V: Financial[Value]): PositiveValue =
  //   kvs reduce V.positiveSemigroup.combine
  //
  // final def total2(implicit V: Financial[Value]): NormalizedValue = V.one

  /** */
  final def keys: NonEmptySet[Key] = kvs.keys

  /** */
  final def toSortedMap: SortedMap[Key, Value] = kvs.toSortedMap map {
    case (k, vrp) => (k, vrp.value)
  }

  /** */
  final def scaled(
      n: PositiveValue
  ): Partition[Key, Value] =
    Partition unsafe kvs.toSortedMap.mapValues { _ =>
      val Right(res) = refineV[Positive](n.value) // (v.value * n.value)
      res
    }

  /** Creates a `Partition` of total `n`, proportional to self. `n` must be positive. */
  def proRated(
      n: Value
  ): Result[Partition[Key, Value]] = ???
  // for {
  //   pn <- refineV[Positive](n)
  // } yield normalized scaled pn // TODO feels half assed

  /** */
  final def priced[C: Currency](
      amount: Money[Value, C]
  ): Map[Key, Money[Value, C]] = kvs.toSortedMap mapValues (amount * _.value)

  /** Creates a [[UnitPartition]] from this one. */
  def normalized: UnitPartition[Key, Value] = ???
  // UnitPartition unsafe [Key, Value] (toSortedMap mapValues (_ / total))
  // this

  /**
    * Share acquired from each, according to their proportion,
    * to make room for the `share` requested by the new `key`.
    */
  def dilutedBy(key: Key, share: Value): Result[Repr] = ???

  /** share returned to each according to their proportion */
  def retiredKey(key: Key): Result[Repr] = ???

  // if (!(toSortedMap contains key) && refineV[Positive](share).isRight)
  //   Partition.unsafe(toSortedMap + (key -> share)).normalized.asRight
  // else
  //   Result fail s"bad params: key=$key, share=$share, kvs=$kvs"

  // UnitPartition.fromShares((toSortedMap - key).toList: _*)
}

/** Modelling the total equity stake, and the stakeholders thereof. */
sealed abstract case class Partition[K, V] private (
    override final val kvs: NonEmptyMap[K, V Refined Positive]
)(
    implicit
    final override val K: Order[K],
    final override val V: Financial[V]
) extends PartitionLike {
  final type Key          = K
  final type Value        = V
  final type RefinedValue = PositiveValue
  final type Repr         = Partition[Key, Value]

  // import V._
  // import refined.cats._
  def total: PositiveValue = ??? //cats.Semigroup[PositiveValue].combineN(kvs)
}

/** */
object Partition {

  /** */
  private def apply[K: Order, V: Financial](kvs: NonEmptyMap[K, V Refined Positive]) =
    new Partition(kvs) {}

  private[deftrade] def unsafe[K: Order, V: Financial](
      sm: SortedMap[K, V Refined Positive]
  ): Partition[K, V] = Partition(NonEmptyMap fromMapUnsafe sm)

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: Order, V: Financial](
      shares: (K, V)*
  ): Result[Partition[K, V]] = shares.toList match {
    case Nil => Result fail "must be non-empty"
    case shareNel =>
      implicit def HACK: Validate[V, Positive] = ???
      val posSharesNelE: List[Either[String, (K, V Refined Positive)]] = shareNel map {
        case (k, v) => refineV[Positive](v) map (v => (k, v))
      }
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      val eNelPosShares: Either[String, List[(K, V Refined Positive)]] = posSharesNelE.sequence
      val ep: Either[String, Partition[K, V]] = for {
        kvs <- eNelPosShares
      } yield unsafe(SortedMap(kvs.toList: _*))
      ep leftMap Fail.fromString
  }

  /**
    * Creates an allocation function (`V => Partition`) from a set of tranches,
    * with a designated key for the remainder (equity).
    */
  def waterfall[K: Order, V: Financial](
      tranches: Map[K, V],
      equity: K
  ): Result[V => Partition[K, V]] = ???

  /** */
  def waterfall[K: Order, V: Financial](
      tranches: K => V Refined Positive,
  ): V => Partition[K, V] = ???

  /** def ipsa loquitur */
  def currified[K: Order, V: Financial](
      compute: NonEmptySet[K] => V Refined Positive => NonEmptyMap[K, V Refined Positive]
  )(
      keys: NonEmptySet[K]
  )(
      amount: V Refined Positive
  ) = compute(keys)(amount) |> apply[K, V]
}

/**
  * Models a sharable value as a single `Unit`.
  *
  * (Not to be confused with Scala Unit.)
  */
sealed abstract case class UnitPartition[K, V] private (
    final override val kvs: NonEmptyMap[K, V Refined Financial.IsUnitInterval.`(0,1]`]
)(
    implicit
    final override val K: Order[K],
    final override val V: Financial[V]
) extends PartitionLike {

  final type Key          = K
  final type Value        = V
  final type RefinedValue = NormalizedValue
  final type Repr         = UnitPartition[Key, Value]
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
    def unapply[K: Order, V: Financial](p: UnitPartition[K, V]): Option[K] =
      p.kvs.toNel.toList match {
        case (k, _) :: Nil => k.some
        case _             => none
      }
  }

  private def inRangeForallShares[K: Order, V: Financial](n: V, ps: Seq[(K, V)]) = {
    val V = Financial[V]
    ps forall { case (_, q) => V.zero <= q && q <= n * V.one }
  }

  private[deftrade] def unsafe[K: Order, V: Financial](kvs: SortedMap[K, V]): UnitPartition[K, V] =
    ??? //new UnitPartition((NonEmptyMap fromMap kvs).fold(???)(identity)) {}
}
