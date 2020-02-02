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

import implicits._, money._, refinements.IsUnitInterval

import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

import spire.math.Fractional
import spire.syntax.field._
import spire.compat.numeric

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined, Validate }
import refined.numeric._
import refined.auto._

import scala.collection.immutable.SortedMap

/** */
object PartitionLike {

  /** */
  final type IsNormalized = IsUnitInterval.`(0,1]`

  /** */
  final type IsPositive = refined.numeric.Positive
}

import PartitionLike._

/**
  * Models a total stake, and the stakeholders thereof.
  *
  * (A pot, divided.)
  */
sealed trait PartitionLike {

  /** self type */
  type Repr <: PartitionLike

  /** */
  type Key

  /** */
  implicit val K: cats.Order[Key]

  /** */
  type Value

  /** */
  implicit val V: Financial[Value]

  /** */
  type RefinedValue <: Refined[Value, _]

  /** */
  val kvs: NonEmptyMap[Key, RefinedValue]

  /** */
  final def keys: NonEmptySet[Key] = kvs.keys

  /** */
  final def toSortedMap: SortedMap[Key, Value] = kvs.toSortedMap map {
    case (k, vrp) => (k, vrp.value)
  }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def scaled(
      n: Value Refined IsPositive
  ): Partition[Key, Value] =
    Partition apply kvs.map { v =>
      val Right(pv) = refineV[IsPositive](v.value * n.value) // how do we know? ;)
      pv
    }

  /** Creates a `Partition` of total `n`, proportional to self. `n` must be positive. */
  final def proRated(
      n: Value
  ): Result[Partition[Key, Value]] =
    (for {
      pn <- refineV[IsPositive](n)
    } yield normalized scaled pn) leftMap Fail.fromString

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def priced[C: Currency](
      amount: Money[Value, C]
  ): Map[Key, Money[Value, C]] = (kvs map (amount * _.value)).toSortedMap

  /** */
  def total: RefinedValue

  /** Creates a [[UnitPartition]] from this one. */
  def normalized: UnitPartition[Key, Value]

  /**
    * Share acquired from each, according to their proportion,
    * to make room for the `share` requested by the new `key`.
    *
    * If the `key` currently has a value assigned, it is effectively ignored, as the `share`
    * parameter dictates the final value proportion.
    */
  final def buyIn(key: Key, share: Value Refined IsNormalized): Result[Repr] = ???
  // this match {
  //   case Partition(_)     => ???
  //   case UnitPartition(_) => ???
  // }

  /** share returned to each according to their proportion */
  final def sellOut(key: Key): Repr = ???

  /**
    *
    */
  final def assigned(
      from: Key,
      portion: Value Refined IsNormalized,
      to: UnitPartition[Key, Value]
  ): Result[Repr] =
    if ((keys contains from) && (to.keys forall (k => !(keys contains k)))) {
      this match {
        case Partition(_)     => ??? // FIXME Result of (Partition unsafe ???)
        case UnitPartition(_) => ??? // FIXME UnitPartition unsafe ???
      }
    } else {
      Result fail """¯\_(ツ)_/¯"""
    }
}

/**  */
sealed abstract case class Partition[K, V] private (
    final override val kvs: NonEmptyMap[K, V Refined IsPositive]
)(
    implicit
    final override val K: cats.Order[K],
    final override val V: Financial[V]
) extends PartitionLike {
  final type Key          = K
  final type Value        = V
  final type RefinedValue = Value Refined IsPositive
  final type Repr         = Partition[Key, Value]

  import V._

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def total: RefinedValue =
    kvs.reduce // yyep. :|

  /**  */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def normalized: UnitPartition[Key, Value] =
    UnitPartition unsafe (kvs map (_.value / total)).toSortedMap // because (posN / posN) is posN

  /** Partition only */
  final def retired(key: Key): Result[Repr] = ???

  /** Partition only */
  final def diluted(key: Key, share: Value): Result[Partition[Key, Value]] = ???

}

/** */
object Partition {

  /** */
  private[model] def apply[K: cats.Order, V: Financial](
      kvs: NonEmptyMap[K, V Refined IsPositive]
  ) =
    new Partition(kvs) {}

  private[deftrade] def unsafe[K: cats.Order, V: Financial](
      sm: SortedMap[K, V Refined IsPositive]
  ): Partition[K, V] = Partition(NonEmptyMap fromMapUnsafe sm)

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: cats.Order, V: Financial](
      shares: (K, V)*
  ): Result[Partition[K, V]] = shares.toList match {
    case Nil => Result fail "must be non-empty"
    case shareNel =>
      implicit def HACK: Validate[V, IsPositive] = ???
      val posSharesNelE: List[Either[String, (K, V Refined IsPositive)]] = shareNel map {
        case (k, v) => refineV[IsPositive](v) map (v => (k, v))
      }
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      val eNelPosShares: Either[String, List[(K, V Refined IsPositive)]] = posSharesNelE.sequence
      val ep: Either[String, Partition[K, V]] = for {
        kvs <- eNelPosShares
      } yield unsafe(SortedMap(kvs.toList: _*))
      ep leftMap Fail.fromString
  }

  /**
    * Creates an allocation function (`V => Partition`) from a set of tranches,
    * with a designated key for the remainder (equity).
    */
  def waterfall[K: cats.Order, V: Financial](
      tranches: Map[K, V],
      equity: K
  ): Result[V => Partition[K, V]] = ???

  /** */
  def waterfall[K: cats.Order, V: Financial](
      tranches: K => V Refined IsPositive,
  ): V => Partition[K, V] = ???

  /** def ipsa loquitur */
  def currified[K: cats.Order, V: Financial](
      compute: NonEmptySet[K] => V Refined IsPositive => NonEmptyMap[K, V Refined IsPositive]
  )(
      keys: NonEmptySet[K]
  )(
      amount: V Refined IsPositive
  ) = compute(keys)(amount) |> apply[K, V]
}

/**
  * Models a sharable value as a single `Unit`.
  *
  * (Not to be confused with Scala Unit.)
  */
sealed abstract case class UnitPartition[K, V] private (
    final override val kvs: NonEmptyMap[K, V Refined IsUnitInterval.`(0,1]`]
)(
    implicit
    final override val K: cats.Order[K],
    final override val V: Financial[V]
) extends PartitionLike {

  final type Key          = K
  final type Value        = V
  final type RefinedValue = V Refined IsNormalized
  final type Repr         = UnitPartition[Key, Value]

  /** */
  final def total: RefinedValue = {
    val Right(normalOne) = refineV[IsNormalized](V.one)
    normalOne
  }

  /** */
  final def normalized: UnitPartition[Key, Value] = this
}

/** Conventional creation patterns. */
object UnitPartition {

  /** Whole pie for me. */
  def single[K: cats.Order, V: Financial](k: K): UnitPartition[K, V] =
    unsafe(SortedMap(k -> Financial[V].one))

  /** What every pizza slicer aims for; their's is a fine example. */
  def fair[K: cats.Order, V: Financial](ks: NonEmptySet[K]): UnitPartition[K, V] = {
    val V        = Fractional[V]; import V._
    val oneSlice = one / (V fromLong ks.size)
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

  /** Total shares outstanding computed from the sum of `shares`. */
  def fromShares[K: cats.Order, V: Financial](shares: (K, V)*): Result[UnitPartition[K, V]] =
    Partition.fromShares(shares: _*) map (_.normalized)

  /** `n` shares issued; sum of slices must equal whole pie */
  def fromTotalShares[K: cats.Order, V: Financial](
      n: V
  )(
      ps: (K, V)*
  ): Result[UnitPartition[K, V]] = {
    val V              = Fractional[V]; import V._
    val computedShares = ps.map(_._2).fold(zero)(plus)
    if (computedShares === n) fromShares(ps: _*)
    else Result fail s"${computedShares.toString} != ${n.toString}"
  }

  /** `exact` slices are claimed by the caller; this is checked. */
  def exact[K: cats.Order, V: Financial](shares: (K, V)*): Result[UnitPartition[K, V]] = {
    val VF = Fractional[V]; import VF._
    shares.map(_._2).fold(zero)(plus) match {
      case x if one === x => Result of unsafe(SortedMap(shares: _*))
      case noUnity =>
        Result fail s"UnitPartition: ${shares.toString} total ${noUnity.toString}"
    }
  }

  /** For the extractor. */
  object Single {

    /** Extracts a single key, if that's what's there. */
    def unapply[K: cats.Order, V: Financial](p: UnitPartition[K, V]): Option[K] =
      p.kvs.toNel.toList match {
        case (k, _) :: Nil => k.some
        case _             => none
      }
  }

  /** checks only that values are normalized */
  private[deftrade] def unsafe[
      K: cats.Order,
      V: Financial
  ](
      kvs: SortedMap[K, V]
  ): UnitPartition[K, V] =
    new UnitPartition(NonEmptyMap fromMapUnsafe (for {
      (k, v) <- kvs
    } yield {
      val Right(nv) = refineV[IsNormalized](v)
      k -> nv
    })) {}
}
