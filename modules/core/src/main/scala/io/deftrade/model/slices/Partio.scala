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
package model.slices

import syntax._, money._, refinements.IsUnitInterval, keyval.{ Fail, Result }

import cats.implicits._
import cats.{ Contravariant, Eq, Order, Show }
import cats.data.{ NonEmptyList, NonEmptyMap, NonEmptySet }
import cats.derived.{ auto, semiauto }

import spire.math.Fractional
import spire.syntax.field._
import spire.compat.numeric

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined }
import refined.cats._

import scala.collection.immutable.{ SortedMap, SortedSet }

/**
  * Models a total stake, and the stakeholders thereof.
  *
  * From the Latin
  * [[https://en.wiktionary.org/wiki/partio#Etymology_1  partio]]:
  * share, part, apportion; divide, distribute.
  */
/**  */
sealed abstract case class Partio[K: Order, N: Financial, Q] private (
    final val kvs: NonEmptyMap[K, N Refined Q]
) {

  import Partio.{ IsNormalized, IsPositive }

  final type Repr         = Partio[K, N, Q]
  final type ScaledPartio = Partio[K, N, IsPositive]
  final type UnitPartio   = Partio[K, N, IsNormalized]

  final type Qualified  = N Refined Q
  final type Normalized = N Refined IsNormalized
  final type Positive   = N Refined IsPositive

  final type Money[C] = Mny[N, C]

  /** */
  final def keys: NonEmptySet[K] = kvs.keys

  /**
    */
  final def toSortedMap: SortedMap[K, Qualified] =
    kvs.toSortedMap

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def scaled(n: Positive): ScaledPartio =
    Partio(kvs.map { v =>
      val Right(pv) = refineV[IsPositive](v.value * n.value) // how do we know? ;)
      pv
    })

  /** Creates a `Partio` of total `n`, proportional to self. `n` must be positive.
    */
  final def proRated(n: N): Result[ScaledPartio] =
    (for { pn <- refineV[IsPositive](n) } yield scaled(pn)) leftMap Fail.fromString

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def priced[C: Currency](amount: Money[C]): NonEmptyMap[K, Money[C]] =
    kvs map (amount * _.value)

  /**
    * Share acquired from each, according to their proportion,
    * to make room for the `share` requested by the new `key`.
    * FIXME finish
    *
    * If the `key` currently has a value assigned, it is effectively ignored, as the `share`
    * parameter dictates the final value proportion.
    */
  final def buyIn(key: K, share: Normalized): Result[Repr] = ???

  /** Share returned to each according to their proportion.
    * FIXME finish
    */
  final def sellOut(key: K): Result[Repr] = ???

  /**
    * Assign some portion of the equity from some party to another.
    *
    * FIXME finish
    */
  final def assigned(
      portion: Normalized,
      from: K,
      to: UnitPartio,
  ): Result[Repr] =
    if ((keys contains from) && (to.keys forall (k => !(keys contains k)))) {
      // this match {
      //   case Partio(_)     => ??? // FIXME Result of (Partio unsafe ???)
      //   case UnitPartio(_) => ??? // FIXME UnitPartio unsafe ???
      // }
      ???
    } else {
      Result fail """¯\_(ツ)_/¯"""
    }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def total(implicit IsQ: IsPositive =:= Q): Positive = {
    val Right(ret) = refineV[IsPositive](
      (kvs
        .map(_.value))
        .reduce
    ) // this is fine
    ret
  }

  /**  */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def normalized(implicit IsQ: IsPositive =:= Q): UnitPartio = {
    import refined.auto._
    UnitPartio unsafe (kvs map (_.value / total)).toSortedMap // this is fine
  }

  /** Partio only */
  final def retired(key: K): Result[Repr] = ???

  /** Partio only */
  final def diluted(key: K, share: N): Result[ScaledPartio] = ???

}

/** */
object Partio {

  final type IsPositive = refined.numeric.Positive

  final type IsNormalized = IsUnitInterval.`(0,1]`

  /** */
  private[model] def apply[K: Order, N: Financial, Q](
      kvs: NonEmptyMap[K, N Refined Q]
  ): Partio[K, N, Q] =
    new Partio(kvs) {}

  private[deftrade] def unsafe[K: Order, N: Financial](
      sm: SortedMap[K, N Refined IsPositive]
  ): ScaledPartio[K, N] = Partio(NonEmptyMap fromMapUnsafe sm)

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: Order, N: Financial](
      share: (K, N Refined IsPositive),
      shares: (K, N Refined IsPositive)*
  ): ScaledPartio[K, N] = {

    val kvs: NonEmptyMap[K, N Refined IsPositive] = NonEmptyList(share, shares.toList).toNem
    ScaledPartio(kvs)
  }

  /**
    * Creates an allocation function (`N => Partio`) from a set of tranches,
    * with a designated key for the remainder (equity).
    */
  def waterfall[K: Order, N: Financial](
      tranches: Map[K, N],
      equity: K
  ): Result[N => ScaledPartio[K, N]] = ???

  /** */
  def waterfall[K: Order, N: Financial](
      tranches: K => N Refined IsPositive,
  ): N => ScaledPartio[K, N] = ???

  /** def ipsa loquitur */
  def currified[K: Order, N: Financial](
      compute: NonEmptySet[K] => N Refined IsPositive => NonEmptyMap[K, N Refined IsPositive]
  )(
      keys: NonEmptySet[K]
  )(
      amount: N Refined IsPositive
  ) = compute(keys)(amount) |> apply[K, N, IsPositive]

  implicit def partitionEq[K: Order, N: Financial, Q]: Eq[Partio[K, N, Q]] = {
    import auto.eq._; semiauto.eq
  }

  implicit def partitionShow[K: Show, N: Show, Q]: Show[Partio[K, N, Q]] =
    Contravariant[Show].contramap(Show[NonEmptyMap[K, N Refined Q]])(_.kvs)
}

object ScaledPartio {

  protected final type Positive[N] = N Refined Partio.IsPositive

  /**
    */
  def apply[K: Order, N: Financial](
      kvs: NonEmptyMap[K, Positive[N]]
  ): ScaledPartio[K, N] =
    Partio(kvs)
}

/** Conventional creation patterns. */
object UnitPartio {

  import Partio.{ IsNormalized, IsPositive }

  /** Whole pie for me. */
  def single[K: Order, N: Financial](k: K): UnitPartio[K, N] =
    unsafe(SortedMap(k -> Financial[N].one))

  /** What every pizza slicer aims for. */
  def fair[K: Order, N: Financial](ks: K*): Result[UnitPartio[K, N]] =
    Result(for {
      k <- ks.headOption if SortedSet(ks: _*).size === ks.size
    } yield fairExact(NonEmptySet(k, SortedSet((ks drop 1): _*))))

  /** */
  def fairExact[K: Order, N: Financial](ks: NonEmptySet[K]): UnitPartio[K, N] = {
    val N        = Fractional[N]; import N._
    val oneSlice = one / (N fromLong ks.size)
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

  /** Total shares outstanding computed from the sum of `shares`. */
  def fromShares[K: Order, N: Financial](
      shares: (K, N Refined IsPositive)*
  ): Result[UnitPartio[K, N]] =
    shares.toList match {
      case Nil    => Fail("wut").asLeft
      case h :: t => Result safe { Partio.fromShares(h, t: _*).normalized }
    }

  /** `n` shares issued; sum of slices must equal whole pie */
  def fromTotalShares[K: Order, N: Financial](
      n: N Refined IsPositive
  )(
      ps: (K, N Refined IsPositive)*
  ): Result[UnitPartio[K, N]] = {
    import refined.auto._
    val N              = Fractional[N]; import N._
    val computedShares = ps.map(_._2.value).fold(zero)(plus)
    if (computedShares === n) fromShares(ps: _*)
    else Result fail s"${computedShares.toString} != ${n.toString}"
  }

  /** `exact` slices are claimed by the caller; this is checked. */
  def exact[K: Order, N: Financial](shares: (K, N)*): Result[UnitPartio[K, N]] = {
    val VF = Fractional[N]; import VF._
    shares.map(_._2).fold(zero)(plus) match {
      case x if one === x => Result of unsafe(SortedMap(shares: _*))
      case noUnity =>
        Result fail s"UnitPartio: ${shares.toString} total ${noUnity.toString}"
    }
  }

  /** For the extractor. */
  object Single {

    /** Extracts a single key, if that's what's there. */
    def unapply[K: Order, N: Financial](p: UnitPartio[K, N]): Option[K] =
      p.kvs.toNel.toList match {
        case (k, _) :: Nil => k.some
        case _             => none
      }
  }

  /** checks only that values are normalized */
  private[deftrade] def unsafe[K: Order, N: Financial](kvs: SortedMap[K, N]): UnitPartio[K, N] =
    Partio(NonEmptyMap fromMapUnsafe (for {
      (k, v) <- kvs
    } yield {
      val Right(nv) = refineV[IsNormalized](v) // this is fine
      k -> nv
    }))
}
