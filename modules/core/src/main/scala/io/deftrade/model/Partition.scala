package io.deftrade
package model

import money._

import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

import spire.math.Fractional
import spire.syntax.field._

import scala.collection.immutable.SortedMap

private[deftrade] trait PartitionLike[K, V] extends FinancialEntailsFractional {

  def kvs: NonEmptyMap[K, V]

  final def total(implicit V: Financial[V]): V = kvs reduce V.fractional.plus
  final def keys: NonEmptySet[K]               = kvs.keys
  final def toSortedMap: SortedMap[K, V]       = kvs.toSortedMap

  final def scaled(n: V)(implicit K: cats.Order[K], V: Financial[V]): Partition[K, V] =
    Partition unsafe (toSortedMap mapValues (_ * n))

}

/** Modelling the total equity stake, and the stakeholders thereof. */
sealed abstract case class Partition[K, V] private (
    val kvs: NonEmptyMap[K, V]
) extends PartitionLike[K, V] {

  final def normalized(implicit K: cats.Order[K], V: Financial[V]): UnitPartition[K, V] = {
    val _total = total
    UnitPartition unsafe [K, V] (toSortedMap mapValues (_ / _total))
  }

  /** Creates a `Partition` of total `n`, proportional to self. */
  def proRated(n: V)(implicit K: cats.Order[K], V: Financial[V]): Partition[K, V] =
    normalized scaled n // TODO feels half assed

  /** share acquired from each according to their proportion */
  def dilutedBy(key: K, share: V)(implicit K: cats.Order[K], V: Financial[V]): Result[Partition[K, V]] =
    ???

  /** share returned to each according to their proportion */
  def retiredKey(key: K)(implicit K: cats.Order[K], V: Financial[V]): Result[Partition[K, V]] =
    ???

}

object Partition extends FinancialEntailsFractional {

  private def apply[K: cats.Order, V: Financial](kvs: NonEmptyMap[K, V]) = new Partition(kvs) {}

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: cats.Order, V: Financial](shares: (K, V)*): Result[Partition[K, V]] =
    if (shares.toList.nonEmpty) unsafe(shares |> SortedMap.apply[K, V]).asRight
    else fail("must be non-empty")

  /**
    * Creates an allocation function (`V => Partition`) from a set of tranches,
    * with a designated key for the remainder (equity).
    */
  def waterfall[K: cats.Order, V: Financial](
      tranches: Map[K, V],
      equity: K
  ): V => Partition[K, V] = ???

  /** def ipsa loquitur */
  def currified[K: cats.Order, V: Financial](
      compute: NonEmptySet[K] => V => NonEmptyMap[K, V]
  )(
      keys: NonEmptySet[K]
  )(
      amount: V
  ) = compute(keys)(amount) |> apply[K, V]

  private[deftrade] def unsafe[K: cats.Order, V: Financial](sm: SortedMap[K, V]): Partition[K, V] =
    Partition(NonEmptyMap fromMapUnsafe sm)

}

/**
  * Models the equity of an entity as a single Unit.
  *
  * (Not to be confused with Scala Unit.)
  * TODO use refined to restrict V to be between 0 and 1
  */
sealed abstract case class UnitPartition[K, V] private (
    val kvs: NonEmptyMap[K, V]
) extends PartitionLike[K, V] {

  /** share acquired from each according to their proportion */
  def buyIn(key: K, share: V)(implicit K: cats.Order[K], V: Financial[V]): Result[UnitPartition[K, V]] =
    if (!(toSortedMap contains key) && share > V.fractional.zero)
      Partition.unsafe(toSortedMap + (key -> share)).normalized.asRight
    else
      fail(s"bad params: key=$key, share=$share, kvs=$kvs")

  /** share returned to each according to their proportion */
  def sellOut(key: K)(implicit K: cats.Order[K], V: Financial[V]): Result[UnitPartition[K, V]] =
    UnitPartition.fromShares((toSortedMap - key).toList: _*)
}

/**
  */
object UnitPartition extends FinancialEntailsFractional {

  /** Whole pie for me. */
  def single[K: cats.Order, V: Financial](k: K): UnitPartition[K, V] =
    unsafe(SortedMap(k -> Financial[V].fractional.one))

  /** What every pizza slicer aims for; their's is a fine example. */
  def fair[K: cats.Order, V: Financial](ks: NonEmptySet[K]): UnitPartition[K, V] = {
    val VF       = Financial[V].fractional; import VF._
    val oneSlice = one / (VF fromLong ks.size)
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

  /** Total shares outstanding computed from the sum of `shares`. */
  def fromShares[K: cats.Order, V: Financial](shares: (K, V)*): Result[UnitPartition[K, V]] =
    Partition.fromShares(shares: _*) map (_.normalized)

  /** `n` shares issued; sum of slices must equal whole pie */
  def fromTotalShares[K: cats.Order, V: Financial](n: V)(ps: (K, V)*): Result[UnitPartition[K, V]] = {
    val VF             = Financial[V].fractional; import VF._
    val computedShares = ps.map(_._2).fold(zero)(plus)
    if (computedShares === n && inRangeForallShares(n, ps)) {
      fromShares(ps: _*)
    } else {
      s"$computedShares != $n" |> fail
    }
  }

  /** `exact` slices are claimed by the caller; this is checked. */
  def exact[K: cats.Order, V: Financial](shares: (K, V)*): Result[UnitPartition[K, V]] = {
    val VF        = Fractional[V]; import VF._
    def isUnitary = one === shares.map(_._2).fold(zero)(plus)
    if (isUnitary && inRangeForallShares(one, shares))
      unsafe(SortedMap(shares: _*)).asRight
    else
      fail(s"UnitPartition: invalid creation parms: $shares")
  }

  object Single {

    def unapply[K: cats.Order, V: Financial](p: UnitPartition[K, V]): Option[K] = {
      val VF = Fractional[V]; import VF._
      p.kvs.toNel.toList match {
        case (k, v) :: Nil if v === one => k.some
        case _                          => none
      }
    }
  }

  private type BrokenScalaSeq[A] = scala.collection.Seq[A]
  private def inRangeForallShares[K: cats.Order, V: Financial](n: V, ps: BrokenScalaSeq[(K, V)]) = {
    val V = Financial[V]
    ps forall { case (_, q) => V.fractional.zero <= q && q <= n * V.fractional.one }
  }

  private[deftrade] def unsafe[K: cats.Order, V: Financial](kvs: SortedMap[K, V]): UnitPartition[K, V] =
    new UnitPartition((NonEmptyMap fromMap kvs).fold(???)(identity)) {}
}
