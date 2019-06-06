package io.deftrade

import cats._
import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
import refined.api.Refined
import refined.numeric.Positive
import refined.auto._

import spire.math.Fractional
// n.b. the intentionally narrow import!
// point being: this is how `spire.math.Fractional` and `cats.Order` can co-exist
// (before grand unification)
// FIXME: revisit this
import spire.syntax.field._

import scala.collection.immutable.SortedMap

private[deftrade] trait PartitionLike[K, V] extends Any { self =>

  def kvs: NonEmptyMap[K, V]

  final def total(implicit V: Fractional[V]): V = kvs reduce V.plus
  final def keys: NonEmptySet[K]                = kvs.keys
  final def toSortedMap: SortedMap[K, V]        = kvs.toSortedMap

  final def scaled(n: V)(implicit K: Order[K], V: Fractional[V]): Partition[K, V] =
    Partition unsafe (toSortedMap mapValues (_ * n))

}

final case class Partition[K, V] private (
    val kvs: NonEmptyMap[K, V]
) extends AnyVal
    with PartitionLike[K, V] {

  final def normalized(implicit K: Order[K], V: Fractional[V]): UnitPartition[K, V] = {
    val _total = total
    UnitPartition unsafe [K, V] (toSortedMap mapValues (_ / _total))
  }

  /** Creates a `Partition` of total `n`, proportional to self. */
  def proRated(n: V)(implicit K: Order[K], V: Fractional[V]): Partition[K, V] =
    normalized scaled n // TODO feels half assed

  /** share acquired from each according to their proportion */
  def dilutedBy(key: K, share: V)(implicit K: Order[K], V: Fractional[V]): Result[Partition[K, V]] =
    ???

  /** share returned to each according to their proportion */
  def retiredKey(key: K)(implicit K: Order[K], V: Fractional[V]): Result[Partition[K, V]] =
    ???

}

object Partition {

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: Order, V: Fractional](shares: (K, V)*): Result[Partition[K, V]] =
    if (shares.toList.nonEmpty) unsafe(shares |> SortedMap.apply[K, V]).asRight
    else fail("wtf scrooge you don't know how to share do you?")

  /**
    * Creates an allocation function (`V => Partition`) from a set of tranches,
    * with a designated key for the remainder (equity).
    */
  def waterfall[K: Order, V: Fractional](
      tranches: Map[K, V],
      equity: K
  ): V => Partition[K, V] = ???

  /** def ipsa loquitur */
  def currified[K: Order, V: Fractional](
      compute: NonEmptySet[K] => V => NonEmptyMap[K, V]
  )(
      keys: NonEmptySet[K]
  )(
      amount: V
  ) = compute(keys)(amount) |> apply[K, V]

  private[deftrade] def unsafe[K: Order, V: Fractional](sm: SortedMap[K, V]): Partition[K, V] =
    Partition(NonEmptyMap fromMapUnsafe sm)

}

// TODO use refined to restrict V to be between 0 and 1
/** Guaranteed untitary and reasonable proportioning among several unique keys. */
final case class UnitPartition[K, V] private (val kvs: NonEmptyMap[K, V]) extends AnyVal with PartitionLike[K, V] {

  /** share acquired from each according to their proportion */
  def buyIn(key: K, share: V)(implicit K: Order[K], V: Fractional[V]): Result[UnitPartition[K, V]] =
    if (!(toSortedMap contains key) && share > V.zero)
      Partition.unsafe(toSortedMap + (key -> share)).normalized.asRight
    else
      fail(s"bad params: key=$key, share=$share, kvs=$kvs")

  /** share returned to each according to their proportion */
  def sellOut(key: K)(implicit K: Order[K], V: Fractional[V]): Result[UnitPartition[K, V]] =
    UnitPartition.fromShares((toSortedMap - key).toList: _*)
}

/**
  */
object UnitPartition {

  /** Whole pie for me. */
  def single[K: Order, V: Fractional](k: K): UnitPartition[K, V] =
    unsafe(SortedMap(k -> Fractional[V].one))

  /** What every pizza slicer aims for; their's is a fine example. */
  def fair[K: Order, V: Fractional](ks: NonEmptySet[K]): UnitPartition[K, V] = {
    val V        = Fractional[V]
    val oneSlice = V.one / (V fromLong ks.size)
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: Order, V: Fractional](shares: (K, V)*): Result[UnitPartition[K, V]] =
    Partition.fromShares(shares: _*) map (_.normalized)

  /** `n` shares issued; sum of slices must equal whole pie */
  def fromTotalShares[K: Order, V: Fractional](n: V)(ps: (K, V)*): Result[UnitPartition[K, V]] = {
    val V = Fractional[V]
    import V._
    val computedShares = ps.map(_._2).fold(zero)(plus)
    if (computedShares === n && inRangeForallShares(n, ps)) {
      fromShares(ps: _*)
    } else {
      s"$computedShares != $n" |> fail
    }
  }

  /** `exact` slices are claimed by the caller; this is checked. */
  def exact[K: Order, V: Fractional](shares: (K, V)*): Result[UnitPartition[K, V]] = {
    val V = Fractional[V]
    import V._
    def isUnitary = one === shares.map(_._2).fold(zero)(plus)
    if (isUnitary && inRangeForallShares(one, shares))
      unsafe(SortedMap(shares: _*)).asRight
    else
      fail(s"UnitPartition: invalid creation parms: $shares")
  }

  object Single {

    def unapply[K: Order, V: Fractional](p: UnitPartition[K, V]): Option[K] =
      p.kvs.toNel.toList match {
        case (k, v) :: Nil if v === Fractional[V].one => k.some
        case _                                        => none
      }
  }

  private type BrokenScalaSeq[A] = scala.collection.Seq[A]
  private def inRangeForallShares[K: Order, V: Fractional](n: V, ps: BrokenScalaSeq[(K, V)]) = {
    val V = Fractional[V]
    ps forall { case (_, q) => V.zero <= q && q <= n * V.one }
  }

  private[deftrade] def unsafe[K: Order, V: Fractional](kvs: SortedMap[K, V]): UnitPartition[K, V] =
    new UnitPartition((NonEmptyMap fromMap kvs).fold(???)(identity))
}
