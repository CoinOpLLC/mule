package io.deftrade

import money._

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
import spire.syntax.field._

import scala.collection.immutable.SortedMap

trait PartitionLike[K, V] extends Any {

  def kvs: NonEmptyMap[K, V]

  final type KeyType   = K
  final type ValueType = V

  final def total(implicit V: Fractional[V]): ValueType = kvs reduce V.plus
  final def keys: NonEmptySet[K]                        = kvs.keys
  final def toSortedMap: SortedMap[K, V]                = kvs.toSortedMap

  final def proRate[N](n: N)(implicit K: Order[K], V: Financial[V], N: Financial[N]): Partition[K, N] =
    Partition unsafe (toSortedMap mapValues (v => n * V.to[N](v)))

  final def normalize(implicit K: Order[K], V: Financial[V]): UnitPartition[K, V] = {
    val _total = total
    UnitPartition unsafe [K, V] (toSortedMap mapValues (_ / _total))
  }

}

final case class Partition[K, V] private (val kvs: NonEmptyMap[K, V]) extends AnyVal with PartitionLike[K, V]

object Partition {

  def currified[K: Order, V: Financial](
      compute: NonEmptySet[K] => V => NonEmptyMap[K, V]
  )(
      keys: NonEmptySet[K]
  )(
      amount: V
  ) = compute(keys)(amount) |> apply[K, V]

  def waterfall[K: Order, V: Fractional](
      tranches: Map[K, V],
      equity: K
  ): V => Partition[K, V] = ???

  private[deftrade] def unsafe[K: Order, V: Financial](sm: SortedMap[K, V]): Partition[K, V] =
    Partition(NonEmptyMap fromMapUnsafe sm)

}

// TODO use refined to restrict V to be between 0 and 1
/** Guaranteed untitary and reasonable proportioning among several unique keys. */
final case class UnitPartition[K, V] private (val kvs: NonEmptyMap[K, V]) extends AnyVal with PartitionLike[K, V] {

  /** share acquired from each according to their proportion */
  def buyIn(key: K, share: V)(implicit K: Order[K], V: Financial[V]): Result[UnitPartition[K, V]] =
    if ((toSortedMap contains key) && share > V.fractional.zero)
      Partition.unsafe(toSortedMap + (key -> share)).normalize.asRight
    else
      fail(s"bad params: key=$key, share=$share")

  /** share returned to each according to their proportion */
  def buyOut(key: K)(implicit K: Order[K], V: Financial[V]): Result[UnitPartition[K, V]] = ???
}

/**
  */
object UnitPartition {

  /** `exact` slices is what you claim; we'll be the judge of that ;) */
  def exact[K: Order, V: Fractional](shares: (K, V)*): Result[UnitPartition[K, V]] = {
    val V                     = Fractional[V]
    def isUnitary             = V.one === shares.map(_._2).fold(V.zero)(_ + _)
    def zeroToOneForallShares = shares forall { case (_, q) => V.zero <= q && q <= V.one }
    def failmsg               = s"UnitPartition: invalid creation parms: $shares"
    if (isUnitary && zeroToOneForallShares)
      unsafe(SortedMap(shares: _*)).asRight
    else
      Fail(failmsg).asLeft
  }

  /** `n` shares issued; sum of slices must equal whole pie */
  def fromTotalShares[K: Order, V: Fractional](n: Long)(ps: (K, Long)*): Result[UnitPartition[K, V]] = {
    val V              = Fractional[V]
    val computedShares = ps.map(_._2).sum
    if (computedShares =!= n) {
      Fail(s"$computedShares != $n").asLeft
    } else {
      val vn = V fromLong n
      val shares = ps map {
        case (k, l) => (k -> (V fromLong l) / vn)
      }
      exact(shares: _*)
    }
  }

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: Order, V: Fractional](shares: (K, Long)*): Result[UnitPartition[K, V]] =
    fromTotalShares(shares.map(_._2).sum)(shares: _*)

  /** what every pizza slicer aims for */
  def fair[K: Order, V: Fractional](ks: NonEmptySet[K]): UnitPartition[K, V] = {
    val V        = Fractional[V]
    val oneSlice = V.one / (V fromLong ks.size)
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

  /** whole pie for me */
  def single[K: Order, V: Fractional](k: K): UnitPartition[K, V] =
    unsafe(SortedMap(k -> Fractional[V].one))

  object Single {

    def unapply[K: Order, V: Fractional](p: UnitPartition[K, V]): Option[K] = {
      val V = Fractional[V]
      p.kvs.toNel.toList match {
        case (k, v) :: Nil if v === V.one => k.some
        case _                            => none
      }
    }
  }

  private[deftrade] def unsafe[K: Order, V: Fractional](kvs: SortedMap[K, V]): UnitPartition[K, V] =
    new UnitPartition((NonEmptyMap fromMap kvs).fold(???)(identity))
}
