package io.deftrade

import cats._
import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
// import refined.api.Refined
// import refined.numeric.Positive
import refined.auto._

import spire.math.Fractional
// n.b. the intentionally narrow import!
// point being: this is how `spire.math.Fractional` and `cats.Order` can co-exist
// (before grand unification)
import spire.syntax.field._

import scala.collection.immutable.SortedMap

// TODO use refined to restrict V to be between 0 and 1
/** Guaranteed untitary and reasonable proportioning among several unique keys. */
final case class Partition[K, V] private (val kvs: NonEmptyMap[K, V]) extends AnyVal {
  def keys: NonEmptySet[K] = kvs.keys

  def proRata[N](n: N)(implicit V: Fractional[V], N: Fractional[N]): NonEmptyMap[K, N] = ???

}

/**
  * we will explain with the pizza metaphore
  * TODO: see if
  * type Pos[N] = N Refined Positive
  * is a help... maybe to return, not to accept (Postel's rule again)
  */
object Partition {

  type PositiveLong = Long // Refined Positive // FIXME do this

  /** `exact` slices is what you claim; we'll be the judge of that ;) */
  def exact[K: Order, V: Fractional](shares: (K, V)*): Result[Partition[K, V]] = {
    val V                     = Fractional[V]
    def isUnitary             = V.one === shares.map(_._2).fold(V.zero)(_ + _)
    def zeroToOneForallShares = shares forall { case (_, q) => V.zero <= q && q <= V.one }
    def failmsg               = s"Partition: invalid creation parms: $shares"
    if (isUnitary && zeroToOneForallShares)
      unsafe(SortedMap(shares: _*)).asRight
    else
      Fail(failmsg).asLeft
  }

  /** `n` shares issued; sum of slices must equal whole pie */
  def fromTotalShares[K: Order, V: Fractional](n: Long)(ps: (K, Long)*): Result[Partition[K, V]] = {
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
  def fromShares[K: Order, V: Fractional](shares: (K, Long)*): Result[Partition[K, V]] =
    fromTotalShares(shares.map(_._2).sum)(shares: _*)

  /** what every pizza slicer aims for */
  def fair[K: Order, V: Fractional](ks: NonEmptySet[K]): Partition[K, V] = {
    val V        = Fractional[V]
    val oneSlice = V.one / (V fromLong ks.size)
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

  /** whole pie for me */
  def single[K: Order, V: Fractional](k: K): Partition[K, V] =
    unsafe(SortedMap(k -> Fractional[V].one))

  def buyIn[K: Order, V: Fractional](in: K, slice: V): Result[Partition[K, V]] = ???

  def buyOut[K: Order, V: Fractional](in: K): Result[Partition[K, V]] = ???

  private def unsafe[K: Order, V: Fractional](kvs: SortedMap[K, V]): Partition[K, V] =
    new Partition((NonEmptyMap fromMap kvs).fold(???)(identity))

  object Single {

    def unapply[K: Order, V: Fractional](p: Partition[K, V]): Option[K] = {
      val V = Fractional[V]
      p.kvs.toNel.toList match {
        case (k, v) :: Nil if v === V.one => k.some
        case _                            => none
      }
    }
  }
}
