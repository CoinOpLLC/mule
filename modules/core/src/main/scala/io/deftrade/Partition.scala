package io.deftrade

import spire.math.Fractional

import cats._
import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

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

  import eu.timepit.refined
  import refined.api.Refined
  import refined.numeric.Positive
  import refined.auto._

  type PositiveLong = Long // Refined Positive // FIXME do this

  /** `exact` slices is what you say; we'll be the judge of that ;) */
  def exact[K: Order, V: Fractional](shares: (K, V)*): Result[Partition[K, V]] = {
    val V = Fractional[V]
    import V._
    def isUnitary    = one == shares.map(_._2).fold(zero)(plus(_, _))
    def isReasonable = shares forall { case (_, q) => zero <= q && q <= one }
    def failmsg      = s"Partition: invalid creation parms: $shares"
    if (isUnitary && isReasonable) unsafe(SortedMap(shares: _*)).asRight else Fail(failmsg).asLeft
  }

  /** `n` shares issued; sum of slices must equal whole pie */
  def fromTotalShares[K: Order, V: Fractional](n: Long)(ps: (K, Long)*): Result[Partition[K, V]] = {
    val V = Fractional[V]
    import V._
    val computedShares = ps.map(_._2).sum
    if (computedShares =!= n) {
      io.deftrade.Fail(s"$computedShares != $n").asLeft
    } else {
      val shares = ps map {
        case (k, l) => (k, div(fromLong(l), fromLong(n)))
      }
      exact(shares: _*)
    }
  }

  /** total shares outstanding computed from the sum of `shares` */
  def fromShares[K: Order, V: Fractional](shares: (K, Long)*): Result[Partition[K, V]] =
    fromTotalShares(shares.map(_._2).sum)(shares: _*)

  /** what every pizza slicer aims for */
  def fair[K: Order, V: Fractional](ks: NonEmptySet[K]): Partition[K, V] = {
    val V = Fractional[V]
    import V._
    val oneSlice = div(one, fromLong(ks.size))
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

  def buyIn[K: Order, V: Fractional](in: K, slice: V): Result[Partition[K, V]] = ???

  def buyOut[K: Order, V: Fractional](in: K): Result[Partition[K, V]] = ???

  /** whole pie for me */
  def single[K: Order, V: Fractional](k: K): Partition[K, V] =
    unsafe(SortedMap(k -> Fractional[V].one))

  object Single {

    def unapply[K: Order, V: Fractional](p: Partition[K, V]): Option[K] = {
      val V = Fractional[V]
      import V._
      p.kvs.toNel.toList match {
        case (k, v) :: Nil if v === one => k.some
        case _                          => none
      }
    }
  }
  private def unsafe[K: Order, V: Fractional](kvs: SortedMap[K, V]): Partition[K, V] =
    new Partition((NonEmptyMap fromMap kvs).fold(???)(identity))

}
