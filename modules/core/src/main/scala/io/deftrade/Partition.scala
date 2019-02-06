package io.deftrade

import cats._
import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

// import refined.{ cats => refinedCats, _ }
import spire.math.Fractional

import scala.collection.immutable.{ SortedMap }

/** Guaranteed untitary and reasonable proportioning among several unique keys. */
final case class Partition[K, V] private (val kvs: NonEmptyMap[K, V]) extends AnyVal {
  def keys: NonEmptySet[K] = kvs.keys
}

object Partition {

  private def unsafe[K: Order, V: Fractional](kvs: SortedMap[K, V]): Partition[K, V] =
    new Partition((NonEmptyMap fromMap kvs).fold(???)(identity))

  def exact[K: Order, V: Fractional](shares: (K, V)*): Result[Partition[K, V]] = {
    val V = Fractional[V]
    import V._
    def isUnitary    = one == shares.map(_._2).fold(zero)(plus(_, _))
    def isReasonable = shares forall { case (_, q) => zero <= q && q <= one }
    def failmsg      = s"Partition: invalid creation parms: $shares"
    if (isUnitary && isReasonable) unsafe(SortedMap(shares: _*)).asRight else Fail(failmsg).asLeft
  }

  def fromShares[K: Order, V: Fractional](n: Long)(ps: (K, Long)*): Result[Partition[K, V]] = {
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

  def proRata[K: Order, V: Fractional](shares: (K, Long)*): Result[Partition[K, V]] =
    fromShares(shares.map(_._2).sum)(shares: _*)

  def pariPassu[K: Order, V: Fractional](ks: NonEmptySet[K]): Partition[K, V] = {
    val V = Fractional[V]
    import V._
    val oneSlice = div(one, fromLong(ks.size))
    val slices   = SortedMap(ks.toList.map(_ -> oneSlice): _*)
    unsafe(slices)
  }

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
}
