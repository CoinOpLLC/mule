package io.deftrade
package model

import opaqueid.OpaqueId

import io.deftrade.time._
import io.deftrade.time.implicits._

import cats._
import cats.kernel.CommutativeGroup
// import cats.implicits._

import spire.math.{ Integral, Interval }
import spire.math.interval._

import scala.language.higherKinds

sealed trait Fresh[I] {
  def init: I
  def next(id: I): I
}
object Fresh {

  def apply[K: Integral, P]: Fresh[OpaqueId[K, P]] = freshK

  implicit def order[I: Integral]: cats.Order[I] = cats.Order fromOrdering Integral[I].toOrdering

  implicit def commutativeGroup[I: Integral]: CommutativeGroup[I] = Integral[I].additive

  implicit def fresh[K, P]: Fresh[K] = ??? // FIXME: enum case

  implicit def freshK[K: Integral, P]: Fresh[OpaqueId[K, P]] = new Fresh[OpaqueId[K, P]] {
    type Id = OpaqueId[K, P]
    private lazy val K = Integral[K]
    def init: Id       = OpaqueId(K.zero)
    def next(id: Id)   = OpaqueId(K.plus(id.id, K.one))
  }

}

trait RepoApi {
  //////////////////////////////////////////////////////////////////////////////////////////////////
  //  Repository
  //////////////////////////////////////////////////////////////////////////////////////////////////

  abstract class Repository[IO[_]: Monad, K: cats.Order: Fresh, V: Eq] {

    lazy val IO = Monad[IO]
    lazy val K  = cats.Order[K]
    lazy val V  = cats.Eq[V]

    type Row   = (K, V)
    type Rows  = List[Row]
    type Table = Map[K, V]

    /** Simple Queries */
    final def empty: IO[Table] = IO pure { Map.empty }
    def rows: IO[Rows]
    def table: IO[Table]
    def get(id: K): IO[Option[V]]

    def insert(v: V): IO[Result[K]]
    def update(row: Row): IO[Result[Unit]]
    // def delete(id: K): IO[Result[Unit]]
  }

  trait RepositoryMemImpl[IO[_], K, V] { self: Repository[IO, K, V] =>
    private var kvs: Table        = Map.empty
    def rows: IO[Rows]            = IO pure kvs.toList
    def table: IO[Table]          = IO pure kvs
    def get(id: K): IO[Option[V]] = IO pure { kvs get id }

    /** mutators record timestamp FIXME: do they now? */
    def insert(v: V): IO[Result[K]] = IO pure {
      Result {
        def k: K = ??? // FIXME: `Fresh`
        kvs = kvs + (k -> v)
        k
      }
    }
    def update(row: Row): IO[Result[Unit]] = IO pure {
      Result { kvs = kvs + row }
    }

  }

  class SimpleRepository[IO[_]: Monad, K: cats.Order: Fresh, V: Eq] extends Repository[IO, K, V] with RepositoryMemImpl[IO, K, V]

  abstract class PointInTimeRepository[IO[_]: Monad, K: cats.Order: Fresh, V: Eq] extends Repository[IO, K, V] {

    type LocalDateTimeRange = Interval[LocalDateTime]
    object LocalDateTimeRange {
      def all: LocalDateTimeRange = Interval.all[LocalDateTime]
    }

    type PitTable = Map[K, List[(LocalDateTimeRange, V)]]
    type PitRow   = (K, (LocalDateTimeRange, V))
    type PitRows  = List[PitRow]

    /** Simple Queries always take from the current data stored in the `Table` */
    final override def rows: IO[Rows]            = rowsBetween(LocalDateTimeRange.all)
    final override def table: IO[Table]          = tableAt(localDateTime)
    final override def get(id: K): IO[Option[V]] = getAt(localDateTime)(id)

    /** Point In Time Queries */
    def tableAt(pit: LocalDateTime): IO[Table]
    def rowsBetween(range: LocalDateTimeRange): IO[Rows]
    def getAt(pit: LocalDateTime)(id: K): IO[Option[V]]
    def getBetween(range: LocalDateTimeRange)(id: K): IO[Rows]

    final override def insert(v: V): IO[Result[K]] = insert(localDateTime)(v)
    def insert(pit: LocalDateTime)(v: V): IO[Result[K]]

    final override def update(row: Row): IO[Result[Unit]] = update(localDateTime)(row)
    def update(pit: LocalDateTime)(row: Row): IO[Result[Unit]]

  }

  trait PiTRepoImpl[IO[_], K, V] { self: PointInTimeRepository[IO, K, V] =>

    implicit def IO_ = self.IO
    implicit def K_  = self.K
    implicit def V_  = self.V

    var cache: Table     = Map.empty
    var pitRows: PitRows = List.empty

    def tableAt(pit: LocalDateTime): IO[Table] =
      IO pure {
        (for {
          (k, (range, v)) <- pitRows if range contains pit
        } yield (k -> v)).toMap
      }

    def rowsBetween(range: LocalDateTimeRange): IO[Rows] =
      IO pure {
        for {
          (k, (r, v)) <- pitRows if range intersects r
        } yield (k, v)
      }

    def getAt(pit: LocalDateTime)(id: K): IO[Option[V]] =
      IO.map(tableAt(pit)) { _ get id }

    def getBetween(range: LocalDateTimeRange)(id: K): IO[Rows] =
      IO.map(rowsBetween(range)) { _ filter { case (k, _) => k == id } }

    /** mutators record timestamp */
    // pitRow discipline: for any given key, the list of (range, v) has each range perfectly ascending, with the earlierst matching key being the latest [ts, inf)
    override def insert(pit: LocalDateTime)(v: V): IO[Result[K]] =
      IO.map(tableAt(pit)) { table =>
        val now     = localDateTime
        val updated = Interval.fromBounds(Closed(now), Unbound())
        def k       = ??? // FIXME: Fresh
        (table get k) match {
          case Some(u) =>
            // val (pfx, sfx)        = pitRows span (_._1 != k) // (k, _) is `head` of sfx
            // val (kk, (range, uu)) = sfx.headOption getOrElse ??? // tell it like it is
            // assert(k === kk)
            // assert(u === uu)
            // val retired = Interval.fromBounds(range.lowerBound, Open(now))
            Result.fail[K]("sucks")
          case None =>
            Result {
              pitRows = (k, (updated, v)) :: pitRows
              k
            }
        }
      }

    override def update(pit: LocalDateTime)(row: Row): IO[Result[Unit]] = ???

  }
  case class SimplePointInTimeRepository[IO[_]: Monad, K: cats.Order: Fresh, V: Eq]()
      extends PointInTimeRepository[IO, K, V]
      with PiTRepoImpl[IO, K, V]
}

object RepoApi extends RepoApi
