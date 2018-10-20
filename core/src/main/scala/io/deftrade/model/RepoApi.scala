package io.deftrade
package model

import scala.language.higherKinds

import io.deftrade.time._
import io.deftrade.time.implicits._

import cats._
import cats.implicits._

import spire.math.Interval
import spire.math.interval._

trait RepoApi {
  //////////////////////////////////////////////////////////////////////////////////////////////////
  //  Repository
  //////////////////////////////////////////////////////////////////////////////////////////////////

  abstract class Repository[IO[_]: Monad, K: cats.Order, V: Eq] {

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

    def delete(id: K): IO[Result[Unit]] = ???
    def upsert(row: Row): IO[Result[Unit]]
    def upsert(table: Table): IO[Result[Unit]] = (IO pure table) flatMap { upsert(_) }
  }

  trait RepositoryMemImpl[IO[_], K, V] { self: Repository[IO, K, V] =>
    private var kvs: Table        = Map.empty
    def rows: IO[Rows]            = IO pure kvs.toList
    def table: IO[Table]          = IO pure kvs
    def get(id: K): IO[Option[V]] = IO pure { kvs get id }

    /** mutators record timestamp */
    def upsert(row: Row): IO[Result[Unit]] = IO pure { Result { kvs = kvs + row } }

  }

  class SimpleRepository[IO[_]: Monad, K: cats.Order, V: Eq] extends Repository[IO, K, V] with RepositoryMemImpl[IO, K, V]

  abstract class PointInTimeRepository[IO[_]: Monad, K: cats.Order, V: Eq] extends Repository[IO, K, V] {

    type LocalDateTimeRange = Interval[LocalDateTime]
    object LocalDateTimeRange {
      def all: LocalDateTimeRange = Interval.all[LocalDateTime]
    }

    type PitTable = Map[K, List[(LocalDateTimeRange, V)]]
    type PitRow   = (K, (LocalDateTimeRange, V))
    type PitRows  = List[PitRow]

    /** Simple Queries always take from the current data stored in the `Table` */
    final def rows: IO[Rows]            = rowsBetween(LocalDateTimeRange.all)
    final def table: IO[Table]          = tableAt(localDateTime)
    final def get(id: K): IO[Option[V]] = getAt(localDateTime)(id)

    /** Point In Time Queries */
    def tableAt(pit: LocalDateTime): IO[Table]
    def rowsBetween(range: LocalDateTimeRange): IO[Rows]
    def getAt(pit: LocalDateTime)(id: K): IO[Option[V]]
    def getBetween(range: LocalDateTimeRange)(id: K): IO[Rows]
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
    override def upsert(row: Row): IO[Result[Unit]] = upsert(localDateTime)(row)

    // pitRow discipline: for any given key, the list of (range, v) has each range perfectly ascending, with the earlierst matching key being the latest [ts, inf)
    def upsert(pit: LocalDateTime)(row: Row): IO[Result[Unit]] =
      IO.map(tableAt(pit)) { table =>
        val (k, v)  = row
        val now     = localDateTime
        val updated = Interval.fromBounds(Closed(now), Unbound())
        (table get k) match {
          case Some(u) =>
            val (pfx, sfx)        = pitRows span (_._1 != k) // (k, _) is `head` of sfx
            val (kk, (range, uu)) = sfx.headOption getOrElse ??? // tell it like it is
            assert(k === kk)
            assert(u === uu)
            val retired = Interval.fromBounds(range.lowerBound, Open(now))
            Result { pitRows = ((k, (updated, v)) :: pfx) ::: ((k, (retired, u)) :: sfx drop 1) }
          case None =>
            Result { pitRows = (k, (updated, v)) :: pitRows }
        }
      }
  }
  case class SimplePointInTimeRepository[IO[_]: Monad, K: cats.Order, V: Eq]()
      extends PointInTimeRepository[IO, K, V]
      with PiTRepoImpl[IO, K, V]
}

object RepoApi extends RepoApi
