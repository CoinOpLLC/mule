package io.deftrade
package model

import scala.language.higherKinds

import io.deftrade.time._
import io.deftrade.opaqueid._

import cats._
import cats.implicits._

trait RepoApi {
  //////////////////////////////////////////////////////////////////////////////////////////////////
  //  Repository
  //////////////////////////////////////////////////////////////////////////////////////////////////

  abstract class Repository[IO[_]: Monad, K, V] {

    lazy val IO = Monad[IO]

    type Id = OpaqueId[K, V]
    object Id extends OpaqueIdC[Id]

    type Row   = (Id, V)
    type Rows  = List[Row]
    type Table = Map[Id, V]

    /** Simple Queries */
    final def empty: IO[Table] = IO pure { Map.empty }
    def rows: IO[Rows]
    def table: IO[Table]
    def get(id: Id): IO[Option[V]]

    /** insert only - no update, no delete */
    def upsert(row: Row): IO[Result[Unit]]
    def upsert(table: Table): IO[Result[Unit]] = (IO pure table) flatMap { upsert(_) }
  }

  trait RepositoryMemImpl[IO[_], K, V] { self: Repository[IO, K, V] =>
    private var kvs: Table         = Map.empty
    def rows: IO[Rows]             = IO pure kvs.toList
    def table: IO[Table]           = IO pure kvs
    def get(id: Id): IO[Option[V]] = IO pure { kvs get id }

    /** mutators record timestamp */
    def upsert(row: Row): IO[Result[Unit]] = IO pure { Result { kvs = kvs + row } }

  }

  class SimpleRepository[IO[_]: Monad, K, V] extends Repository[IO, K, V] with RepositoryMemImpl[IO, K, V]

  abstract class PointInTimeRepository[IO[_]: Monad, K, V] extends Repository[IO, K, V] {

    type LocalDateTimeRange
    object LocalDateTimeRange { def allOfTime: LocalDateTimeRange = ??? }

    type PitTable = Map[K, List[(LocalDateTimeRange, V)]]
    type PitRow   = (Id, (LocalDateTimeRange, V))
    type PitRows  = List[PitRow]

    /** Simple Queries */
    final def rows: IO[Rows]             = rowsBetween(LocalDateTimeRange.allOfTime)
    final def table: IO[Table]           = tableAt(localDateTime)
    final def get(id: Id): IO[Option[V]] = getAt(localDateTime)(id)

    /** Point In Time Queries */
    def tableAt(pit: LocalDateTime): IO[Table]
    def rowsBetween(range: LocalDateTimeRange): IO[Rows]
    def getAt(pit: LocalDateTime)(id: Id): IO[Option[V]]
    def getBetween(range: LocalDateTimeRange)(id: Id): IO[Rows]
  }

  trait PiTRepoImpl[IO[_], K, V] { self: PointInTimeRepository[IO, K, V] =>

    var kvs: Table = Map.empty

    def tableAt(pit: LocalDateTime): IO[Table]                  = ???
    def rowsBetween(range: LocalDateTimeRange): IO[Rows]        = ???
    def getAt(pit: LocalDateTime)(id: Id): IO[Option[V]]        = ???
    def getBetween(range: LocalDateTimeRange)(id: Id): IO[Rows] = ???

    /** mutators record timestamp */
    def upsert(pit: LocalDateTime)(row: (Id, V)): IO[Result[Unit]] = ???

  }

}

object RepoApi extends RepoApi
