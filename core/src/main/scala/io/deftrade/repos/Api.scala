package io.deftrade
package repos

import io.deftrade.time._
import io.deftrade.time.implicits._

import io.deftrade.opaqueid._

import cats._
import cats.implicits._

import spire.math.Interval
import spire.math.interval._

import scala.language.higherKinds

sealed abstract class Fail extends Product with Serializable {
  def msg: String
}
object Fail {
  final case class Impl(val msg: String) extends Fail
  def apply(msg: String): Fail = Impl(msg)
}

/**
  * TODO: There is a single `F[_]` type - `IO` - below.
  * split into an effect type and a container type (IO/Pure vs Stream/Chain)
  *
  * n.b. doobie can return `Streams`
  *
  */
trait Api {

  abstract class RepoImplicits[IO[_]: Monad, K: cats.Order, V: Eq] {

    lazy val IO = Monad[IO]
    lazy val K  = cats.Order[K]
    lazy val V  = cats.Eq[V]
  }

  abstract class AppendableRepoImplicits[IO[_]: Monad, K: cats.Order: Fresh, V: Eq] extends RepoImplicits[IO, K, V] {
    lazy val FK = Fresh[K]
  }

  trait Repository[IO[_], K, V] { self: RepoImplicits[IO, K, V] =>

    type R[x]
    type T[k, v]

    final type Row   = (K, V)
    final type Rows  = R[Row]
    final type Table = T[K, V]

    /** Simple Queries */
    def rows: IO[Rows]
    def get(k: K): IO[Option[V]]
  }

  trait Filterable[IO[_], K, V] { self: RepoImplicits[IO, K, V] with Repository[IO, K, V] =>
    def filter(f: Row => Boolean): IO[Rows] = ???
  }

  trait Mutable[IO[_], K, V] { self: RepoImplicits[IO, K, V] with Repository[IO, K, V] =>
    def update(row: Row): IO[Result[Unit]]
    def delete(k: K): IO[Result[Boolean]]
  }

  trait Insertable[IO[_], K, V] { self: RepoImplicits[IO, K, V] with Repository[IO, K, V] =>
    def insert(row: Row): IO[Result[Unit]]
    def upsert(row: Row): IO[Result[Int]] = ??? // try insert, then update
  }

  trait Appendable[IO[_], K, V] { self: AppendableRepoImplicits[IO, K, V] =>
    def append(v: V): IO[Result[K]]
  }

  trait MemImpl[IO[_], K, V] {
    self: RepoImplicits[IO, K, V] with Repository[IO, K, V] with Mutable[IO, K, V] =>

    type R[x]    = List[x]
    type T[k, v] = Map[k, v]

    protected var kvs: Table     = Map.empty
    def rows: IO[Rows]           = IO pure kvs.toList
    def get(k: K): IO[Option[V]] = IO pure { kvs get k }
    def update(row: Row): IO[Result[Unit]] = IO pure {
      Result { kvs += row }
    }
    def delete(k: K): IO[Result[Boolean]] = IO pure {
      Result {
        implicit def V_       = self.V
        val oldKvs: Map[K, V] = kvs
        kvs -= k
        oldKvs === kvs
      }
    }
  }

  trait MemImplAppendable[IO[_], K, V] extends MemImpl[IO, K, V] {
    self: AppendableRepoImplicits[IO, K, V] with Repository[IO, K, V] with Mutable[IO, K, V] with Appendable[IO, K, V] =>

    private var k: K = FK.init

    def append(v: V): IO[Result[K]] = IO pure {
      Result {
        k = FK next k
        kvs += (k -> v)
        k
      }
    }
  }

  trait MemImplInsertable[IO[_], K, V] extends MemImpl[IO, K, V] {
    self: RepoImplicits[IO, K, V] with Repository[IO, K, V] with Mutable[IO, K, V] with Insertable[IO, K, V] =>

    def insert(row: Row): IO[Result[Unit]] = IO pure Result { kvs += row }
  }

  class MemAppendableRepository[IO[_]: Monad, K: cats.Order: Fresh, V: Eq]
      extends AppendableRepoImplicits[IO, K, V]
      with Repository[IO, K, V]
      with Mutable[IO, K, V]
      with Appendable[IO, K, V]
      with MemImplAppendable[IO, K, V]

  class MemInsertableRepository[IO[_]: Monad, K: cats.Order, V: Eq]
      extends RepoImplicits[IO, K, V]
      with Repository[IO, K, V]
      with Mutable[IO, K, V]
      with Insertable[IO, K, V]
      with MemImplInsertable[IO, K, V]

  /**
    * `PointInTime` feature
    */
  trait PointInTimeRepository[IO[_], K, V] {
    self: AppendableRepoImplicits[IO, K, V] with Repository[IO, K, V] with Mutable[IO, K, V] with Appendable[IO, K, V] =>

    type LocalDateTimeRange = Interval[LocalDateTime]
    object LocalDateTimeRange {
      def all: LocalDateTimeRange = Interval.all[LocalDateTime]
    }

    type PitTable = Map[K, List[(LocalDateTimeRange, V)]]
    type PitRow   = (K, (LocalDateTimeRange, V))
    type PitRows  = List[PitRow]

    /** Simple Queries always take from the current data stored in the `Table` */
    final override def rows: IO[Rows]           = rowsBetween(LocalDateTimeRange.all)
    final override def get(k: K): IO[Option[V]] = getAt(localDateTime)(k)

    /** Point In Time Queries */
    def tableAt(pit: LocalDateTime): IO[Table]
    def rowsBetween(range: LocalDateTimeRange): IO[Rows]
    def getAt(pit: LocalDateTime)(k: K): IO[Option[V]]
    def getBetween(range: LocalDateTimeRange)(k: K): IO[Rows]

    /** mutators record timestamp */
    final override def append(v: V): IO[Result[K]] = append(localDateTime)(v)
    def append(pit: LocalDateTime)(v: V): IO[Result[K]]

    final override def update(row: Row): IO[Result[Unit]] = update(localDateTime)(row)
    def update(pit: LocalDateTime)(row: Row): IO[Result[Unit]]

    final override def delete(k: K): IO[Result[Boolean]] = delete(localDateTime)(k)
    def delete(pit: LocalDateTime)(k: K): IO[Result[Boolean]]

  }

  trait MemImplPiT[IO[_], K, V] {
    self: AppendableRepoImplicits[IO, K, V] with PointInTimeRepository[IO, K, V] with Repository[IO, K, V] =>

    implicit def IO_ = self.IO
    implicit def K_  = self.K
    implicit def V_  = self.V

    type R[x]    = List[x]
    type T[k, v] = Map[k, v]

    var cache: Table     = Map.empty
    var pitRows: PitRows = List.empty
    var k: K             = FK.init

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

    def getAt(pit: LocalDateTime)(k: K): IO[Option[V]] =
      IO.map(tableAt(pit)) { _ get k }

    def getBetween(range: LocalDateTimeRange)(k: K): IO[Rows] =
      IO.map(rowsBetween(range)) { _ filter { case (kk, _) => k == kk } }

    /** mutators record timestamp
    pitRow discipline: for any given key, the list of (range, v) has each range perfectly ascending, with the earlierst matching key being the latest [ts, inf)
      */
    override def append(pit: LocalDateTime)(v: V): IO[Result[K]] =
      IO.map(tableAt(pit)) { table =>
        val now     = localDateTime
        val updated = Interval.fromBounds(Closed(now), Unbound())
        k = FK next k
        (table get k) match {
          case Some(u) =>
            Result.fail[K](s"id=$k already present with value $u")
          case None =>
            Result {
              pitRows = (k, (updated, v)) :: pitRows
              k
            }
        }
      }
    override def update(pit: LocalDateTime)(row: Row): IO[Result[Unit]] =
      IO.map(tableAt(pit)) { table =>
        val now     = localDateTime
        val updated = Interval.fromBounds(Closed(now), Unbound())
        val (k, v)  = row
        (table get k) match {
          case Some(u) =>
            Result {
              val (_, sfx)          = pitRows span (_._1 =!= k) // (k, _) is `head` of sfx
              val (kk, (range, uu)) = sfx.headOption getOrElse ??? // tell it like it is
              assert(k === kk)
              assert(u === uu)
              val ValueBound(lb) = range.lowerBound
              assert(lb < now)
              val retired = Interval fromBounds (range.lowerBound, Open(now))
              pitRows = (k, (updated, v)) :: (k, (retired, u)) :: (pitRows drop 1)
              // FIXME: and herin lies the problem: we'd like a stable-over-time id  as well
              // the better to reference `V`s without needing dates.
              // solution:
              // type PitMap[J] = Map[J, PitRow]
              // ^ implement this as insert-only
              // insert has to return _two_ ids? that ain't right... !
              // // alt semantics where library client is in charge of key generation
              // def insert(row: Row): Result[Unit]
              // def upsert(row: Row): Result[Boolean]
              // def insert(row: Row): Result[J]
              // def upsert(row: Row): Result[J]  // row._1 == J.init != return if new
            }
          case None =>
            Result.fail[Unit](s"id=$k not found")
        }
      }
    override def delete(pit: LocalDateTime)(k: K): IO[Result[Boolean]] = ???

  }
  case class SimplePointInTimeRepository[IO[_]: Monad, K: cats.Order: Fresh, V: Eq]()
      extends AppendableRepoImplicits[IO, K, V]
      with PointInTimeRepository[IO, K, V]
      with Repository[IO, K, V]
      with Mutable[IO, K, V]
      with Appendable[IO, K, V]
      with MemImplPiT[IO, K, V]
}

object Api extends Api
