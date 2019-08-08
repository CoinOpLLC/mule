package io.deftrade
package repos

import time._, keyval._

import cats.implicits._

import cats.{ Eq, Order }
import cats.effect.Sync

import spire.math.Interval
import spire.math.interval._

import scala.language.higherKinds

/**
  * TODO: There is a single `F[_]` type - `F` - below.
  * split into an effect type and a container type (F/Pure vs Stream/Chain)
  *
  * nb doobie can return `Streams`
  *
  * FIXME: The "toy repository model" becomes:
  * - append-only file of `WithXyxKey.PitRow`s
  * - in memory `WithXyxKey.Table`
  *
  */
trait Api {

  /** */
  abstract class ValueRepoImplicits[F[_]: Sync, V: Eq] {

    /** */
    final lazy val F = Sync[F]

    /** */
    final lazy val V = Eq[V]
  }

  /** */
  abstract class KeyValueRepoImplicits[F[_]: Sync, K: Order, V: Eq] extends ValueRepoImplicits[F, V] {

    /** */
    final lazy val K = Order[K]
  }

  /** */
  abstract class AppendableRepoImplicits[F[_]: Sync, K: Order: Fresh, V: Eq] extends KeyValueRepoImplicits[F, K, V] {

    /** */
    final lazy val fresh = Fresh[K]
  }

  abstract class ValueRepository[F[_]: Sync, V: Eq](
      val value: WithRepo[V]
  ) extends ValueRepoImplicits[F, V] {

    /** Simple Queries */
    def rows: F[value.Rows]
    def get(id: value.Id): F[Option[value.Value]]
  }

  abstract class KeyValueRepository[F[_], V](
      final val KV: keyval.WithKeyBase[V]
  )(implicit
    final val F: Sync[F],
    final val V: Eq[V]) {

    /** Simple Queries */
    def rows: F[KV.Rows]
    def get(k: KV.Key): F[Option[KV.Value]]
  }

  trait Repository[F[_], K, V] { self: KeyValueRepoImplicits[F, K, V] =>

    type R[x]
    type T[k, v]

    final type Row   = (K, V)
    final type Rows  = R[Row]
    final type Table = T[K, V]

    /** Simple Queries */
    def rows: F[Rows]
    def get(k: K): F[Option[V]]
  }

  trait Filterable[F[_], K, V] { self: KeyValueRepoImplicits[F, K, V] with Repository[F, K, V] =>
    def filter(f: Row => Boolean): F[Rows] = ???
  }

  trait Mutable[F[_], K, V] { self: KeyValueRepoImplicits[F, K, V] with Repository[F, K, V] =>
    def update(row: Row): F[Result[Unit]]
    def delete(k: K): F[Result[Boolean]]
  }

  trait Insertable[F[_], K, V] { self: KeyValueRepoImplicits[F, K, V] with Repository[F, K, V] =>
    def insert(row: Row): F[Result[Unit]]
    def upsert(row: Row): F[Result[Int]] = ??? // try insert, then update
  }

  trait Appendable[F[_], K, V] { self: AppendableRepoImplicits[F, K, V] =>
    def append(v: V): F[Result[K]]
  }

  trait MemImpl[F[_], K, V] {
    self: KeyValueRepoImplicits[F, K, V] with Repository[F, K, V] with Mutable[F, K, V] =>

    type R[x]    = List[x]
    type T[k, v] = Map[k, v]

    protected var kvs: Table    = Map.empty
    def rows: F[Rows]           = F pure kvs.toList
    def get(k: K): F[Option[V]] = F pure { kvs get k }
    def update(row: Row): F[Result[Unit]] = F pure {
      Result safe { kvs += row }
    }
    def delete(k: K): F[Result[Boolean]] = F pure {
      Result safe {
        implicit def V_       = self.V
        val oldKvs: Map[K, V] = kvs
        kvs -= k
        oldKvs === kvs
      }
    }
  }

  trait MemImplAppendable[F[_], K, V] extends MemImpl[F, K, V] {
    self: AppendableRepoImplicits[F, K, V] with Repository[F, K, V] with Mutable[F, K, V] with Appendable[F, K, V] =>

    private var k: K = fresh.init

    def append(v: V): F[Result[K]] = F pure {
      Result safe {
        k = fresh next k
        kvs += (k -> v)
        k
      }
    }
  }

  trait MemImplInsertable[F[_], K, V] extends MemImpl[F, K, V] {
    self: KeyValueRepoImplicits[F, K, V] with Repository[F, K, V] with Mutable[F, K, V] with Insertable[F, K, V] =>

    def insert(row: Row): F[Result[Unit]] = F pure (Result safe { kvs += row })
  }

  class MemAppendableRepository[F[_]: Sync, K: cats.Order: Fresh, V: Eq]
      extends AppendableRepoImplicits[F, K, V]
      with Repository[F, K, V]
      with Mutable[F, K, V]
      with Appendable[F, K, V]
      with MemImplAppendable[F, K, V]

  class MemInsertableRepository[F[_]: Sync, K: cats.Order, V: Eq]
      extends KeyValueRepoImplicits[F, K, V]
      with Repository[F, K, V]
      with Mutable[F, K, V]
      with Insertable[F, K, V]
      with MemImplInsertable[F, K, V]

  /**
    * `PointInTime` feature
    */
  trait PointInTimeRepository[F[_], K, V] {
    self: AppendableRepoImplicits[F, K, V] with Repository[F, K, V] with Mutable[F, K, V] with Appendable[F, K, V] =>

    type LocalDateTimeRange = Interval[LocalDateTime]
    object LocalDateTimeRange {
      def all: LocalDateTimeRange = Interval.all[LocalDateTime]
    }

    type PitTable = Map[K, R[(LocalDateTimeRange, V)]]
    type PitRow   = (K, (LocalDateTimeRange, V))
    type PitRows  = R[PitRow]

    /** Simple Queries always take from the current data stored in the `Table` */
    final override def rows: F[Rows]           = rowsBetween(LocalDateTimeRange.all)
    final override def get(k: K): F[Option[V]] = getAt(localDateTime)(k)

    /** Point In Time Queries */
    def tableAt(pit: LocalDateTime): F[Table]
    def rowsBetween(range: LocalDateTimeRange): F[Rows]
    def getAt(pit: LocalDateTime)(k: K): F[Option[V]]
    def getBetween(range: LocalDateTimeRange)(k: K): F[Rows]

    /** mutators record timestamp */
    final override def update(row: Row): F[Result[Unit]] = update(localDateTime)(row)
    def update(pit: LocalDateTime)(row: Row): F[Result[Unit]]

    final override def delete(k: K): F[Result[Boolean]] = delete(localDateTime)(k)
    def delete(pit: LocalDateTime)(k: K): F[Result[Boolean]]

  }

  case class SimplePointInTimeRepository[F[_]: Sync, K: cats.Order: Fresh, V: Eq]()
      extends AppendableRepoImplicits[F, K, V]
      with PointInTimeRepository[F, K, V]
      with Repository[F, K, V]
      with Mutable[F, K, V]
      with Appendable[F, K, V] {

    type R[x]    = List[x]
    type T[k, v] = Map[k, v]

    // @SuppressWarnings(Array("org.wartremover.warts.Var"))
    // private var cache: Table = Map.empty
    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    private var pitRows: PitRows = List.empty

    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    private var k: K = fresh.init

    def tableAt(pit: LocalDateTime): F[Table] =
      F pure {
        (for {
          (k, (range, v)) <- pitRows if range contains pit
        } yield (k -> v)).toMap
      }

    def rowsBetween(range: LocalDateTimeRange): F[Rows] =
      F pure {
        for {
          (k, (r, v)) <- pitRows if range intersects r
        } yield (k, v)
      }

    def getAt(pit: LocalDateTime)(k: K): F[Option[V]] =
      F.map(tableAt(pit)) { _ get k }

    def getBetween(range: LocalDateTimeRange)(k: K): F[Rows] =
      F.map(rowsBetween(range)) { _ filter { case (kk, _) => k == kk } }

    /** mutators record timestamp
    pitRow discipline: for any given key, the list of (range, v) has each range perfectly ascending, with the earlierst matching key being the latest [ts, inf)
      */
    override def append(v: V): F[Result[K]] =
      F pure {
        val updated = Interval fromBounds (Closed(localDateTime), Unbound())
        k = fresh next k
        pitRows = (k, (updated, v)) :: pitRows
        Result safe { k }
      }

    override def update(pit: LocalDateTime)(row: Row): F[Result[Unit]] = {
      import Interval.fromBounds
      val now    = localDateTime
      val (k, v) = row
      F.map(tableAt(pit)) { table =>
        (table get k).fold(Result.fail[Unit](s"id=$k not found")) { u =>
          Result safe {
            val (_, sfx)          = pitRows span (_._1 =!= k) // (k, _) is `head` of suffix
            val (kk, (range, uu)) = sfx.headOption getOrElse ??? // tell it like it is
            assert(k === kk)
            assert(u === uu)
            val ValueBound(lb) = range.lowerBound
            assert(lb < now)
            // FIXME bugz drop 1 lmao
            // todo: use cache to remember start of interval:
            // var cache: Map[K, (LocalDateTimeRange, V)]
            // don't worry about stale open intervals: flatMap them to empty when reading the file.
            // only apppend to a fs2.Stream[F, (K, LocalDateTimeRange, V)]
            pitRows = (k, (fromBounds(Closed(now), Unbound()), v)) :: (k, (fromBounds(range.lowerBound, Open(now)), u)) :: (pitRows drop 1)
          }
        }
      }
    }
    override def delete(pit: LocalDateTime)(k: K): F[Result[Boolean]] = ???

  }
}

object Api extends Api
