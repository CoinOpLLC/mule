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
  */
trait Api {

  /** */
  abstract class ValueRepository[F[_], V](
      val V: WithValue[V]
  )(
      implicit
      final val F: Sync[F],
      final val eqV: Eq[V]
  ) {

    import V._

    /**  */
    def rows: F[Rows]

    /** */
    def permaRows: F[PermaRows]

    /**  */
    def get(id: Id): F[Option[Row]]

    /**  */
    final type Pred = Row => Boolean

    /**  */
    final def filter(pred: Pred): F[Rows] = rows map { _ filter pred }

    /** */
    def fresh: Fresh[Id]

    /** */
    def append(v: Row): F[Result[Id]]
  }

  /**  Necessary for ctor parameter V to carry the specific type mapping. (Index mapped to Id) */
  abstract class ValueOnlyRepository[F[_]: Sync, V: Eq](
      final override val V: WithId[V]
  ) extends ValueRepository[F, V](V)

  /**  */
  abstract class KeyValueRepository[F[_]: Sync, V: Eq](
      final val KV: WithKey[V]
  ) extends ValueRepository(KV) {

    import KV._

    /** TODO: review the fact that this overloads `get()` `*/
    def get(k: KV.Key): F[Option[KV.Value]]

    /** */
    def insert(row: Row): F[Result[Unit]]

    /**
      * Default (overridable!) implementation tries insert, then update.
      *
      * @return the number of rows inserted
      */
    def upsert(row: Row): F[Result[Int]] = ???

    /** */
    def update(row: Row): F[Result[Unit]]

    /** */
    def delete(k: Key): F[Result[Boolean]]
  }

  /** */
  trait MemFileImplV[F[_], V] { self: ValueOnlyRepository[F, V] =>

    import V._

    /** */
    protected var kvs: Table = Map.empty

    /** */
    override def rows: F[Rows] = F pure kvs.values.toList

    /** */
    def get(id: Id): F[Option[V]] = F pure { kvs get id }

    private var id: Id = fresh.init

    def append(v: V): F[Result[Id]] = F delay {
      Result safe {
        id = fresh next id
        // FIXME the file append goes here k thx
        kvs += (id -> v)
        id
      }
    }
  }

  trait MemFileImplKV[F[_], V] {
    self: KeyValueRepository[F, V] =>

    import KV._

    val VV: WithValue[V] = KV

    /** */
    protected var kvs: Table = Map.empty

    /** */
    override def rows: F[Rows] = F pure kvs.toList

    /** FIXME effectively unplmenented */
    def get(id: Id): F[Option[Row]] = F pure { none }

    /** */
    def update(row: Row): F[Result[Unit]] = F delay {
      Result safe { kvs += row }
    }

    /** */
    def delete(k: Key): F[Result[Boolean]] = F pure {
      Result safe {
        val oldKvs: Table = kvs
        kvs -= k
        oldKvs == /* WHAAAT? = */ kvs
      }
    }

    /** */
    def insert(row: Row): F[Result[Unit]] = F delay (Result safe { kvs += row })

  }

  final class MemFileValueRepository[F[_]: Sync, V: Eq](V: WithId[V]) extends ValueOnlyRepository[F, V](V) with MemFileImplV[F, V] {
    def fresh: Fresh[OpaqueKey[Long, V]]            = ???
    def permaRows: F[List[(OpaqueKey[Long, V], V)]] = ???
  }
}

object Api extends Api
