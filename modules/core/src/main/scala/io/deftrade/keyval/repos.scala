package io.deftrade
package keyval

import cats.implicits._

import cats.{ Eq }
import cats.effect.Sync

import fs2.Stream

import scala.language.higherKinds

/**
  * @note one consequence of this design is that there is only one `Key` type per value object type.
  */
trait repos {

  /** */
  abstract class ValueRepository[F[_], W[?] <: WithValue[?], V](
      val V: W[V]
  )(
      implicit
      final val F: Sync[F],
      final val eqV: Eq[V]
  ) {

    import V._

    /** */
    def rows: Stream[F, Row]

    /** */
    def permRows: Stream[F, PermRow]

    /**  @return a `Stream` of length zero or one. */
    def get(id: Id): Stream[F, Row]

    /**  */
    final type Pred = Row => Boolean

    /**  */
    final def filter(pred: Pred): Stream[F, Row] = rows filter pred

    /** */
    def fresh: Fresh[Id] = Fresh.zeroBasedIncr

    /** */
    def append(v: Row): F[Result[Id]]
  }

  /**  Necessary for ctor parameter V to carry the specific type mapping. (Index mapped to Id) */
  abstract class ValueOnlyRepository[F[_]: Sync, V: Eq](
      override val V: WithId[V]
  ) extends ValueRepository(V)

  /**  */
  abstract class KeyValueRepository[F[_]: Sync, V: Eq](
      val KV: WithKey[V]
  ) extends ValueRepository(KV) {

    import KV._

    /** TODO: review the fact that this overloads `get()` `*/
    def get(k: KV.Key): Stream[F, KV.Value]

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

    private var id: Id = fresh.init

    /** */
    protected final var kvs: Table = Map.empty

    /** */
    override def rows: Stream[F, Row] = ???

    /** */
    def get(id: Id): Stream[F, Row] = ??? /// Stream emit something something
    // F pure { kvs get id }

    /** keep this streamless for now */
    final def append(v: V): F[Result[Id]] = F delay {
      Result safe {
        id = fresh next id
        // FIXME the file append goes here k thx
        kvs += (id -> v)
        id
      }
    }
  }

  trait MemFileImplKV[F[_], V] /* extends MemFileImplV[F, V] */ {
    self: KeyValueRepository[F, V] =>

    import KV._

    final val WV: WithValue[V] = KV

    /** */
    protected final var kvs: Table = Map.empty

    /** */
    override def rows: Stream[F, Row] = ???

    /** Results in re-reading file.  */
    def get(id: Id): Stream[F, Row] = ??? /// Stream emit something something
    // def get(id: Id): F[Option[Row]] = F pure { none }

    def get(k: KV.Key): Stream[F, KV.Value] = ???

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

  final class MemFileValueRepository[F[_]: Sync, V: Eq](final override val V: WithId[V])
      extends ValueOnlyRepository(V)
      with MemFileImplV[F, V] {

    import V._

    override def fresh: Fresh[OpaqueKey[Long, Value]] = ???
    override def permRows: Stream[F, PermRow]         = ???

  }

  // final class MemFileKeyValueRepository[F[_]: Sync, V: Eq](KV: WithKey[V]) extends KeyValueRepository(KV) with MemFileImplKV[F, V]
}

object repos extends repos
