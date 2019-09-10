package io.deftrade
package keyval

import cats.{ Eq }
import cats.effect.Sync

import fs2.Stream

import scala.language.higherKinds

/**
  * @note one consequence of this design is that there is only one `Key` type per value object type.
  */
trait repos {

  /**
    * `V` is a type carrier, basically.
    */
  abstract class ValueRepository[F[_], W[?] <: WithValue[?], V](
      val V: W[V]
  )(
      implicit
      final val F: Sync[F],
      final val eqV: Eq[V]
  ) {

    import V._

    /** */
    final type T[k, v] = Map[k, v]

    /** Basic in-memory table structure */
    final type Table = T[Index, Value]

    /** */
    final type R[x] = Stream[F, x]

    /** */
    final type Rows = R[Row]

    /** */
    final type PermRows = R[PermRow]

    /** */
    def rows: Rows

    /** */
    def permRows: PermRows

    /**  @return a `Stream` of length zero or one. */
    def get(id: Id): Rows

    /**  */
    final type Pred = Row => Boolean

    /**  */
    final def filter(pred: Pred): Rows = rows filter pred

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
      override val V: WithKey[V]
  ) extends ValueRepository(V) {

    import V._

    /** TODO: review the fact that this overloads `get()` `*/
    def get(k: V.Key): R[V.Value]

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
    def delete(k: V.Key): F[Result[Boolean]]
  }

  /** */
  trait MemFileImplV[F[_], V] { self: ValueOnlyRepository[F, V] =>

    import V._

    /** FIXME this needs to be atomic swap. Think about it. :| */
    private var id: Id = fresh.init

    /** */
    protected final var kvs: Table = Map.empty

    /** */
    override def rows: Rows = ???

    /** */
    def get(id: Id): Rows = ??? /// Stream emit something something
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

    /** */
    override def permRows: PermRows = Stream emit [F, PermRow] {
      def f = ???
      f
    }

  }

  /** */
  trait MemFileImplKV[F[_], V] /* extends MemFileImplV[F, V] */ {
    self: KeyValueRepository[F, V] =>

    import V._

    /** */
    protected final var kvs: Table = Map.empty

    /** */
    override def rows: Rows = ???

    /** Results in re-reading file.  */
    def get(id: Id): Rows = ??? /// Stream emit something something
    // def get(id: Id): F[Option[Row]] = F pure { none }

    def get(k: Key): R[Value] = ???

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

    /** */
    def append(v: V.Row): F[Result[V.Id]] = ???

    /** */
    def permRows: Stream[F, V.PermRow] = ???

  }

  final class MemFileValueRepository[F[_]: Sync, V: Eq](final override val V: WithId[V])
      extends ValueOnlyRepository(V)
      with MemFileImplV[F, V]

  final class MemFileKeyValueRepository[F[_]: Sync, V: Eq](final override val V: WithKey[V])
      extends KeyValueRepository(V)
      with MemFileImplKV[F, V]
}

object repos extends repos
