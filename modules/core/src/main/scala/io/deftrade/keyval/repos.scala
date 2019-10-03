package io.deftrade
package keyval

import cats.implicits._
import cats.{ Eq }
import cats.effect.Sync

import fs2.Stream

import scala.language.higherKinds

/**
  * Note: consequence of this design is that there is only one `Key` type per value object type.
  *
  * TODO revisit this decision and its implication.
  */
trait repos {

  /** */
  abstract class ValueRepository[F[_], W[?] <: WithValue.Aux[?], V](
      val V: W[V]
  )(
      implicit final val F: Sync[F]
  ) {

    import V._

    /** Basic in-memory table structure */
    final type Table = Map[Index, Value]

    /** */
    final type R[x] = Stream[F, x]

    /** */
    def rows: R[Row]

    /** */
    def permRows: R[PermRow]

    /**  @return a `Stream` of length zero or one. */
    def get(id: Id): R[Row]

    /**  */
    final type Pred = Row => Boolean

    /**  */
    final def filter(pred: Pred): R[Row] = rows filter pred

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
      override val V: WithKey.AuxK[V]
  ) extends ValueRepository(V) {

    import V._

    /**
      * Note that `get()` is overloaded (not overridden) here.
      */
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
    override def rows: R[Row] = ???

    /** */
    def get(id: Id): R[Row] = ??? /// Stream emit something something
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
    override def permRows: R[PermRow] = ??? // Stream emit [F, PermRow] { /* rowz an stuff */ }

  }

  /** */
  trait MemFileImplKV[F[_], V] /* extends MemFileImplV[F, V] */ {
    self: KeyValueRepository[F, V] =>

    import V._

    /** */
    protected final var kvs: Table = Map.empty

    /** */
    override def rows: R[Row] = ???

    /** Results in re-reading file.  */
    def get(id: Id): R[Row] = ??? /// Stream emit something something
    // def get(id: Id): F[Option[Row]] = F pure { none }

    /** */
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
        oldKvs === kvs
      }
    }

    /** */
    def insert(row: Row): F[Result[Unit]] = F delay (Result safe { kvs += row })

    /** */
    def append(v: V.Row): F[Result[V.Id]] = ???

    /** */
    def permRows: Stream[F, V.PermRow] = ???
  }

  /** */
  sealed abstract case class MemFileValueRepository[F[_]: Sync, V: Eq](
      override val V: WithId[V]
  ) extends ValueOnlyRepository(V)
      with MemFileImplV[F, V]

  /** */
  sealed abstract case class MemFileKeyValueRepository[F[_]: Sync, V: Eq](
      override val V: WithKey.AuxK[V]
  ) extends KeyValueRepository(V)
      with MemFileImplKV[F, V]

  /** */
  def valueRepository[F[_]: Sync, V: Eq](v: WithId[V]): ValueOnlyRepository[F, V] =
    new MemFileValueRepository(v) {}

  /** */
  def keyValueRepository[F[_]: Sync, V: Eq](v: WithKey.AuxK[V]): KeyValueRepository[F, V] =
    new MemFileKeyValueRepository(v) {}
}

object repos extends repos
