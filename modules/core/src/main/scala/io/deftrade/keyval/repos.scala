package io.deftrade
package keyval

import cats.implicits._
import cats.{ Eq }
import cats.effect.{ Blocker, ContextShift, Resource, Sync }

import fs2._

import scala.language.higherKinds

import java.nio.file.{ Path, Paths }

/**
  */
trait repos {

  trait ValueModuleTypes {

    type ValueCompanionType[x] <: WithValue.Aux[x]

    type EffectType[_]

    implicit def F: Sync[EffectType]
    implicit def X: ContextShift[EffectType]

    /** */
    final type R[x] = Stream[EffectType, x]
  }

  object ValueModuleTypes {
    abstract class Aux[F[_], W[?] <: WithValue.Aux[?], V](
        val V: W[V]
    )(
        implicit
        final override val F: Sync[F],
        final override val X: ContextShift[F]
    ) extends ValueModuleTypes {
      final type EffectType[x]         = F[x]
      final type ValueCompanionType[x] = W[x]
      import V.{ Index, Value }

      /** Basic in-memory table structure */
      final type Table = Map[Index, Value]
    }
  }

  /** */
  abstract class ValueRepository[F[_]: Sync: ContextShift, W[?] <: WithValue.Aux[?], V](
      v: W[V]
  ) extends ValueModuleTypes.Aux[F, W, V](v) {

    import V._

    /**  */
    final type Pred = Row => Boolean

    /**  */
    final def filter(pred: Pred): R[Row] = rows filter pred

    /** */
    final def fresh: Fresh[Id] = Fresh.zeroBasedIncr

    /** */
    def rows: R[Row]

    /** */
    def permRows: R[PermRow]

    /**  @return a `Stream` of length zero or one. */
    def get(id: Id): R[Row]

    /** */
    def append(v: Row): R[Id]
  }

  /**  Necessary for ctor parameter V to carry the specific type mapping. (Index mapped to Id) */
  abstract class ValueOnlyRepository[F[_]: Sync: ContextShift, V: Eq](
      override val V: WithId[V]
  ) extends ValueRepository(V)

  /**  */
  abstract class KeyValueRepository[F[_]: Sync: ContextShift, V: Eq](
      override val V: WithKey.AuxK[V]
  ) extends ValueRepository(V) {

    import V._

    /**
      * Note that `get()` is overloaded (not overridden) here.
      */
    def get(k: V.Key): R[Value]

    /** */
    def insert(row: Row): R[Unit]

    /**
      * Default (overridable!) implementation tries insert, then update.
      *
      * @return the number of rows inserted
      */
    def upsert(row: Row): R[Int] = ???

    /** */
    def update(row: Row): R[Unit]

    /** */
    def delete(k: V.Key): R[Boolean]
  }

  /** */
  trait MemFileImplV[F[_], V] { self: ValueOnlyRepository[F, V] =>

    import V._

    def path: Path = Paths get "target/foo.txt"

    /** FIXME this needs to be atomic swap. Think about it. :| */
    private var id: Id = fresh.init

    /** */
    protected final var kvs: Table = Map.empty

    /** FIXME: */
    override def rows: R[Row] = {
      def rsrc: Resource[EffectType, Blocker] = Blocker[EffectType]
      val blockerz: R[Blocker]                = Stream resource rsrc
      val foo: Blocker => R[Byte] =
        blocker => io.file.readAll[EffectType](path, blocker, 4096)
      def bs: R[Byte]                            = ??? // blockerz flatMap foo
      def csv2row: Pipe[EffectType, String, Row] = ???
      //
      // def x: R[String] = bs through text.utf8Decode // through text.lines // through csv2row
      ???
    }
    //

    /** */
    def get(id: Id): R[Row] = ??? /// Stream emit something something
    // F pure { kvs get id }

    /** keep this streamless for now */
    final def append(v: V): R[Id] = ???
    //   F delay {
    //   Result safe {
    //     id = fresh next id
    //     // FIXME the file append goes here k thx
    //     kvs += (id -> v)
    //     id
    //   }
    // }

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
    def update(row: Row): R[Unit] = ???
    //   Sync[EffectType] delay {
    //   Result safe { kvs += row }
    // }

    /** */
    def delete(k: Key): R[Boolean] = ???
    //   F pure {
    //   Result safe {
    //     val oldKvs: Table = kvs
    //     kvs -= k
    //     oldKvs === kvs
    //   }
    // }

    /** */
    def insert(row: Row): R[Unit] = ??? // F delay (Result safe { kvs += row })

    /** */
    def append(v: Row): R[Id] = ???

    /** */
    def permRows: R[PermRow] = ???
  }

  /** */
  sealed abstract case class MemFileValueRepository[F[_]: Sync: ContextShift, V: Eq](
      override val V: WithId[V]
  ) extends ValueOnlyRepository(V)
      with MemFileImplV[F, V]

  /** */
  sealed abstract case class MemFileKeyValueRepository[F[_]: Sync: ContextShift, V: Eq](
      override val V: WithKey.AuxK[V]
  ) extends KeyValueRepository(V)
      with MemFileImplKV[F, V]

  /** */
  def valueRepository[F[_]: Sync: ContextShift, V: Eq](v: WithId[V]): ValueOnlyRepository[F, V] =
    new MemFileValueRepository(v) {}

  /** */
  def keyValueRepository[F[_]: Sync: ContextShift, V: Eq](v: WithKey.AuxK[V]): KeyValueRepository[F, V] =
    new MemFileKeyValueRepository(v) {}
}

object repos extends repos
