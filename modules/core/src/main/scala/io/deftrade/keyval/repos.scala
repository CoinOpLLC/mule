/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.deftrade
package keyval

import implicits._

import cats.implicits._
import cats.{ Eq }
import cats.effect.{ Blocker, ContextShift, Resource, Sync }

import fs2._

import scala.language.higherKinds

import scala.concurrent.ExecutionContext

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
      override val V: WithId.Aux[V]
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

    def path: Path

    /** FIXME this needs to be atomic swap. Think about it. :| */
    private var id: Id = fresh.init

    /** */
    protected final var kvs: Table = Map.empty

    /** FIXME: */
    final override def rows: R[Row] = ??? // kvs.toList.liftTo[EffectType].somehow
    //

    /** */
    final def get(id: Id): R[Row] = permRows filter (_._1 === id) map (_._2)

    /** keep this streamless for now */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def append(r: Row): R[Id] = {

      def updateCache() = () // r match { case (k, v) => { kvs = kvs + (k -> v) } }

      for {
        id <- ids
        _  <- Stream emit id -> r through permRowToCSV through aof
        _  <- Stream emit updateCache()
      } yield id
    }

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final override def permRows: R[PermRow] = readLines through csvToPermRow

    /** */
    private def ids: R[Id] = ???

    /** */
    private def aof: Pipe[EffectType, String, Unit] = ???

    /** */
    private def csvToPermRow: Pipe[EffectType, String, PermRow] = ???

    /** */
    private def permRowToCSV: Pipe[EffectType, PermRow, String] = ???

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    private def readLines: R[String] =
      (Stream resource Blocker[EffectType]) flatMap { blocker =>
        io.file readAll [EffectType] (path, blocker, 1024 * 1042)
      } through text.utf8Decode through text.lines
  }

  /** */
  trait MemFileImplKV[F[_], V] /* extends MemFileImplV[F, V] */ {
    self: KeyValueRepository[F, V] =>

    import V._

    /** */
    protected final var kvs: Table = Map.empty

    /** */
    def path: Path

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
      override val V: WithId.Aux[V]
  ) extends ValueOnlyRepository(V)
      with MemFileImplV[F, V]

  /** */
  sealed abstract case class MemFileKeyValueRepository[F[_]: Sync: ContextShift, V: Eq](
      override val V: WithKey.AuxK[V]
  ) extends KeyValueRepository(V)
      with MemFileImplKV[F, V]

  /** */
  def valueRepository[F[_]: Sync: ContextShift, V: Eq](
      v: WithId.Aux[V],
      p: Path
  ): ValueOnlyRepository[F, V] =
    new MemFileValueRepository(v) { override def path = p }

  /** */
  def keyValueRepository[F[_]: Sync: ContextShift, V: Eq](
      v: WithKey.AuxK[V],
      p: Path
  ): KeyValueRepository[F, V] =
    new MemFileKeyValueRepository(v) { override def path = p }
}

object repos extends repos
