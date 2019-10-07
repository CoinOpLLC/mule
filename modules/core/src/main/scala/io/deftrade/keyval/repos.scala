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

import cats.implicits._
import cats.Eq
import cats.effect.{ Blocker, ContextShift, Sync }

import fs2.{ io, text, Pipe, Stream }

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
    final type EffectStream[x] = Stream[EffectType, x]
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
  protected abstract class ValueRepository[F[_]: Sync: ContextShift, W[?] <: WithValue.Aux[?], V](
      v: W[V]
  ) extends ValueModuleTypes.Aux[F, W, V](v) {

    import V._

    @SuppressWarnings(Array("org.wartremover.warts.Var")) private var id: Id = fresh.init

    /**  */
    final def filter(predicate: Row => Boolean): EffectStream[Row] = rows filter predicate

    /** */
    final def rows: EffectStream[Row] = permRows map (_._2)

    /**  @return a `Stream` of length zero or one. */
    def get(id: Id): EffectStream[Row] = permRows filter (_._1 === id) map (_._2)

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def permRows: EffectStream[PermRow] = readLines through csvToPermRow

    /** */
    /** FIXME: not thread safe, put a queue in front of single thread-contained appender */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def append(row: Row): EffectStream[Id] =
      for {
        id <- ids
        _  <- Stream emit id -> row through permRowToCSV through appendingSink
        _  <- Stream emit updateCache(row)
      } yield id

    private def fresh: Fresh[Id]      = Fresh.zeroBasedIncr
    private def ids: EffectStream[Id] = Stream emit [EffectType, Id] { id = fresh next id; id }

    private def permRowToCSV: Pipe[EffectType, PermRow, String] = ???
    private def csvToPermRow: Pipe[EffectType, String, PermRow] = ???

    /** */
    protected def readLines: EffectStream[String]

    /** */
    protected def appendingSink: Pipe[EffectType, String, Unit]

    /** */
    protected def updateCache(row: Row) = ()
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
    def get(k: Key): EffectStream[Value]

    /** */
    def insert(row: Row): EffectStream[Unit]

    /**
      * Default (overridable!) implementation tries insert, then update.
      *
      * @return the number of rows inserted
      */
    def upsert(row: Row): EffectStream[Int] = ???

    /** */
    def update(row: Row): EffectStream[Unit]

    /** */
    def delete(k: V.Key): EffectStream[Boolean]
  }

  /** */
  private trait MemFileImplV[F[_], W[?] <: WithValue.Aux[?], V] { self: ValueRepository[F, W, V] =>

    /** */
    def path: Path

    /** */
    final protected def appendingSink: Pipe[EffectType, String, Unit] = ???

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final protected def readLines: EffectStream[String] =
      (Stream resource Blocker[EffectType]) flatMap { blocker =>
        io.file readAll [EffectType] (path, blocker, 1024 * 1042)
      } through text.utf8Decode through text.lines
  }

  /** */
  private trait MemFileImplKV[F[_], V] extends MemFileImplV[F, WithKey.AuxK, V] {
    self: KeyValueRepository[F, V] =>

    import V._

    /** */
    private var kvs: Table = Map.empty

    /** TODO: awkward impl Option[?] ~> Stream[F, ?] ought to be a thing, right? */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def get(k: Key): EffectStream[Value] =
      (kvs get k).fold(Stream apply [EffectType, Value] ()) { v =>
        Stream emit [EffectType, Value] v
      }

    /** */
    def update(row: Row): EffectStream[Unit] = ???

    /** FIXME - not clear how to memorialize this in a Row... */
    def delete(k: Key): EffectStream[Boolean] = ???

    /** */
    def insert(row: Row): EffectStream[Unit] = ???
  }

  /** */
  private sealed abstract case class MemFileValueRepository[F[_]: Sync: ContextShift, V: Eq](
      override val V: WithId.Aux[V]
  ) extends ValueOnlyRepository(V)
      with MemFileImplV[F, WithId.Aux, V]

  /** */
  private sealed abstract case class MemFileKeyValueRepository[F[_]: Sync: ContextShift, V: Eq](
      override val V: WithKey.AuxK[V]
  ) extends KeyValueRepository(V)
      with MemFileImplKV[F, V]

  /** */
  def valueRepository[F[_]: Sync: ContextShift, V: Eq](
      v: WithId.Aux[V],
      p: String
  ): Result[ValueOnlyRepository[F, V]] = Result safe {
    new MemFileValueRepository(v) { override def path = Paths get p }
  }

  /** */
  def keyValueRepository[F[_]: Sync: ContextShift, V: Eq](
      v: WithKey.AuxK[V],
      p: String
  ): Result[KeyValueRepository[F, V]] = Result safe {
    new MemFileKeyValueRepository(v) { override def path = Paths get p }
  }
}

object repos extends repos
