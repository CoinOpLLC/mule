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
import cats.Eq
import cats.effect.{ Blocker, ContextShift, Sync }

import shapeless.{ ::, HList, LabelledGeneric, Lazy }
import shapeless.labelled._

import eu.timepit.refined
import refined.api.Validate

import fs2.{ io, text, Pipe, Stream }

import _root_.io.chrisdavenport.cormorant
import cormorant._
import cormorant.implicits._
import cormorant.generic.semiauto._
import cormorant.refined._
import cormorant.fs2._

import scala.language.higherKinds

import java.nio.file.{ Path, Paths }

/**
  *
  */
trait stores {

  private val errorToFail: Error => Fail = e => Fail("csv failure", e)

  /** */
  protected trait ModuleTypes {

    /** */
    type ValueCompanionType[x] <: WithValue.Aux[x]

    /** */
    type ValueType

    /** */
    type HValue <: HList

    /** */
    implicit val lgv: LabelledGeneric.Aux[ValueType, HValue]

    /** */
    type EffectType[_]

    /** */
    implicit def F: Sync[EffectType]

    /** */
    implicit def X: ContextShift[EffectType]

    /** */
    final type EffectStream[x] = Stream[EffectType, x]
  }

  /** */
  protected object ModuleTypes {

    /** */
    abstract class Aux[
        F[_],
        W[?] <: WithValue.Aux[?],
        V,
        HV <: HList
    ](
        val V: W[V]
    )(
        implicit
        final override val F: Sync[F],
        final override val X: ContextShift[F],
        override val lgv: LabelledGeneric.Aux[V, HV]
    ) extends ModuleTypes {

      final type ValueCompanionType[x] = W[x]

      final type ValueType = V.Value
      final type HValue    = HV // = lgv.Repr ?!?

      final type EffectType[x] = F[x]

      /** Basic in-memory table structure */
      final type Table = Map[V.Index, V.Value]
      implicit def validateId: Validate[Long, V.Value] = ???
      final implicit lazy val putId                    = Put[V.Id]
      final implicit lazy val getId                    = Get[V.Id]
    }
  }

  /** */
  protected trait Store[
      F[_],
      W[?] <: WithValue.Aux[?],
      V,
      HV <: HList
  ] { self: ModuleTypes.Aux[F, W, V, HV] =>

    import V._

    /**  */
    type HRow <: HList

    /**  */
    final type HPermRow = IdField :: HRow

    /**  */
    final def filter(predicate: Row => Boolean): EffectStream[Row] = rows filter predicate

    /** FIXME - `rethrow`(s)*/
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def rows: EffectStream[Row] = permRows.rethrow map (_._2)
    // EitherT[EffectStream, Fail, Row]

    /**  @return a `Stream` of length zero or one. */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def get(id: Id): EffectStream[Row] = permRows.rethrow filter (_._1 === id) map (_._2)

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def permRows: EffectStream[Result[PermRow]] = readLines through csvToPermRow

    /** FIXME: not thread safe, put a queue in front of single thread-contained appender */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def append(row: Row): EffectStream[Id] =
      for {
        id <- Stream eval { F delay { Id(rawId.getAndIncrement) } }
        _  <- Stream eval { F delay { id -> row } } through permRowToCSV through appendingSink
        _  <- Stream eval { F delay updateCache(row) }
      } yield id

    /** */
    protected def readLines: EffectStream[String]

    /** */
    protected def appendingSink: Pipe[EffectType, String, Unit]

    /** */
    protected def updateCache(row: Row) = ()

    /** */
    protected def permRowToCSV: Pipe[EffectType, PermRow, String]

    /** */
    protected def csvToPermRow: Pipe[EffectType, String, Result[PermRow]]

    private lazy val rawId = new java.util.concurrent.atomic.AtomicLong(fresh.init().value)

    private lazy val fresh: Fresh[Id] = Fresh.zeroBasedIncr
  }

  /**
    * Ctor parameter `V` carries types specific to `type V`.
    * `type Index` mapped to [[WithValue.Id]].
    */
  trait ValueStore[F[_], V, HV <: HList] extends Store[F, WithId.Aux, V, HV] {
    self: ModuleTypes.Aux[F, WithId.Aux, V, HV] =>

    import V._

    final type HRow = HValue

    implicit final def writePermRow(
        implicit
        llw: Lazy[LabelledWrite[HValue]]
    ): LabelledWrite[PermRow] =
      new LabelledWrite[PermRow] {
        implicit val lwhv        = llw.value
        implicit val lwhpr       = LabelledWrite[HPermRow]
        def headers: CSV.Headers = lwhpr.headers
        override def write(pr: PermRow): CSV.Row = pr match {
          case (i, v) =>
            lwhpr write field[id.T](i) :: (lgv to v)
        }
      }

    /** */
    implicit final def readPermRow(
        implicit
        llr: Lazy[LabelledRead[HV]]
    ): LabelledRead[PermRow] =
      new LabelledRead[PermRow] {
        implicit val lrhv  = llr.value
        implicit val lrhpr = LabelledRead[HPermRow]
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, PermRow] =
          lrhpr read (row, headers) map { h =>
            (h.head, lgv from h.tail)
          }
      }

    final protected def deriveCsvToV(
        implicit
        llr: Lazy[LabelledRead[HV]]
    ): Pipe[EffectType, String, Result[PermRow]] =
      readLabelledCompleteSafe[F, PermRow] andThen (_ map (_ leftMap errorToFail))

    final protected def deriveVToCsv(
        implicit
        llw: Lazy[LabelledWrite[HV]]
    ): Pipe[EffectType, PermRow, String] = writeLabelled[F, PermRow](printer)
  }
  private def printer: Printer = ???

  /**  */
  trait KeyValueStore[
      F[_],
      K,
      V,
      HV <: HList
  ] extends Store[
        F,
        ({ type W[v] = WithKey.Aux[K, v] })#W,
        V,
        HV
      ] {
    self: ModuleTypes.Aux[F, ({ type W[v] = WithKey.Aux[K, v] })#W, V, HV] =>

    import V._

    /** */
    final type HRow = KeyField :: HValue

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

    implicit final def writePermRow(
        implicit
        llw: Lazy[LabelledWrite[HValue]],
        lputk: Lazy[Put[Key]]
    ): LabelledWrite[PermRow] =
      new LabelledWrite[PermRow] {
        implicit val lwhv        = llw.value
        implicit val lwhpr       = LabelledWrite[HPermRow]
        def headers: CSV.Headers = lwhpr.headers
        def write(pr: PermRow): CSV.Row = pr match {
          case (i, (k, v)) =>
            lwhpr write field[id.T](i) :: field[key.T](k) :: (lgv to v)
        }
      }

    /** */
    implicit final def readPermRow(
        implicit
        llr: Lazy[LabelledRead[HV]],
        lgetk: Lazy[Get[Key]]
    ): LabelledRead[PermRow] =
      new LabelledRead[PermRow] {
        implicit val lrhv  = llr.value
        implicit val lrhpr = LabelledRead[HPermRow]
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, PermRow] =
          lrhpr read (row, headers) map { h =>
            (h.head, (h.tail.head, lgv from h.tail.tail))
          }
      }

    /** */
    final protected def deriveCsvToKv(
        implicit
        llr: Lazy[LabelledRead[HV]],
        lgetk: Lazy[Get[Key]]
    ): Pipe[EffectType, String, Result[PermRow]] =
      readLabelledCompleteSafe[F, PermRow] andThen (_ map (_ leftMap errorToFail))

    /** */
    protected final def deriveKvToCsv(
        implicit
        llw: Lazy[LabelledWrite[HV]],
        lputk: Lazy[Put[Key]]
    ): Pipe[EffectType, PermRow, String] = { prs =>
      ???
    }
  }

  /** */
  private trait MemFileImplV[
      F[_],
      W[?] <: WithValue.Aux[?],
      V,
      HV <: HList
  ] extends Store[F, W, V, HV] { self: ModuleTypes.Aux[F, W, V, HV] =>

    /** */
    def path: Path

    /** */
    final protected var table: Table = Map.empty

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final protected def appendingSink: Pipe[EffectType, String, Unit] =
      fs =>
        for {
          s <- fs
          u <- Stream emit s |> { _ =>
                ()
              }
        } yield u

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final protected def readLines: EffectStream[String] =
      (Stream resource Blocker[EffectType]) flatMap { blocker =>
        io.file readAll [EffectType] (path, blocker, 1024 * 1042)
      } through text.utf8Decode through text.lines
  }

  /** */
  private trait MemFileImplKV[
      F[_],
      K,
      V,
      HV <: HList
  ] extends MemFileImplV[
        F,
        ({ type W[v] = WithKey.Aux[K, v] })#W,
        V,
        HV
      ] {
    self: ModuleTypes.Aux[F, ({ type W[v] = WithKey.Aux[K, v] })#W, V, HV] =>

    import V._

    /** */
    final def get(k: Key): EffectStream[Value] = Stream evals (F delay (table get k))

    /** */
    def update(row: Row): EffectStream[Unit] = ???

    /** FIXME - not clear how to memorialize this in a Row... */
    def delete(k: Key): EffectStream[Boolean] = ???

    /** */
    def insert(row: Row): EffectStream[Unit] = ???
  }

  /** */
  private sealed abstract case class MemFileValueStore[
      F[_]: Sync: ContextShift,
      V: Eq,
      HV <: HList
  ](
      final override val V: WithId.Aux[V]
  )(
      implicit
      final override val lgv: LabelledGeneric.Aux[V, HV],
  ) extends ModuleTypes.Aux[F, WithId.Aux, V, HV](V)
      with ValueStore[F, V, HV]
      with MemFileImplV[F, WithId.Aux, V, HV]

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private sealed abstract case class MemFileKeyValueStore[
      F[_]: Sync: ContextShift,
      K,
      V: Eq,
      HV <: HList
  ](
      final override val V: WithKey.Aux[K, V]
  )(
      implicit
      final override val lgv: LabelledGeneric.Aux[V, HV],
  ) extends ModuleTypes.Aux(V)
      with KeyValueStore[F, K, V, HV]
      with MemFileImplKV[F, K, V, HV]

  /** */
  def valueStore[F[_]: Sync: ContextShift, V: Eq, HV <: HList](
      V: WithId.Aux[V],
      p: String
  )(
      implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]
  ): Result[ValueStore[F, V, HV]] = Result safe {
    new MemFileValueStore(V) {

      /** */
      override def path = Paths get p

      /** */
      final lazy val permRowToCSV: Pipe[EffectType, V.PermRow, String] = deriveVToCsv

      /** */
      final lazy val csvToPermRow: Pipe[EffectType, String, Result[V.PermRow]] = deriveCsvToV
    }
  }

  /** */
  def keyValueStore[F[_]: Sync: ContextShift, K, V: Eq, HV <: HList](
      kv: WithKey.Aux[K, V],
      p: String
  )(
      implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]],
      lgetk: Lazy[Get[K]],
      lputk: Lazy[Put[K]]
  ): Result[KeyValueStore[F, K, V, HV]] = Result safe {
    new MemFileKeyValueStore(kv) { self =>

      /** */
      override def path = Paths get p

      /** */
      final lazy val permRowToCSV: Pipe[EffectType, V.PermRow, String] = deriveKvToCsv

      /** */
      final lazy val csvToPermRow: Pipe[EffectType, String, Result[V.PermRow]] = deriveCsvToKv
    }
  }
}

object stores extends stores
