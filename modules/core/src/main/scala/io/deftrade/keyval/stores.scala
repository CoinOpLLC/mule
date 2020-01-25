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

import shapeless.{ ::, HList, HNil, LabelledGeneric, Lazy }
import shapeless.labelled._

import eu.timepit.refined
import refined.cats.refTypeOrder

import fs2.{ text, Pipe, Stream }
import fs2.io.file.{ pulls, FileHandle }

import io.chrisdavenport.cormorant
import cormorant.{ CSV, Error, Get, LabelledRead, LabelledWrite, Printer, Put }
import cormorant.generic.semiauto._
// import cormorant.refined._
import cormorant.fs2.{ readLabelledCompleteSafe, writeLabelled }

import java.nio.file.{ Path, Paths, StandardOpenOption => OpenOption }

/**
  *
  */
trait stores {

  private val errorToFail: Error => Fail = Fail fromThrowable "csv failure"

  /** */
  protected trait ModuleTypes {

    /** */
    type ValueType

    /** */
    type ValueCompanionType[x] <: WithValue

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
  protected[deftrade] object ModuleTypes {

    /** */
    abstract class Aux[F[_], W[_] <: WithValue, V, HV <: HList](
        val V: W[V]
    )(
        implicit
        final override val F: Sync[F],
        final override val X: ContextShift[F],
        override val lgv: LabelledGeneric.Aux[V, HV]
    ) extends ModuleTypes {

      final override type ValueCompanionType[x] = W[x]
      final type HValue                         = HV // === lgv.Repr  test / validate / assume ?!?
      final type EffectType[x]                  = F[x]
      final type ValueType                      = V

      import V.{ Id, Index, Value }

      /** Basic in-memory table structure */
      final type Table = Map[Index, Value]
      final implicit lazy val putId: Put[Id] = Put[Id]
      final implicit lazy val getId: Get[Id] = Get[Id]
    }
  }

  /** */
  protected trait Store[F[_], W[_] <: WithValue, V, HV <: HList] {
    self: ModuleTypes.Aux[F, W, V, HV] =>

    import V._

    /**  */
    type HRow <: HList

    /**  */
    final type HPermRow = IdField :: HRow

    /**
      * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
      *
      * Note: not distinguishing between `not found` and `IO error`
      * TODO: This needs to evolve.
      */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def permRows: EffectStream[PermRow] =
      (readLines through csvToPermRow).rethrow handleErrorWith (_ => Stream.empty)

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def rows: EffectStream[Row] = permRows map (_._2)

    /**  */
    final def filter(predicate: Row => Boolean): EffectStream[Row] = rows filter predicate

    /**  @return a `Stream` of length zero or one. */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def get(id: Id): EffectStream[Row] =
      permRows filter (_._1 === id) map (_._2)

    /** FIXME: not thread safe, put a queue in front of single thread-contained appender */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def append(row: Row): EffectStream[Id] =
      for {
        id <- Stream eval F.delay { Id(rawId.getAndIncrement) }
        _ <- Stream eval F.delay {
              updateCache(row); id -> row
            } through permRowToCSV through appendingSink
      } yield id

    /** When writing whole `Map`s, all rows get the same `Id`. */
    final def appendAll[G[_]: cats.Foldable, K2, V2](
        rows: G[Row]
    )(
        implicit
        ev: V <:< (K2, V2)
    ): EffectStream[Id] = ???

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
    *
    * Cormorant CSV is integrated by implementing implicit methods
    * [[writePermRow]], [[readPermRow]], providing access to
    * [[io.chrisdavenport.cormorant.LabelledRead]] and
    * [[io.chrisdavenport.cormorant.LabelledWrite]]
    * instances for the [[WithValue.PermRow]] type for this store.
    */
  trait ValueStore[F[_], V, HV <: HList] extends Store[F, WithId, V, HV] {
    self: ModuleTypes.Aux[F, WithId, V, HV] =>

    import V._

    /** */
    final type HRow = HValue

    /** */
    implicit final def writePermRow(
        implicit
        llw: Lazy[LabelledWrite[HValue]]
    ): LabelledWrite[PermRow] =
      new LabelledWrite[PermRow] {
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
        implicit val lrhpr = LabelledRead[HPermRow]
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, PermRow] =
          lrhpr read (row, headers) map { hpr =>
            (hpr.head, lgv from hpr.tail)
          }
      }

    /** */
    final protected def deriveCsvToV(
        implicit
        llr: Lazy[LabelledRead[HV]]
    ): Pipe[EffectType, String, Result[PermRow]] =
      readLabelledCompleteSafe[F, PermRow] andThen
        (_ map (_ leftMap errorToFail))

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final protected def deriveVToCsv(
        implicit
        llw: Lazy[LabelledWrite[HV]]
    ): Pipe[EffectType, PermRow, String] =
      writeLabelled(printer)
  }

  /**  */
  final def printer: Printer = Printer.default

  /**  */
  trait KeyValueStore[F[_], K, V, HV <: HList] extends Store[F, WithKey.Aux[K, *], V, HV] {
    self: ModuleTypes.Aux[F, WithKey.Aux[K, *], V, HV] =>

    import V._

    /** */
    final type HRow = KeyField :: HValue

    /** */
    final type HEmptyRow = IdField :: KeyField :: HNil

    /** */
    def select(key: Key): EffectStream[Value]

    /** */
    def insert(key: Key, value: Value): EffectStream[Id]

    /** */
    def update(key: Key, value: Value): EffectStream[Id]

    /** */
    def delete(key: Key): EffectStream[Id]

    /** */
    final def upsert(key: Key, value: Value): EffectStream[Id] = append(key -> value.some)

    /** FIXME: reimplement */
    implicit final def writePermRow(
        implicit
        llw: Lazy[LabelledWrite[HValue]],
        lputk: Lazy[Put[Key]]
    ): LabelledWrite[PermRow] =
      new LabelledWrite[PermRow] {

        LabelledWrite[IdField :: HValue] |> discardValue

        implicit val lwhpr = LabelledWrite[HPermRow]
        implicit val lwher = LabelledWrite[HEmptyRow]

        def headers: CSV.Headers = lwhpr.headers

        def write(pr: PermRow): CSV.Row = pr match {
          case (i, (k, Some(v))) => lwhpr write field[id.T](i) :: field[key.T](k) :: (lgv to v)
          case (i, (k, None))    => lwher write field[id.T](i) :: field[key.T](k) :: HNil
        }
      }

    /** FIXME: reimplement */
    implicit final def readPermRow(
        implicit
        llr: Lazy[LabelledRead[HV]],
        lgetk: Lazy[Get[Key]]
    ): LabelledRead[PermRow] = ???
    // new LabelledRead[PermRow] {
    //
    //   implicit val lrhv  = llr.value
    //   implicit val lrhpr = LabelledRead[HPermRow]
    //
    //   def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, PermRow] =
    //     row match {
    //
    //       case CSV.Row(
    //           NonEmptyList(CSV.Field(i), List(CSV.Field(k)))
    //           ) if i === id.toString && k === key.toString =>
    //         lrhpr read (row, headers) map { hpr =>
    //           (hpr.head, (hpr.tail.head, none))
    //         }
    //
    //       case _ =>
    //         lrhpr read (row, headers) map { hpr =>
    //           (hpr.head, (hpr.tail.head, (lgv from hpr.tail.tail).some))
    //         }
    //     }
    // }

    /** */
    final protected def deriveCsvToKv(
        implicit
        llr: Lazy[LabelledRead[HV]],
        lgetk: Lazy[Get[Key]]
    ): Pipe[EffectType, String, Result[PermRow]] =
      readLabelledCompleteSafe[F, PermRow] andThen
        (_ map (_ leftMap errorToFail))

    /** */
    final protected def deriveKvToCsv(
        implicit
        llw: Lazy[LabelledWrite[HV]],
        lputk: Lazy[Put[Key]]
    ): Pipe[EffectType, PermRow, String] =
      writeLabelled[F, PermRow](printer)
  }

  /** */
  private trait MemFileImplV[F[_], W[_] <: WithValue, V, HV <: HList] extends Store[F, W, V, HV] {
    self: ModuleTypes.Aux[F, W, V, HV] =>

    /** */
    final protected var table: Table = Map.empty

    /** */
    def path: Path

    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final private lazy val appendHandles: EffectStream[FileHandle[EffectType]] = {

      import OpenOption._

      val openOptions = Seq(
        CREATE,
        // READ,
        WRITE,
        APPEND,
        SYNC,
        // DSYNC,
      )

      Stream resource (for {
        blocker <- Blocker[EffectType]
        handle  <- FileHandle fromPath (path, blocker, openOptions)
      } yield handle)
    }

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final protected def appendingSink: Pipe[EffectType, String, Unit] =
      for {
        handle <- appendHandles
        s      <- _
      } yield
        pulls
          .writeAllToFileHandle(
            Stream eval
              (F pure s) through
              text.utf8Encode,
            handle
          )
          .stream |> discardValue // nota bene this is intentional and necessary

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final protected def readLines: EffectStream[String] =
      (Stream resource Blocker[EffectType]) flatMap { blocker =>
        fs2.io.file readAll [EffectType] (path, blocker, 1024 * 1024)
      } through
        text.utf8Decode through
        text.lines
  }

  /** */
  private trait MemFileImplKV[F[_], K, V, HV <: HList]
      extends MemFileImplV[
        F,
        WithKey.Aux[K, *],
        V,
        HV
      ] {
    self: ModuleTypes.Aux[F, WithKey.Aux[K, *], V, HV] =>

    import V._

    /** */
    final def select(key: Key): EffectStream[Value] =
      Stream evals F.delay { table get key }

    /** */
    def insert(key: Key, value: Value): EffectStream[Id] =
      (table get key).fold(append(key -> value.some))(_ => Stream.empty)

    /** */
    def update(key: Key, value: Value): EffectStream[Id] =
      table get key match { // can't fold and get good type inference
        case None    => Stream.empty
        case Some(_) => append(key -> value.some)
      }

    /** Empty `Value` memorializes (persists) `delete` for a given `key` - this row gets an `Id`! */
    def delete(key: Key): EffectStream[Id] =
      (table get key)
        .fold(Stream.empty: EffectStream[Id])(_ => append(key -> none))
  }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private sealed abstract case class MemFileValueStore[
      F[_]: Sync: ContextShift,
      V: Eq,
      HV <: HList
  ](
      final override val V: WithId[V]
  )(
      implicit
      final override val lgv: LabelledGeneric.Aux[V, HV],
  ) extends ModuleTypes.Aux(V)
      with ValueStore[F, V, HV]
      with MemFileImplV[F, WithId, V, HV]

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
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def valueStore[F[_]: Sync: ContextShift, V: Eq, HV <: HList](
      v: WithId[V],
      p: String
  )(
      implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]
  ): Result[ValueStore[F, V, HV]] = Result safe {
    new MemFileValueStore(v) {

      import V._

      /** */
      override def path = Paths get p

      /** */
      final lazy val permRowToCSV: Pipe[EffectType, PermRow, String] = deriveVToCsv

      /** */
      final lazy val csvToPermRow: Pipe[EffectType, String, Result[PermRow]] = deriveCsvToV
    }
  }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
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
