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

import implicits._, refinements.IsSha

import cats.implicits._
import cats.Eq
import cats.data.NonEmptyList
import cats.evidence._

import cats.effect.{ Blocker, ContextShift, Sync }

import shapeless.{ ::, HList, HNil, LabelledGeneric, Lazy }
import shapeless.labelled._

import eu.timepit.refined
import refined.api.Refined
import refined.cats.refTypeOrder

import fs2.{ text, Pipe, Stream }
import fs2.io.file.{ pulls, FileHandle, ReadCursor, WriteCursor }

import scodec.bits.ByteVector

import io.chrisdavenport.cormorant
import cormorant.{ CSV, Error, Get, LabelledRead, LabelledWrite, Put }
import cormorant.generic.semiauto._
// import cormorant.refined._
import cormorant.fs2.{ readLabelledCompleteSafe, writeLabelled }

import java.nio.file.{ Path, StandardOpenOption => OpenOption }

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
  type Effect[_]

  /** */
  implicit def F: Sync[Effect]

  /** */
  implicit def X: ContextShift[Effect]

  /** */
  final type EffectStream[x] = Stream[Effect, x]
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
    final type Effect[x]                      = F[x]
    final type ValueType                      = V

    import V.{ Id, Index, Value }

    /** Basic in-memory table structure */
    final type Table = Map[Index, Value]

    final implicit lazy val putId: Put[Id] = Put[Id]
    final implicit lazy val getId: Get[Id] = Get[Id]
  }
}

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
protected trait Store[F[_], W[_] <: WithValue, V, HV <: HList] {
  self: ModuleTypes.Aux[F, W, V, HV] =>

  import V._

  /**  */
  type HRow <: HList

  /**  */
  final type HPermRow = IdField :: HRow

  /** */
  protected def indexFrom(pr: PermRow): Index

  /** */
  protected def valueFrom(row: Row): Value

  /**  table is in memory */
  protected def tableRows: EffectStream[(Index, Value)]

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * Note: not distinguishing between `not found` and `IO error`
    * TODO: This needs to evolve.
    */
  final def permRows: EffectStream[PermRow] =
    (readLines through csvToPermRow).rethrow handleErrorWith (_ => Stream.empty)

  /** */
  final def rows: EffectStream[Row] = permRows map (_._2)
  //
  // /**  */
  // final def filter(predicate: Row => Boolean): EffectStream[Row] = rows filter predicate

  /**  @return a `Stream` of length zero or one. */
  final def get(id: Id): EffectStream[Row] =
    permRows filter (_._1 === id) map (_._2)

  /**  */
  final def getAll(x: Index): EffectStream[Value] =
    permRows
      .filter(pr => indexFrom(pr) == x)
      .map(pr => valueFrom(pr._2))

  /**  */
  final def getList(x: Index): EffectStream[List[Value]] =
    getAll(x)
      .fold(List.empty[Value])((vs, v) => v :: vs)
      .map(_.reverse)

  /** FIXME: not thread safe, put a queue in front of single thread-contained appender */
  final def append(row: Row): EffectStream[Id] =
    appendAll(row)

  /** */
  final def appendAll(row: Row, rows: Row*): EffectStream[Id] =
    for {
      id <- Stream eval F.delay { fresh.nextAll(prev, row, rows: _*) }
      r  <- Stream evals F.delay { (row +: rows).toList }
      _ <- Stream eval F.delay { updateCache(r); (id, r) } through
            permRowToCSV through
            appendingSink
    } yield id

  /** */
  protected def readLines: EffectStream[String]

  /** */
  protected def appendingSink: Pipe[Effect, String, Unit]

  /** Default no-op imlementation. */
  protected def updateCache(row: Row) = ()

  /** */
  protected def permRowToCSV: Pipe[Effect, PermRow, String]

  /** */
  protected def csvToPermRow: Pipe[Effect, String, Result[PermRow]]

  /** FIXME obviously... this works, not obvously, that's the problem */
  protected var prev: Id =
    Refined unsafeApply [String, IsSha] "7hereWazAPharmrHadADogNBingoWuzHizN4m3oB1NGo"

  /** */
  protected def fresh: Fresh[Id, Row]
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
  protected def indexFrom(pr: PermRow): Index =
    pr match {
      case (id, _) => id
    }

  /** */
  protected def valueFrom(row: Row): Value =
    row

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
  ): Pipe[Effect, String, Result[PermRow]] =
    readLabelledCompleteSafe[F, PermRow] andThen
      (_ map (_ leftMap errorToFail))

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveVToCsv(
      implicit
      llw: Lazy[LabelledWrite[HV]]
  ): Pipe[Effect, PermRow, String] = writeLabelled(printer)
}

/**  */
trait KeyValueStore[F[_], K, V, HV <: HList] extends Store[F, WithKey.Aux[K, *], V, HV] {
  self: ModuleTypes.Aux[F, WithKey.Aux[K, *], V, HV] =>

  import V._

  /** */
  final type HRow = KeyField :: HValue

  /** */
  final type HEmptyRow = IdField :: KeyField :: HNil

  /** */
  protected def indexFrom(pr: PermRow): Index =
    pr match {
      case (_, (key, _)) => key
    }

  /** */
  protected def valueFrom(row: Row): Value =
    row match {
      case (_, Some(value)) => value
    }

  /** */
  def select(key: Key): EffectStream[Value]

  /** */
  final def selectAll(key: Key): EffectStream[Value] =
    getAll(key)

  /** */
  final def selectList(key: Key): EffectStream[List[Value]] =
    getList(key)

  /** */
  def insert(key: Key, value: Value): EffectStream[Id]

  /** */
  def update(key: Key, value: Value): EffectStream[Id]

  /** */
  def delete(key: Key): EffectStream[Id]

  /** */
  final def upsert(key: Key, value: Value): EffectStream[Id] =
    append(key -> value.some)

  /** */
  final def upsertAll(key: Key, value: Value, values: Key*): EffectStream[Id] =
    append(key -> value.some)

  /**  */
  implicit final def writePermRow(
      implicit
      llw: LabelledWrite[HValue],
      putk: Put[Key]
  ): LabelledWrite[PermRow] =
    new LabelledWrite[PermRow] {

      private val lwhpr = LabelledWrite[HPermRow]
      private val lwher = LabelledWrite[HEmptyRow]

      def headers: CSV.Headers = lwhpr.headers

      def write(pr: PermRow): CSV.Row = pr match {
        case (i, (k, Some(v))) => lwhpr write field[id.T](i) :: field[key.T](k) :: (lgv to v)
        case (i, (k, None))    => lwher write field[id.T](i) :: field[key.T](k) :: HNil
      }
    }

  /** */
  implicit final def readPermRow(
      implicit
      llr: LabelledRead[HV],
      getk: Get[Key]
  ): LabelledRead[PermRow] =
    new LabelledRead[PermRow] {

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, PermRow] = {

        val lrhpr = LabelledRead[HPermRow]

        row match {

          case CSV.Row(
              NonEmptyList(CSV.Field(i), List(CSV.Field(k)))
              ) if i === id.toString && k === key.toString =>
            lrhpr read (row, headers) map { hpr =>
              (hpr.head, (hpr.tail.head, none))
            }

          case _ =>
            lrhpr read (row, headers) map { hpr =>
              (hpr.head, (hpr.tail.head, (lgv from hpr.tail.tail).some))
            }
        }
      }
    }

  /** */
  final protected def deriveCsvToKv(
      implicit
      llr: Lazy[LabelledRead[HV]],
      lgetk: Lazy[Get[Key]]
  ): Pipe[Effect, String, Result[PermRow]] = {
    implicit def lrhv = llr.value
    implicit def getk = lgetk.value
    readLabelledCompleteSafe[F, PermRow] andThen
      (_ map (_ leftMap errorToFail))
  }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveKvToCsv(
      implicit
      llw: Lazy[LabelledWrite[HV]],
      lputk: Lazy[Put[Key]]
  ): Pipe[Effect, PermRow, String] = {

    implicit def lwhv: LabelledWrite[HV] = llw.value
    implicit def putk: Put[Key]          = lputk.value
    writeLabelled(printer)
  }
}

/** */
protected trait MemFileImplV[F[_], W[_] <: WithValue, V, HV <: HList] extends Store[F, W, V, HV] {
  self: ModuleTypes.Aux[F, W, V, HV] =>

  /** */
  final protected var table: Table = Map.empty

  /** */
  def path: Path

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final private lazy val appendHandles: EffectStream[FileHandle[Effect]] = {

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
      blocker <- Blocker[Effect]
      handle  <- FileHandle fromPath (path, blocker, openOptions)
    } yield handle)
  }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def appendingSink: Pipe[Effect, String, Unit] =
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
  // WriteCursor(out, 0).writeAll(in).void

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def readLines: EffectStream[String] =
    (Stream resource Blocker[Effect]) flatMap { blocker =>
      fs2.io.file readAll [Effect] (path, blocker, 1024 * 1024)
    } through
      text.utf8Decode through
      text.lines
}

/** */
protected trait MemFileImplKV[F[_], K, V, HV <: HList]
    extends MemFileImplV[
      F,
      WithKey.Aux[K, *],
      V,
      HV
    ] { self: ModuleTypes.Aux[F, WithKey.Aux[K, *], V, HV] =>

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
protected abstract case class MemFileValueStore[
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
protected abstract case class MemFileKeyValueStore[
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
