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

import syntax._, refinements.IsSha

import cats.implicits._
import cats.Eq
import cats.data.NonEmptyList

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
protected trait CsvStoreTypes extends StoreTypes {

  /** */
  type HValue <: HList

  /** */
  implicit val lgv: LabelledGeneric.Aux[ValueType, HValue]
}

/** */
protected[deftrade] object CsvStoreTypes {

  /** */
  abstract class Aux[F[_], W[_] <: WithValue, V, HV <: HList](
      override val V: W[V]
  )(
      implicit
      final override val F: Sync[F],
      final override val X: ContextShift[F],
      val lgv: LabelledGeneric.Aux[V, HV]
  ) extends StoreTypes.Aux[F, W, V](V) {

    final type HValue = HV // === lgv.Repr  test / validate / assume ?!?

    final implicit lazy val putId: Put[V.Id] = Put[V.Id]
    final implicit lazy val getId: Get[V.Id] = Get[V.Id]
  }
}

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
protected trait CsvStore[
    F[_],
    W[_] <: WithValue,
    V,
    HV <: HList
] extends Store[F, W, V] {
  self: CsvStoreTypes.Aux[F, W, V, HV] =>

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
  final def idRows: EffectStream[(Id, Row)] =
    (readLines through csvToPermRow).rethrow handleErrorWith (_ => Stream.empty)

  /** */
  final def appendAll(row: Row, rows: Row*): EffectStream[Id] =
    for {
      id <- Stream eval F.delay { fresh.nextAll(prev, row, rows: _*) }
      r  <- Stream evals F.delay { (row +: rows).toList }
      _ <- Stream eval F.delay { updateCache(r); (id, r) } through
            permRowToCSV through
            appendingSink
    } yield id

  /** Default no-op imlementation. */
  protected def updateCache(row: Row) = ()

  /** */
  protected def readLines: EffectStream[String]

  /** */
  protected def appendingSink: Pipe[Effect, String, Unit]

  /** */
  protected def permRowToCSV: Pipe[Effect, (Id, Row), String]

  /** */
  protected def csvToPermRow: Pipe[Effect, String, Result[(Id, Row)]]
}

/**
  * Ctor parameter `V` carries types specific to `type V`.
  * `type Index` mapped to [[WithValue.Id]].
  *
  * Cormorant CSV is integrated by implementing implicit methods
  * [[writePermRow]], [[readPermRow]], providing access to
  * [[io.chrisdavenport.cormorant.LabelledRead]] and
  * [[io.chrisdavenport.cormorant.LabelledWrite]]
  * instances for the [[WithValue.(Id, Row)]] type for this store.
  */
trait CsvValueStore[
    F[_],
    V,
    HV <: HList
] extends CsvStore[F, WithId, V, HV] {
  self: CsvStoreTypes.Aux[F, WithId, V, HV] =>

  import V._

  /** */
  final type HRow = HValue

  /** */
  protected def indexFrom(pr: (Id, Row)): Index =
    pr match {
      case (id, _) => id
    }

  /** */
  implicit final def writePermRow(
      implicit
      llw: Lazy[LabelledWrite[HValue]]
  ): LabelledWrite[(Id, Row)] =
    new LabelledWrite[(Id, Row)] {
      implicit val lwhpr       = LabelledWrite[HPermRow]
      def headers: CSV.Headers = lwhpr.headers
      override def write(pr: (Id, Row)): CSV.Row = pr match {
        case (i, v) =>
          lwhpr write field[id.T](i) :: (lgv to v)
      }
    }

  /** */
  implicit final def readPermRow(
      implicit
      llr: Lazy[LabelledRead[HV]]
  ): LabelledRead[(Id, Row)] =
    new LabelledRead[(Id, Row)] {
      implicit val lrhpr = LabelledRead[HPermRow]
      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, (Id, Row)] =
        lrhpr read (row, headers) map { hpr =>
          (hpr.head, lgv from hpr.tail)
        }
    }

  /** */
  final protected def deriveCsvToV(
      implicit
      llr: Lazy[LabelledRead[HV]]
  ): Pipe[Effect, String, Result[(Id, Row)]] =
    readLabelledCompleteSafe[F, (Id, Row)] andThen
      (_ map (_ leftMap errorToFail))

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveVToCsv(
      implicit
      llw: Lazy[LabelledWrite[HV]]
  ): Pipe[Effect, (Id, Row), String] = writeLabelled(printer)
}

/**  */
trait CsvKeyValueStore[
    F[_],
    K,
    V,
    HV <: HList
] extends KeyValueStore[F, K, V]
    with CsvStore[F, WithKey.Aux[K, *], V, HV] {
  self: CsvStoreTypes.Aux[F, WithKey.Aux[K, *], V, HV] =>

  import V._

  /** */
  final type HRow = KeyField :: HValue

  /** */
  final type HEmptyRow = IdField :: KeyField :: HNil

  // /** */
  // protected def valueFrom(row: Row): Value =
  //   row match {
  //     case (_, Some(value)) => value
  //   }

  /**  */
  implicit final def writePermRow(
      implicit
      llw: LabelledWrite[HValue],
      putk: Put[Key]
  ): LabelledWrite[(Id, Row)] =
    new LabelledWrite[(Id, Row)] {

      private val lwhpr = LabelledWrite[HPermRow]
      private val lwher = LabelledWrite[HEmptyRow]

      def headers: CSV.Headers = lwhpr.headers

      def write(pr: (Id, Row)): CSV.Row = pr match {
        case (i, (k, Some(v))) => lwhpr write field[id.T](i) :: field[key.T](k) :: (lgv to v)
        case (i, (k, None))    => lwher write field[id.T](i) :: field[key.T](k) :: HNil
      }
    }

  /** */
  implicit final def readPermRow(
      implicit
      llr: LabelledRead[HV],
      getk: Get[Key]
  ): LabelledRead[(Id, Row)] =
    new LabelledRead[(Id, Row)] {

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, (Id, Row)] = {

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
  ): Pipe[Effect, String, Result[(Id, Row)]] = {
    implicit def lrhv = llr.value
    implicit def getk = lgetk.value
    readLabelledCompleteSafe[F, (Id, Row)] andThen
      (_ map (_ leftMap errorToFail))
  }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveKvToCsv(
      implicit
      llw: Lazy[LabelledWrite[HV]],
      lputk: Lazy[Put[Key]]
  ): Pipe[Effect, (Id, Row), String] = {

    implicit def lwhv: LabelledWrite[HV] = llw.value
    implicit def putk: Put[Key]          = lputk.value
    writeLabelled(printer)
  }
}

/** */
protected trait MemFileImplV[F[_], W[_] <: WithValue, V, HV <: HList] extends CsvStore[F, W, V, HV] {
  self: CsvStoreTypes.Aux[F, W, V, HV] =>

  /** */
  final protected var table: Map[V.Index, V.Value] = Map.empty

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
    ] { self: CsvStoreTypes.Aux[F, WithKey.Aux[K, *], V, HV] =>

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
      case Some(_) => append(key -> value.some)
      case None    => Stream.empty
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
) extends CsvStoreTypes.Aux(V)
    with CsvValueStore[F, V, HV]
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
) extends CsvStoreTypes.Aux(V)
    with CsvKeyValueStore[F, K, V, HV]
    with MemFileImplKV[F, K, V, HV]
