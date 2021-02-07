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
package impl

import syntax._

import cats.implicits._
import cats.data.{ NonEmptyList }

import cats.effect.{ Blocker, ContextShift, Sync }

import shapeless.{ ::, HList, HNil, LabelledGeneric, Lazy }
import shapeless.labelled.field

import fs2.{ text, Pipe, Stream }
import fs2.io.file
import file.{ pulls, FileHandle }

// import scodec.bits.ByteVector

import io.chrisdavenport.cormorant
import cormorant.fs2.{ readLabelledCompleteSafe, writeLabelled }
import cormorant._
import cormorant.generic.semiauto._

import java.nio.file.{ Path, StandardOpenOption => OpenOption }

/** Value parameter `V` carries type members specific to `type V`.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed protected abstract class CsvStore[F[_], V](
    V: Stores[V]
) extends CsvImplicits {

  self: Stores[V]#Store[F] =>

  /** Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * Note: not distinguishing between `not found` and `IO error`
    * TODO: This needs to evolve.
    */
  final def records: Stream[F, Record] =
    (readLines through csvToRecord).rethrow handleErrorWith (_ => Stream.empty)

  /** csv
    */
  final protected def persist: Pipe[F, Record, Unit] = recordToCSV andThen appendingSink

  /**
    */
  protected def readLines: Stream[F, String]

  /**
    */
  protected def appendingSink: Pipe[F, String, Unit]

  /**
    */
  protected def recordToCSV: Pipe[F, Record, String]

  /**
    */
  protected def csvToRecord: Pipe[F, String, Result[Record]]
}

/**
  */
abstract class CsvValueStore[F[_], V](
    final val VS: ValueStores[V]
) extends CsvStore[F, V](VS) {

  self: ValueStores[V]#ValueStore[F] =>

  import VS.{ Id, IdField, Row }

  import cormorant.refined._
  import cormorant.implicits._

  protected implicit def idPut = Put[Id]
  protected implicit def idGet = Get[Id]

  /**
    */
  implicit final protected def readRecord[HV <: HList](
      implicit
      lgav: LabelledGeneric.Aux[Row, HV],
      llhv: Lazy[LabelledRead[HV]]
  ): LabelledRead[Record] =
    new LabelledRead[Record] {

      implicit val lrhr = LabelledRead[IdField :: HV]

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Record] =
        lrhr.read(row, headers) map { hr =>
          (hr.head, lgav from hr.tail)
        }
    }

  /**
    */
  final protected def deriveCsvDecoderV[HV <: HList](
      implicit
      lgav: LabelledGeneric.Aux[Row, HV],
      llhv: Lazy[LabelledRead[HV]]
  ): Pipe[F, String, Result[Record]] =
    readLabelledCompleteSafe[F, Record] andThen
      (_ map (_ leftMap errorToFail))

  /**
    */
  implicit def writeRecord[
      HV <: HList
  ](implicit
    lgav: LabelledGeneric.Aux[V, HV],
    llhv: Lazy[LabelledWrite[HV]]): LabelledWrite[Record] =
    new LabelledWrite[Record] {

      private val lwhpr = LabelledWrite[IdField :: HV]

      def headers: CSV.Headers = lwhpr.headers

      def write(pr: Record): CSV.Row =
        pr match {
          case (i, v) => lwhpr write field[id.T](i) :: (lgav to v)
        }
    }

  /**
    */
  final protected def deriveCsvEncoderV[HV <: HList](
      implicit
      lgav: LabelledGeneric.Aux[V, HV],
      llhv: Lazy[LabelledWrite[HV]]
  ): Pipe[F, Record, String] =
    writeLabelled[F, Record](printer)
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class CsvKeyValueStore[F[_], K: Get: Put, V](
    final val KVS: KeyValueStores[K, V]
// )(
//   implicit
//   override val F: Sync[F],
//   override val X: ContextShift[F]
) extends CsvStore[F, V](KVS) {

  self: KeyValueStores[K, V]#KeyValueStore[F] =>

  import KVS.{ IdField, Key, KeyField, Value }

  import cormorant.refined._
  import cormorant.implicits._

  /**
    */
  implicit def readRecord[
      HV <: HList
  ](implicit
    lgav: LabelledGeneric.Aux[Value, HV],
    llhv: Lazy[LabelledRead[HV]]): LabelledRead[Record] =
    new LabelledRead[Record] {

      val lrhr = LabelledRead[IdField :: KeyField :: HV]

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Record] =
        row match {

          case CSV.Row(
              NonEmptyList(CSV.Field(i), List(CSV.Field(k)))
              ) if i === id.toString && k === key.toString =>
            lrhr.read(row, headers) map { hr =>
              (hr.head, (hr.tail.head: Key, none[Value]))
            }

          case _ =>
            lrhr.read(row, headers) map { hr =>
              (hr.head, (hr.tail.head, (lgav from hr.tail.tail).some))
            }
        }
    }

  /**
    */
  final protected def deriveCsvDecoderKv[
      HV <: HList
  ](implicit
    lgav: LabelledGeneric.Aux[V, HV],
    llr: Lazy[LabelledRead[HV]]): Pipe[F, String, Result[Record]] =
    readLabelledCompleteSafe[F, Record] andThen
      (_ map (_ leftMap errorToFail))

  /**
    */
  implicit final def writeRecord[
      HV <: HList
  ](implicit
    lgav: LabelledGeneric.Aux[V, HV],
    llhv: Lazy[LabelledWrite[HV]]): LabelledWrite[Record] =
    new LabelledWrite[Record] {

      private val lwhpr = LabelledWrite[IdField :: KeyField :: HV]
      private val lwher = LabelledWrite[IdField :: KeyField :: HNil]

      def headers: CSV.Headers = lwhpr.headers

      def write(pr: Record): CSV.Row =
        pr match {
          case (i, (k, Some(v))) => lwhpr write field[id.T](i) :: field[key.T](k) :: (lgav to v)
          case (i, (k, None))    => lwher write field[id.T](i) :: field[key.T](k) :: HNil
        }
    }

  /**
    */
  final protected def deriveCsvEncoderKv[
      HV <: HList
  ](implicit
    lgav: LabelledGeneric.Aux[V, HV],
    llw: Lazy[LabelledWrite[HV]]): Pipe[F, Record, String] =
    writeLabelled(printer)
}

/** No part of this implementation tree depends on the `Spec`type.
  *
  * This is where we hide the fact that we're doing implementation inheritance.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed protected trait MemFile[F[_], V] {

  self: CsvStore[F, V] with Stores[V]#Store[F] =>

  implicit val F: Sync[F]
  implicit val X: ContextShift[F]

  /**
    */
  def path: Path

  final private lazy val appendHandles: Stream[F, FileHandle[F]] = {

    import OpenOption._

    val openOptions = Seq(
      CREATE,
      // READ,
      WRITE,
      APPEND,
      SYNC
      // DSYNC,
    )

    Stream resource (for {
      blocker <- Blocker[F]
      handle  <- FileHandle.fromPath(path, blocker, openOptions)
    } yield handle)
  }

  /**
    */
  final protected def readLines: Stream[F, String] =
    (Stream resource Blocker[F]) flatMap { blocker =>
      file.readAll[F](path, blocker, 1024 * 1024)
    } through
      text.utf8Decode through
      text.lines

  /**
    */
  final protected def appendingSink: Pipe[F, String, Unit] =
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
}

/**
  */
trait MemFileV[F[_], V] extends MemFile[F, V] {
  self: CsvValueStore[F, V] with ValueStores[V]#ValueStore[F] =>

  import VS.{ Id, Row }

  override protected def cacheLookup(id: Id): F[List[Row]] =
    ???

  override protected def cacheFill(id: Id, rows: List[Row]): F[Unit] =
    ???
}

/**
  */
trait MemFileKV[F[_], K, V] extends MemFile[F, V] {
  self: CsvKeyValueStore[F, K, V] with KeyValueStores[K, V]#KeyValueStore[F] =>

  import KVS.{ Id, Key, Row, Value }

  override protected def cacheLookup(key: Key): F[Option[(Id, List[Value])]] =
    ???

  override protected def cacheFill(id: Id, rows: List[Row]): F[Unit] =
    ???
}
