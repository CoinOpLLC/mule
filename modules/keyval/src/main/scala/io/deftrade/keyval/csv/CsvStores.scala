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
package csv

// import syntax._

import cats.implicits._
import cats.data.{ NonEmptyList }

import cats.effect.{ Sync }

import shapeless.{ ::, HList, HNil, LabelledGeneric }
import shapeless.labelled.field

import fs2.{ Pipe, Stream }
import fs2.io.file
import file.{ FileHandle }

// import scodec.bits.ByteVector

import io.chrisdavenport.cormorant
import cormorant.{ CSV, Error, Get, LabelledRead, LabelledWrite, Printer, Put }
import cormorant.fs2.{ readLabelledCompleteSafe, writeLabelled }

import java.nio.file.{ Path, StandardOpenOption => OpenOption }

/** Value parameter `V` carries type members specific to `type V`.
  */
sealed abstract class CsvStore[F[_]: Sync, V](
    final val V: Stores[V]
) {

  self: Stores[V]#Store[F] =>

  /** Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * Note: not distinguishing between `not found` and `IO error`
    * TODO: This needs to evolve.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final def records: Stream[F, Record] =
    readLines
      .through[F, Result[Record]](csvToRecord)
      .rethrow[F, Record]
      .handleErrorWith[F, Record](_ => Stream.empty.covaryAll[F, Record])

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

  protected lazy val errorToFail: Error => Fail = Fail fromThrowable "csv failure"
  protected def printer: Printer                = Printer.default
}

/**
  */
abstract class CsvValueStore[F[_]: Sync, V](
    final val VS: ValueStores[V]
) extends CsvStore[F, V](VS) {

  self: ValueStores[V]#ValueStore[F] =>

  import VS.{ id, IdField }

  /**
    */
  implicit final protected def recordLabelledRead[HV <: HList](
      implicit
      lgav: LabelledGeneric.Aux[V, HV],
      lrhr: LabelledRead[IdField :: HV]
  ): LabelledRead[Record] =
    new LabelledRead[Record] {

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Record] =
        lrhr.read(row, headers) map { hr =>
          (hr.head, lgav from hr.tail)
        }
    }

  /**
    */
  final protected def deriveCsvDecoderV(
      implicit lrr: LabelledRead[Record]
  ): Pipe[F, String, Result[Record]] =
    readLabelledCompleteSafe[F, Record] andThen
      (_ map (_ leftMap errorToFail))

  /**
    */
  implicit def recordLabelledWrite[HV <: HList](
      implicit
      lgav: LabelledGeneric.Aux[V, HV],
      lwhr: LabelledWrite[IdField :: HV]
  ): LabelledWrite[Record] =
    new LabelledWrite[Record] {

      def headers: CSV.Headers = lwhr.headers

      def write(pr: Record): CSV.Row =
        pr match {
          case (i, v) => lwhr write field[id.T](i) :: (lgav to v)
        }
    }

  /**
    */
  final protected def deriveCsvEncoderV(
      implicit lwr: LabelledWrite[Record]
  ): Pipe[F, Record, String] =
    writeLabelled[F, Record](printer)
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class CsvKeyValueStore[F[_]: Sync, K: Get: Put, V](
    final val KVS: KeyValueStores[K, V]
) extends CsvStore[F, V](KVS) {

  self: KeyValueStores[K, V]#KeyValueStore[F] =>

  import KVS.{ id, key, IdField, KeyField }

  /**
    */
  implicit def labelledReadRecord[HV <: HList](
      implicit
      lgav: LabelledGeneric.Aux[V, HV],
      lrhr: LabelledRead[IdField :: KeyField :: HV]
  ): LabelledRead[Record] =
    new LabelledRead[Record] {

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Record] =
        row match {

          case CSV.Row( // empty row (sic)
              NonEmptyList(CSV.Field(i), List(CSV.Field(k)))
              ) if i === id.toString && k === key.toString =>
            lrhr.read(row, headers) map { hr =>
              (hr.head, (hr.tail.head: K, none[V]))
            }

          case _ => // non-empty row (sic)
            lrhr.read(row, headers) map { hr =>
              (hr.head, (hr.tail.head, (lgav from hr.tail.tail).some))
            }
        }
    }

  /**
    */
  final protected def deriveCsvDecoderKv(
      implicit
      lrr: LabelledRead[Record]
  ): Pipe[F, String, Result[Record]] =
    readLabelledCompleteSafe[F, Record] andThen
      (_ map (_ leftMap errorToFail))

  /**
    */
  implicit def labelledWriteRecord[HV <: HList](
      implicit
      lgav: LabelledGeneric.Aux[V, HV],
      lwhr: LabelledWrite[IdField :: KeyField :: HV],
      lwhe: LabelledWrite[IdField :: KeyField :: HNil]
  ): LabelledWrite[Record] =
    new LabelledWrite[Record] {

      def headers: CSV.Headers = lwhr.headers

      def write(pr: Record): CSV.Row =
        pr match {
          case (i, (k, Some(v))) => lwhr write field[id.T](i) :: field[key.T](k) :: (lgav to v)
          case (i, (k, None))    => lwhe write field[id.T](i) :: field[key.T](k) :: HNil
        }
    }

  /**
    */
  final protected def deriveCsvEncoderKv(
      implicit lwr: LabelledWrite[Record]
  ): Pipe[F, Record, String] =
    writeLabelled(printer)
}

/** No part of this implementation tree depends on the `Spec`type.
  *
  * This is where we hide the fact that we're doing implementation inheritance.
  *
  * (Our API doesn't lock us into this, so we can change that implementation detail.)
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed protected trait MemFile[F[_], V] {

  self: CsvStore[F, V] with Stores[V]#Store[F] =>

  /**
    */
  def path: Path

  /**
    */
  final protected def readLines: Stream[F, String] = ???
  // Stream resource Blocker[F] flatMap { blocker =>
  //   file.readAll[F](path, blocker, 1024 * 1024)
  // } through
  //   text.utf8Decode through
  //   text.lines

  val discardValue: Any => Unit = (_: Any) => ()
  implicit final class PipeToFunction1[A](val a: A) {
    def |>[B](f: A => B): B = f(a)
  }

  /**
    */
  final protected def appendingSink: Pipe[F, String, Unit] = {

    import OpenOption._
    def openOptions =
      Seq(
        CREATE,
        // READ,
        WRITE,
        APPEND,
        SYNC
        // DSYNC,
      )

    def appendHandles: Stream[F, FileHandle[F]] = ???
    // Stream resource (for {
    //   blocker <- Blocker[F]
    //   handle  <- FileHandle.fromPath(path, blocker, openOptions)
    // } yield handle)

    for {
      handle <- appendHandles
      s      <- _
    } yield ???
    // pulls
    //   .writeAllToFileHandle(Stream eval s.pure[F] through text.utf8Encode, handle)
    //   .stream |> discardValue // nota bene this is intentional and necessary
  }
}

/**
  */
trait MemFileV[F[_], V] extends MemFile[F, V] {
  self: CsvValueStore[F, V] with ValueStores[V]#ValueStore[F] =>

  import VS.{ Id }

  override protected def cacheLookup(id: Id)(implicit F: Sync[F]): F[List[Row]] =
    ???

  override protected def cacheFill(id: Id, rows: List[Row])(implicit F: Sync[F]): F[Unit] =
    ???
}

/**
  */
trait MemFileKV[F[_], K, V] extends MemFile[F, V] {
  self: CsvKeyValueStore[F, K, V] with KeyValueStores[K, V]#KeyValueStore[F] =>

  import KVS.{ Id }

  override protected def cacheLookup(key: K)(implicit F: Sync[F]): F[Option[(Id, List[V])]] =
    ???

  override protected def cacheFill(id: Id, rows: List[Row])(implicit F: Sync[F]): F[Unit] =
    ???
}
