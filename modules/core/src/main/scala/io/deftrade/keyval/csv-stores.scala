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
import cats.{ Eq }
import cats.data.{ NonEmptyList }

import cats.effect.{ Blocker, ContextShift, Sync }

import shapeless.{ ::, HList, HNil, LabelledGeneric, Lazy }
import shapeless.labelled._

import fs2.{ text, Pipe, Stream }
import fs2.io.file.{ pulls, FileHandle /*, ReadCursor, WriteCursor */ }

// import scodec.bits.ByteVector

import io.chrisdavenport.cormorant
import cormorant.{ CSV, Error, Get, LabelledRead, LabelledWrite, Put }
import cormorant.generic.semiauto._
// import cormorant.refined._
import cormorant.fs2.{ readLabelledCompleteSafe, writeLabelled }

import java.nio.file.{ Path, StandardOpenOption => OpenOption }

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class CsvStore[F[_], W[_] <: WithValue, V, HV <: HList](
    override val V: W[V]
)(implicit
    final override val F: Sync[F],
    final override val X: ContextShift[F],
    final val lgv: LabelledGeneric.Aux[V, HV]
) extends StoreTypes.Aux[F, W, V](V)
    with Store[F, W, V]
    with CsvImplicits {

  final type HValue = HV // === lgv.Repr  test / validate / assume ?!?

  implicit final lazy val idPut: Put[V.Id] = Put[V.Id]
  implicit final lazy val idGet: Get[V.Id] = Get[V.Id]

  import V._

  /**
    */
  type HRow <: HList

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * Note: not distinguishing between `not found` and `IO error`
    * TODO: This needs to evolve.
    */
  final def records: StreamF[Record] =
    (readLines through csvToRecord).rethrow handleErrorWith (_ => Stream.empty)

  /** nop
    */
  protected def cache(id: Id, row: Row) = ()

  /** nop
    */
  protected def lookup(id: Id): List[Row] = List.empty

  /** csv
    */
  final protected def persist: Record PipeF Unit = recordToCSV andThen appendingSink

  /**
    */
  protected def readLines: StreamF[String]

  /**
    */
  protected def appendingSink: String PipeF Unit

  /**
    */
  protected def recordToCSV: Record PipeF String

  /**
    */
  protected def csvToRecord: String PipeF Result[Record]
}

/**
  * Value parameter `V` carries type members specific to `type V`.
  *
  * Cormorant CSV is integrated by implementing implicit methods
  * [[writeIdRow]], [[readIdRow]], providing access to
  * [[io.chrisdavenport.cormorant.LabelledRead]] and
  * [[io.chrisdavenport.cormorant.LabelledWrite]] instances for type `Record`.
  */
abstract class CsvValueStore[
    F[_]: Sync: ContextShift,
    V,
    HV <: HList
](v: WithId.Aux[V])(implicit lgv_ : LabelledGeneric.Aux[V, HV])
    extends CsvStore[F, WithId.Aux, V, HV](v)
    with ValueStore[F, V] {

  import V._

  /**
    */
  final type HRow = HV

  /**
    */
  implicit final def writeIdRow(implicit
      llw: Lazy[LabelledWrite[HV]]
  ): LabelledWrite[Record] =
    new LabelledWrite[Record] {
      implicit val lwhpr       = LabelledWrite[IdField :: HRow]
      def headers: CSV.Headers = lwhpr.headers
      override def write(pr: Record): CSV.Row =
        pr match {
          case (i, v) =>
            lwhpr write field[id.T](i) :: (lgv to v)
        }
    }

  /**
    */
  implicit final def readIdRow(implicit
      llr: Lazy[LabelledRead[HV]]
  ): LabelledRead[Record] =
    new LabelledRead[Record] {
      implicit val lrhpr = LabelledRead[IdField :: HRow]
      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Record] =
        lrhpr.read(row, headers) map { hpr =>
          (hpr.head, lgv from hpr.tail)
        }
    }

  /**
    */
  final protected def deriveCsvDecoderV(implicit
      llr: Lazy[LabelledRead[HV]]
  ): Pipe[F, String, Result[Record]] =
    readLabelledCompleteSafe[F, Record] andThen
      (_ map (_ leftMap errorToFail))

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveCsvEncoderV(implicit
      llw: Lazy[LabelledWrite[HV]]
  ): Pipe[F, Record, String] = writeLabelled(printer)
}

/**
  */
abstract class CsvKeyValueStore[
    F[_]: Sync: ContextShift,
    K,
    V,
    HV <: HList
](v: WithKey.Aux[K, V])(implicit lgv_ : LabelledGeneric.Aux[V, HV])
    extends CsvStore[F, WithKey.Aux[K, *], V, HV](v)
    with KeyValueStore[F, K, V] {

  import V._

  /**
    */
  implicit def kGet: Get[K]

  /**
    */
  implicit def kPut: Put[K]

  /**
    */
  final type HRow = KeyField :: HValue

  /**
    */
  final type HEmptyRow = IdField :: KeyField :: HNil

  // /** */
  // protected def valueFrom(row: Row): Value =
  //   row match {
  //     case (_, Some(value)) => value
  //   }

  /**
    */
  implicit final def writeIdRow(implicit
      llw: LabelledWrite[HV]
  ): LabelledWrite[Record] =
    // implicit final def writeIdRow: LabelledWrite[Record] =
    new LabelledWrite[Record] {

      private val lwhpr = LabelledWrite[IdField :: HRow]
      // private val lwhpr = LabelledWrite[IdField :: HRow]
      private val lwher = LabelledWrite[HEmptyRow]

      def headers: CSV.Headers = lwhpr.headers

      def write(pr: Record): CSV.Row =
        pr match {
          case (i, (k, Some(v))) => lwhpr write field[id.T](i) :: field[key.T](k) :: (lgv to v)
          case (i, (k, None))    => lwher write field[id.T](i) :: field[key.T](k) :: HNil
        }
    }

  /**
    */
  implicit final def readIdRow(implicit
      llr: LabelledRead[HV]
  ): LabelledRead[Record] =
    new LabelledRead[Record] {

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Record] = {

        val lrhpr = LabelledRead[IdField :: HRow]

        row match {

          case CSV.Row(
                NonEmptyList(CSV.Field(i), List(CSV.Field(k)))
              ) if i === id.toString && k === key.toString =>
            lrhpr.read(row, headers) map { hpr =>
              (hpr.head, (hpr.tail.head, none))
            }

          case _ =>
            lrhpr.read(row, headers) map { hpr =>
              (hpr.head, (hpr.tail.head, (lgv from hpr.tail.tail).some))
            }
        }
      }
    }

  /**
    */
  final protected def deriveCsvDecoderKv(implicit
      llr: Lazy[LabelledRead[HV]]
  ): Pipe[F, String, Result[Record]] = {

    implicit def lrhv = llr.value
    readLabelledCompleteSafe[F, Record] andThen
      (_ map (_ leftMap errorToFail))
  }

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveCsvEncoderKv(implicit
      llw: Lazy[LabelledWrite[HV]]
  ): Pipe[F, Record, String] = {
    implicit def lwhv = llw.value
    writeLabelled(printer)
  }
}

/**
  */
protected trait MemFileV[F[_], W[_] <: WithValue, V, HV <: HList] {

  self: CsvStore[F, W, V, HV] =>

  // cache stuff
  //  - this is where it matters whether we have single or multi value api
  //
  // final protected var table: Map[V.Index, V.Value]                  = Map.empty
  // final protected var tableNel: Map[V.Index, NonEmptyList[V.Value]] = Map.empty
  // final protected def tableNem[K2: Order, V2: Eq](
  //     implicit asK2V2: V.Value <~< (K2, V2)
  // ): Map[V.Index, NonEmptyMap[K2, V2]] = tableNel map {
  //   case (k, vs) => (k, (vs map (asK2V2 coerce _)).toNem)
  // }

  /**
    */
  def path: Path

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
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
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def appendingSink: Pipe[F, String, Unit] =
    for {
      handle <- appendHandles
      s      <- _
    } yield pulls
      .writeAllToFileHandle(
        Stream eval
          (F pure s) through
          text.utf8Encode,
        handle
      )
      .stream |> discardValue // nota bene this is intentional and necessary
  // WriteCursor(out, 0).writeAll(in).void

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def readLines: Stream[F, String] =
    (Stream resource Blocker[F]) flatMap { blocker =>
      fs2.io.file.readAll[F](path, blocker, 1024 * 1024)
    } through
      text.utf8Decode through
      text.lines
}

/**
  */
trait MemFileKV[F[_], K, V, HV <: HList]
    extends MemFileV[
      F,
      WithKey.Aux[K, *],
      V,
      HV
    ] {

  self: CsvStore[F, WithKey.Aux[K, *], V, HV] =>
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract case class MemFileValueStore[
    F[_]: Sync: ContextShift,
    V: Eq,
    HV <: HList
](
    v: WithId.Aux[V],
    final override val path: Path
)(implicit
    lgv_ : LabelledGeneric.Aux[V, HV],
    llr: Lazy[LabelledRead[HV]],
    llw: Lazy[LabelledWrite[HV]]
) extends CsvValueStore[F, V, HV](v)
    with MemFileV[F, WithId.Aux, V, HV] {

  final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderV
  final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderV
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract case class MemFileKeyValueStore[
    F[_]: Sync: ContextShift,
    K,
    V: Eq,
    HV <: HList
](
    v: WithKey.Aux[K, V],
    final override val path: Path
)(implicit
    lgv_ : LabelledGeneric.Aux[V, HV],
    final override val kGet: Get[K],
    final override val kPut: Put[K],
    llr: Lazy[LabelledRead[HV]],
    llw: Lazy[LabelledWrite[HV]]
) extends CsvKeyValueStore[F, K, V, HV](v)
    with MemFileKV[F, K, V, HV] {

  final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderKv
  final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderKv
}
