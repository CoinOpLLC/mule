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
import cats.Show
import cats.data.{ NonEmptyList, NonEmptyMap }

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

/**
  * Value parameter `V` carries type members specific to `type V`.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed trait CsvStore[F[_], W[_] <: WithValue, V] extends CsvImplicits {

  self: Store.Aux[F, W, V] =>

  import V._

  import cormorant.refined._
  import cormorant.implicits._

  protected implicit def idPut = Put[Id]
  protected implicit def idGet = Get[Id]

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * Note: not distinguishing between `not found` and `IO error`
    * TODO: This needs to evolve.
    */
  final def records: StreamF[Record] =
    (readLines through csvToRecord).rethrow handleErrorWith (_ => Stream.empty)

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
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed abstract class CsvValueStore[
    F[_]: Sync: ContextShift,
    V
](v: WithId.Aux[V])
    extends ValueStore.Aux[F, V](v)
    with CsvStore[F, WithId.Aux, V] {

  import V._

  /**
    */
  implicit final protected def readRecord[HV <: HList](implicit
      lgav: LabelledGeneric.Aux[V, HV],
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
  final protected def deriveCsvDecoderV[HV <: HList](implicit
      lgav: LabelledGeneric.Aux[V, HV],
      llhv: Lazy[LabelledRead[HV]]
  ): String PipeF Result[Record] =
    readLabelledCompleteSafe[F, Record] andThen
      (_ map (_ leftMap errorToFail))

  /**
    */
  implicit def writeRecord[
      HV <: HList
  ](implicit lgav: LabelledGeneric.Aux[V, HV], llhv: Lazy[LabelledWrite[HV]]): LabelledWrite[Record] =
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
  final protected def deriveCsvEncoderV[
      HV <: HList
  ](implicit lgav: LabelledGeneric.Aux[V, HV], llhv: Lazy[LabelledWrite[HV]]): Record PipeF String =
    writeLabelled(printer)
}

/**
  */
sealed abstract class CsvKeyValueStore[
    F[_]: Sync: ContextShift,
    K: Get: Put,
    V
](v: WithKey.Aux[K, V])
    extends KeyValueStore.Aux[F, K, V](v)
    with CsvStore[F, WithKey.Aux[K, *], V] {

  import V._

  /**
    */
  implicit def readRecord[
      HV <: HList
  ](implicit lgav: LabelledGeneric.Aux[V, HV], llhv: Lazy[LabelledRead[HV]]): LabelledRead[Record] =
    new LabelledRead[Record] {

      val lrhr = LabelledRead[IdField :: KeyField :: HV]

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Record] =
        row match {

          case CSV.Row(
                NonEmptyList(CSV.Field(i), List(CSV.Field(k)))
              ) if i === id.toString && k === key.toString =>
            lrhr.read(row, headers) map { hr =>
              (hr.head, (hr.tail.head, none))
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
  ](implicit lgav: LabelledGeneric.Aux[V, HV], llr: Lazy[LabelledRead[HV]]): Pipe[F, String, Result[Record]] =
    readLabelledCompleteSafe[F, Record] andThen
      (_ map (_ leftMap errorToFail))

  /**
    */
  implicit final def writeRecord[
      HV <: HList
  ](implicit lgav: LabelledGeneric.Aux[V, HV], llhv: Lazy[LabelledWrite[HV]]): LabelledWrite[Record] =
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
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveCsvEncoderKv[
      HV <: HList
  ](implicit lgav: LabelledGeneric.Aux[V, HV], llw: Lazy[LabelledWrite[HV]]): Pipe[F, Record, String] =
    writeLabelled(printer)
}

/** no part of this implementation tree depends on Spec
  */
sealed protected trait MemFile[F[_], W[_] <: WithValue, V] {

  self: CsvStore[F, W, V] =>

  implicit def F: Sync[F]
  implicit def X: ContextShift[F]

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
      file.readAll[F](path, blocker, 1024 * 1024)
    } through
      text.utf8Decode through
      text.lines
}

sealed trait MemFileV[F[_], V] extends MemFile[F, WithId.Aux, V] {
  self: CsvValueStore[F, V] =>
}

/**
  */
sealed trait MemFileKV[F[_], K, V] extends MemFile[F, WithKey.Aux[K, *], V] {
  self: CsvKeyValueStore[F, K, V] =>
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract case class CaMfValueStore[
    F[_]: Sync: ContextShift,
    V: Show,
    HV <: HList
](
    v: WithId.Aux[V],
    final override val path: Path
)(implicit
    lgv: LabelledGeneric.Aux[V, HV],
    llr: Lazy[LabelledRead[HV]],
    llw: Lazy[LabelledWrite[HV]]
) extends CsvValueStore(v)
    with MemFileV[F, V] {

  final override lazy val fresh = Fresh.shaContent[V.Row]

  final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderV
  final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderV
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract case class ChMfValueStore[
    F[_]: Sync: ContextShift,
    V: Show,
    HV <: HList
](
    v: WithId.Aux[V],
    final override val path: Path
)(implicit
    lgv: LabelledGeneric.Aux[V, HV],
    llr: Lazy[LabelledRead[HV]],
    llw: Lazy[LabelledWrite[HV]]
) extends CsvValueStore(v)
    with MemFileV[F, V] {

  final override lazy val fresh = Fresh.shaChain[V.Row]

  final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderV
  final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderV
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract case class MemFileKeyValueStore[
    F[_]: Sync: ContextShift,
    K: Get: Put,
    V: Show,
    HV <: HList
](
    v: WithKey.Aux[K, V],
    final override val path: Path
)(implicit lgv: LabelledGeneric.Aux[V, HV], llr: Lazy[LabelledRead[HV]], llw: Lazy[LabelledWrite[HV]])
    extends CsvKeyValueStore(v)
    with MemFileKV[F, K, V] {

  final lazy val recordToCSV: Record PipeF String         = deriveCsvEncoderKv
  final lazy val csvToRecord: String PipeF Result[Record] = deriveCsvDecoderKv
}
