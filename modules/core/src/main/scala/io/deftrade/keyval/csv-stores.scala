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
protected trait UrTypes {

  /** */
  type ValueType

  /** */
  type ValueCompanionType[x] <: WithValue

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
protected[deftrade] object UrTypes {

  /** */
  abstract class Aux[F[_], W[_] <: WithValue, V](
      val V: W[V]
  )(
      implicit
      override val F: Sync[F],
      override val X: ContextShift[F]
  ) extends UrTypes {

    final override type ValueCompanionType[x] = W[x]
    final type Effect[x]                      = F[x]
    final type ValueType                      = V
  }
}

/** */
protected trait UrStore[F[_], W[_] <: WithValue, V] {

  self: UrTypes.Aux[F, W, V] =>

  import V._

  /** */
  protected def indexFrom(pr: IdRow): Index

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * TODO: This needs to evolve to use [[Result]]
    */
  def idRows: EffectStream[IdRow]

  /** May override, but must be implemented (or lifted) "as if by". */
  def rows: EffectStream[Row] = idRows map (_._2)

  /**  FIXME do we need this at all? */
  def filter(predicate: Row => Boolean): EffectStream[Row] = rows filter predicate

  /**
    * Returns  `Stream` of length zero or one:
    * - zero => not found
    * - one => result value
    */
  def get(id: Id): EffectStream[Row] =
    idRows filter (_._1 === id) map (_._2)

  /**
    * Like [[get]], but will return ''all'' `Row`s for the `Id` as a [[fs2.Stream]]
    */
  def getAll(x: Index): EffectStream[Row] =
    idRows
      .filter(pr => indexFrom(pr) == x)
      .map(_._2)

  /**
    * Like [[getAll]], but returns `Row`s as a [[scala.collection.immutable.List List]]
    */
  def getList(x: Index): EffectStream[List[Row]] =
    getAll(x)
      .fold(List.empty[Row])((vs, v) => v :: vs)
      .map(_.reverse)

  /**
    * May override for performance.
    *
    * FIXME: not thread safe, put a queue in front of single thread-contained appender?
    */
  final def append(row: Row): EffectStream[Id] =
    appendAll(row)

  /**
    * Note this returns a ''single'' `Id` for the whole sequence of `Row`s.
    */
  def appendAll(row: Row, rows: Row*): EffectStream[Id]

  /** FIXME obviously... this works, not obvously, that's the problem */
  protected var prev: Id =
    Refined unsafeApply [String, IsSha] "7hereWazAPharmrHadADogNBingoWuzHizN4m3oB1NGo"

  /** */
  protected def fresh: Fresh[Id, Row]
}

/** */
trait UrValueStore[F[_], V] extends UrStore[F, WithId, V] {
  self: UrTypes.Aux[F, WithId, V] =>
  // import V._
}

/**  */
trait UrKeyValueStore[F[_], K, V] extends UrStore[F, WithKey.Aux[K, *], V] {
  self: UrTypes.Aux[F, WithKey.Aux[K, *], V] =>

  import V._

  /** */
  final protected def indexFrom(pr: IdRow): Index =
    pr match {
      case (_, (key, _)) => key
    }

  /** */
  def select(key: Key): EffectStream[Value]

  /** */
  final def selectAll(key: Key): EffectStream[Row] =
    getAll(key)

  /** */
  final def selectList(key: Key): EffectStream[List[Row]] =
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
}

/** */
protected trait ModuleTypes extends UrTypes {

  /** */
  type HValue <: HList

  /** */
  implicit val lgv: LabelledGeneric.Aux[ValueType, HValue]
}

/** */
protected[deftrade] object ModuleTypes {

  /** */
  abstract class Aux[F[_], W[_] <: WithValue, V, HV <: HList](
      override val V: W[V]
  )(
      implicit
      final override val F: Sync[F],
      final override val X: ContextShift[F],
      val lgv: LabelledGeneric.Aux[V, HV]
  ) extends UrTypes.Aux[F, W, V](V) {

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
] extends UrStore[F, W, V] {
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
  final def idRows: EffectStream[IdRow] =
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
  protected def permRowToCSV: Pipe[Effect, IdRow, String]

  /** */
  protected def csvToPermRow: Pipe[Effect, String, Result[IdRow]]
}

/**
  * Ctor parameter `V` carries types specific to `type V`.
  * `type Index` mapped to [[WithValue.Id]].
  *
  * Cormorant CSV is integrated by implementing implicit methods
  * [[writePermRow]], [[readPermRow]], providing access to
  * [[io.chrisdavenport.cormorant.LabelledRead]] and
  * [[io.chrisdavenport.cormorant.LabelledWrite]]
  * instances for the [[WithValue.IdRow]] type for this store.
  */
trait CsvValueStore[
    F[_],
    V,
    HV <: HList
] extends CsvStore[F, WithId, V, HV] {
  self: ModuleTypes.Aux[F, WithId, V, HV] =>

  import V._

  /** */
  final type HRow = HValue

  /** */
  protected def indexFrom(pr: IdRow): Index =
    pr match {
      case (id, _) => id
    }

  /** */
  implicit final def writePermRow(
      implicit
      llw: Lazy[LabelledWrite[HValue]]
  ): LabelledWrite[IdRow] =
    new LabelledWrite[IdRow] {
      implicit val lwhpr       = LabelledWrite[HPermRow]
      def headers: CSV.Headers = lwhpr.headers
      override def write(pr: IdRow): CSV.Row = pr match {
        case (i, v) =>
          lwhpr write field[id.T](i) :: (lgv to v)
      }
    }

  /** */
  implicit final def readPermRow(
      implicit
      llr: Lazy[LabelledRead[HV]]
  ): LabelledRead[IdRow] =
    new LabelledRead[IdRow] {
      implicit val lrhpr = LabelledRead[HPermRow]
      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, IdRow] =
        lrhpr read (row, headers) map { hpr =>
          (hpr.head, lgv from hpr.tail)
        }
    }

  /** */
  final protected def deriveCsvToV(
      implicit
      llr: Lazy[LabelledRead[HV]]
  ): Pipe[Effect, String, Result[IdRow]] =
    readLabelledCompleteSafe[F, IdRow] andThen
      (_ map (_ leftMap errorToFail))

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveVToCsv(
      implicit
      llw: Lazy[LabelledWrite[HV]]
  ): Pipe[Effect, IdRow, String] = writeLabelled(printer)
}

/**  */
trait CsvKeyValueStore[
    F[_],
    K,
    V,
    HV <: HList
] extends UrKeyValueStore[F, K, V]
    with CsvStore[F, WithKey.Aux[K, *], V, HV] {
  self: ModuleTypes.Aux[F, WithKey.Aux[K, *], V, HV] =>

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
  ): LabelledWrite[IdRow] =
    new LabelledWrite[IdRow] {

      private val lwhpr = LabelledWrite[HPermRow]
      private val lwher = LabelledWrite[HEmptyRow]

      def headers: CSV.Headers = lwhpr.headers

      def write(pr: IdRow): CSV.Row = pr match {
        case (i, (k, Some(v))) => lwhpr write field[id.T](i) :: field[key.T](k) :: (lgv to v)
        case (i, (k, None))    => lwher write field[id.T](i) :: field[key.T](k) :: HNil
      }
    }

  /** */
  implicit final def readPermRow(
      implicit
      llr: LabelledRead[HV],
      getk: Get[Key]
  ): LabelledRead[IdRow] =
    new LabelledRead[IdRow] {

      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, IdRow] = {

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
  ): Pipe[Effect, String, Result[IdRow]] = {
    implicit def lrhv = llr.value
    implicit def getk = lgetk.value
    readLabelledCompleteSafe[F, IdRow] andThen
      (_ map (_ leftMap errorToFail))
  }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  final protected def deriveKvToCsv(
      implicit
      llw: Lazy[LabelledWrite[HV]],
      lputk: Lazy[Put[Key]]
  ): Pipe[Effect, IdRow, String] = {

    implicit def lwhv: LabelledWrite[HV] = llw.value
    implicit def putk: Put[Key]          = lputk.value
    writeLabelled(printer)
  }
}

/** */
protected trait MemFileImplV[F[_], W[_] <: WithValue, V, HV <: HList] extends CsvStore[F, W, V, HV] {
  self: ModuleTypes.Aux[F, W, V, HV] =>

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
) extends ModuleTypes.Aux(V)
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
) extends ModuleTypes.Aux(V)
    with CsvKeyValueStore[F, K, V, HV]
    with MemFileImplKV[F, K, V, HV]
