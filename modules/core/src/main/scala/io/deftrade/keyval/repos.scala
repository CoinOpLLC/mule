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

import spire.math.Integral
import spire.syntax.field._

import shapeless.{ ::, HList, LabelledGeneric, Lazy }
import shapeless.labelled._

import fs2.{ io, text, Pipe, Stream }

import _root_.io.chrisdavenport.cormorant
import cormorant._
// import cormorant.generic.semiauto._

import scala.language.higherKinds

import java.nio.file.{ Path, Paths }

final case class Fresh[K](init: K, next: K => K)

/**
  * Defines how to create a fresh '''globally unique''' key which
  * is suitable to be persisted.
  */
/** */
object Fresh {

  def apply[K: Fresh] = implicitly[Fresh[K]]

  /**
    * Equivalent to `autoincrement` or `serial` from SQL.
    *
    * TODO: PRNG version.
    */
  def zeroBasedIncr[K: Integral, P]: Fresh[OpaqueKey[K, P]] = {

    val K = Integral[K]; import K._

    Fresh(
      OpaqueKey(zero),
      key => OpaqueKey(key.value + one)
    )
  }
}

/**
  *
  */
trait stores {

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
    implicit val llr: Lazy[LabelledRead[HValue]]

    /** */
    implicit val llw: Lazy[LabelledWrite[HValue]]

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
        override val lgv: LabelledGeneric.Aux[V, HV],
        override val llr: Lazy[LabelledRead[HV]],
        override val llw: Lazy[LabelledWrite[HV]]
    ) extends ModuleTypes {

      final type ValueCompanionType[x] = W[x]

      final type ValueType = V.Value
      final type HValue    = HV // = lgv.Repr ?!?

      final type EffectType[x] = F[x]

      /** Basic in-memory table structure */
      final type Table = Map[V.Index, V.Value]
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
    final def filter(predicate: Row => Boolean): EffectStream[Row] = rows filter predicate

    /** */
    final def rows: EffectStream[Row] = permRows map (_._2)

    /**  @return a `Stream` of length zero or one. */
    def get(id: Id): EffectStream[Row] = permRows filter (_._1 === id) map (_._2)

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def permRows: EffectStream[PermRow] = readLines through csvToPermRow

    /** FIXME: not thread safe, put a queue in front of single thread-contained appender */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def append(row: Row): EffectStream[Id] =
      for {
        id <- Stream eval { F delay { id = fresh next id; id } }
        _  <- Stream emit id -> row through permRowToCSV through appendingSink
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
    protected def csvToPermRow: Pipe[EffectType, String, PermRow]

    @SuppressWarnings(Array("org.wartremover.warts.Var"))
    @volatile private var id: Id      = fresh.init
    private lazy val fresh: Fresh[Id] = Fresh.zeroBasedIncr
  }

  /**
    * Ctor parameter `V` carries types specific to `type V`.
    * `type Index` mapped to [[WithValue.Id]].
    */
  trait ValueStore[F[_], V, HV <: HList] extends Store[F, WithId.Aux, V, HV] {
    self: ModuleTypes.Aux[F, WithId.Aux, V, HV] =>

    import V._

    /** */
    final protected def csvToPermRow: Pipe[EffectType, String, PermRow] = { ess =>
      ???
    }

    /** */
    final protected def permRowToCSV: Pipe[EffectType, PermRow, String] = ???
  }

  /**  */
  trait KeyValueStore[F[_], V, HV <: HList] extends Store[F, WithKey.AuxK, V, HV] {
    self: ModuleTypes.Aux[F, WithKey.AuxK, V, HV] =>

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

    implicit final def writePermRow(
        implicit lwhpr: LabelledWrite[IdField :: KeyField :: HV]
    ): LabelledWrite[PermRow] =
      new LabelledWrite[PermRow] {
        def headers: CSV.Headers = lwhpr.headers
        def write(pr: PermRow): CSV.Row = pr match {
          case (i, (k, v)) =>
            lwhpr write field[id.T](i) :: field[key.T](k) :: (lgv to v)
        }
      }

    /** */
    implicit final def readPermRow(
        implicit lrikv: LabelledRead[IdField :: KeyField :: HV]
    ): LabelledRead[PermRow] =
      new LabelledRead[PermRow] {
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, PermRow] =
          lrikv.read(row, headers) map { h =>
            (h.head, (h.tail.head, lgv from h.tail.tail))
          }
      }

    /** */
    final protected def permRowToCSV: Pipe[EffectType, PermRow, String] = { prs =>
      ???
    }

    /**  */
    final protected def csvToPermRow: Pipe[EffectType, String, PermRow] = { ess =>
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
  private trait MemFileImplKV[F[_], V, HV <: HList] extends MemFileImplV[F, WithKey.AuxK, V, HV] {
    self: ModuleTypes.Aux[F, WithKey.AuxK, V, HV] =>

    import V._

    /** */
    final def get(k: Key): EffectStream[Value] = Stream evals (F pure (table get k))

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
      final override val llr: Lazy[LabelledRead[HV]],
      final override val llw: Lazy[LabelledWrite[HV]]
  ) extends ModuleTypes.Aux[F, WithId.Aux, V, HV](V)
      with ValueStore[F, V, HV]
      with MemFileImplV[F, WithId.Aux, V, HV]

  /** */
  private sealed abstract case class MemFileKeyValueStore[
      F[_]: Sync: ContextShift,
      V: Eq,
      HV <: HList
  ](
      final override val V: WithKey.AuxK[V]
  )(
      implicit
      final override val lgv: LabelledGeneric.Aux[V, HV],
      final override val llr: Lazy[LabelledRead[HV]],
      final override val llw: Lazy[LabelledWrite[HV]]
  ) extends ModuleTypes.Aux[F, WithKey.AuxK, V, HV](V)
      with KeyValueStore[F, V, HV]
      with MemFileImplKV[F, V, HV]

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
    new MemFileValueStore(V) { override def path = Paths get p }
  }

  /** */
  def keyValueStore[F[_]: Sync: ContextShift, V: Eq, HV <: HList](
      V: WithKey.AuxK[V],
      p: String
  )(
      implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]
  ): Result[KeyValueStore[F, V, HV]] = Result safe {
    new MemFileKeyValueStore(V) { override def path = Paths get p }
  }
}

object stores extends stores
