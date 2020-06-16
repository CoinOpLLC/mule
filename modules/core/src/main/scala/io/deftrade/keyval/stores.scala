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

import refinements.IsSha

import cats.implicits._
import cats.{ Order }
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.evidence._
import cats.effect.{ ContextShift, Sync }

import eu.timepit.refined
import refined.api.Refined
import refined.cats.refTypeOrder

import fs2.{ Stream }

import scala.collection.immutable.SortedMap

/** */
trait StoreTypes {

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
object StoreTypes {

  /** */
  abstract class Aux[F[_], W[_] <: WithValue, V](
      val V: W[V]
  )(
      implicit
      override val F: Sync[F],
      override val X: ContextShift[F]
  ) extends StoreTypes {

    final override type ValueCompanionType[x] = W[x]
    final type Effect[x]                      = F[x]
    final type ValueType                      = V
  }
}

/** */
protected trait Store[F[_], W[_] <: WithValue, V] {

  self: StoreTypes.Aux[F, W, V] =>

  import V._

  /** */
  protected def indexFrom(ir: (Id, Row)): Index

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * TODO: This needs to evolve to use [[Result]]
    */
  def idRows: EffectStream[(Id, Row)]

  /** May override, but must be implemented (or lifted) "as if by". */
  def rows: EffectStream[Row] =
    idRows map (_._2)

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
    idRows filter (ir => indexFrom(ir) == x) map (_._2)

  /** */
  protected def listPipe[A]: EffectStream[A] => EffectStream[List[A]] =
    _.fold(List.empty[A])((vs, v) => v :: vs) map (_.reverse)

  /**
    * May override for performance.
    *
    * FIXME: not thread safe, put a queue in front of single thread-contained appender?
    */
  final def append(row: Row): EffectStream[Id] =
    appendAll(row)

  /**
    * Note this returns a ''single'' `Id` for the whole sequence of `Row`s.
    *
    * This feature - the ability to assign multiple rows a single `Id` computed over those all
    * of those rows - is why ''this'' method is the abstract primitive (and not [[append]]).
    *
    */
  def appendAll(row: Row, rows: Row*): EffectStream[Id]

  /** FIXME obviously... this works, not obvously, that's the problem */
  protected var prev: Id =
    Refined unsafeApply [String, IsSha] "7hereWazAPharmrHadADogNBingoWuzHizN4m3oB1NGo"

  /** */
  protected def fresh: Fresh[Id, Row]
}

/** */
trait ValueStore[F[_], V] extends Store[F, WithId, V] {

  self: StoreTypes.Aux[F, WithId, V] =>

  import V._

  /**
    * Like [[getAll]], but returns `Row`s as a [[scala.collection.immutable.List List]]
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getList(x: Index): EffectStream[List[Row]] =
    getAll(x) through listPipe

  /** */
  final def getMap[K2: Order, V2](id: Id)(
      implicit asK2V2: Value <~< (K2, V2)
  ): EffectStream[Map[K2, V2]] =
    for (values <- getList(id))
      yield SortedMap(values map (asK2V2 coerce _): _*)

  /** */
  final def appendNel(values: NonEmptyList[Value]): EffectStream[Id] =
    appendAll(values.head, values.tail: _*)

  /** */
  final def appendNem[K2: Order, V2](k2v2s: NonEmptyMap[K2, V2])(
      implicit asValue: (K2, V2) <~< Value
  ): EffectStream[Id] =
    appendNel(k2v2s.toNel map (asValue coerce _))
}

/** dsl enhancements - ''dry'' type definitions for `Store`s */
object ValueStore {

  /** */
  def of(V: WithValue) = TypeOps(V)

  /** */
  sealed case class TypeOps(final val V: WithValue) {
    final type Store[F[_]] = ValueStore[F, V.Value]
  }
}

/**  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait KeyValueStore[F[_], K, V] extends Store[F, WithKey.Aux[K, *], V] {
  self: StoreTypes.Aux[F, WithKey.Aux[K, *], V] =>

  import V._

  /** */
  def exists(key: Key): EffectStream[Id] =
    idRows filter (ir => indexFrom(ir) == key) map (_._1)

  /** */
  def select(key: Key): EffectStream[Value] =
    getAll(key) flatMap (r => Stream evals F.delay { r._2 }) take 1

  /** Returns all un-[[delete]]d `Value`s for the given `Key`. */
  def selectAll(key: Key): EffectStream[Value] =
    for {
      value <- (getAll(key) map (_._2)).unNoneTerminate
    } yield value

  /**  */
  def selectList(key: Key): EffectStream[List[Value]] =
    selectAll(key) through listPipe

  /** */
  def selectMap[K2: Order, V2](key: Key)(
      implicit asK2V2: Value <~< (K2, V2)
  ): EffectStream[Map[K2, V2]] =
    for (values <- selectList(key))
      yield SortedMap(values map (asK2V2 coerce _): _*)

  /** */
  def insert(key: Key, value: Value): EffectStream[Id] =
    exists(key).last flatMap (_.fold(append(key -> value.some))(_ => Stream.empty))

  /** */
  def update(key: Key, value: Value): EffectStream[Id] =
    updateExistingKey(key, value.some)

  /** Empty `Value` memorializes (persists) `delete` for a given `key` - this row gets an `Id`! */
  def delete(key: Key): EffectStream[Id] =
    updateExistingKey(key, none)

  /** */
  def upsert(key: Key, value: Value): EffectStream[Id] =
    append(key -> value.some)

  /** */
  def upsertNel(key: Key, values: NonEmptyList[Value]): EffectStream[NonEmptyList[Id]] =
    (values map (v => append(key -> v.some))).sequence

  /** */
  def upsertNem[K2: Order, V2](key: Key, k2v2s: NonEmptyMap[K2, V2])(
      implicit asValue: (K2, V2) <~< Value
  ): EffectStream[NonEmptyList[Id]] =
    upsertNel(key, k2v2s.toNel map (asValue coerce _))

  /** */
  final protected def indexFrom(ir: (Id, Row)): Index =
    ir match {
      case (_, (key, _)) => key
    }

  /** */
  protected def updateExistingKey(key: Key, maybeValue: Option[Value]): EffectStream[Id] =
    for {
      _  <- exists(key)
      id <- append(key -> maybeValue)
    } yield id
}

/** dsl enhancements - ''dry'' type definitions for `Store`s */
object KeyValueStore {

  /** */
  def of(V: WithKey) = TypeOps(V)

  /** */
  sealed case class TypeOps(final val V: WithKey) {

    /** */
    final type Store[F[_]] = KeyValueStore[F, V.Key, V.Value]
  }

}
