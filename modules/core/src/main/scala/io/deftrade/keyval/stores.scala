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

/**
  */
trait StoreTypes {

  /**
    */
  type ValueType

  /**
    */
  type ValueCompanionType[x] <: WithValue

  /**
    */
  type EffectType[_]

  /**
    */
  final type StreamF[x] = Stream[EffectType, x]

  /**
    */
  implicit def F: Sync[EffectType]

  /**
    */
  implicit def X: ContextShift[EffectType]

}

/**
  */
object StoreTypes {

  /**
    */
  abstract class Aux[F[_], W[_] <: WithValue, V](
      val V: W[V]
  )(implicit
    override val F: Sync[F],
    override val X: ContextShift[F])
      extends StoreTypes {

    final override type ValueCompanionType[x] = W[x]
    final type EffectType[x]                  = F[x]
    final type ValueType                      = V
  }
}

/**
  * Note the only way to get an Id is as the result of an effectful mutation or probe.
  */
trait Store[F[_], W[_] <: WithValue, V] {

  self: StoreTypes.Aux[F, W, V] =>

  import V._

  /**
    */
  final def rows: Stream[F, Row] = idRows map (_._2)

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * TODO: Does this need to evolve to use [[Result]]? Think through exception handling.
    */
  protected def idRows: Stream[F, (Id, Row)]

  /**
    * Note this returns a ''single'' `Id` for the whole sequence of `Row`s.
    *
    * This feature - the ability to assign multiple rows a single `Id` computed over those all
    * of those rows - is why ''this'' method is the abstract primitive (and not [[append]]).
    *
    * FIXME: not thread safe, put a queue in front of single thread-contained appender?
    */
  protected def append(row: Row, rows: Row*): Stream[F, Id]

  // /** May override, but must be implemented (or lifted) "as if by". */
  // protected def append(row: Row): Stream[F, Id] =
  //   append(row)

  /** FIXME obviously... this works, not obvously, that's the problem */
  protected var prev: Id =
    Refined unsafeApply [String, IsSha] "7hereWazAPharmrHadADogNBingoWuzHizN4m3oB1NGo"

  /**
    */
  protected def fresh: Fresh[Id, Row]
}

/**
  */
object Store {

  /**
    */
  def listPipe[F[_], A]: Stream[F, A] => Stream[F, List[A]] =
    _.fold(List.empty[A])((vs, v) => v :: vs) map (_.reverse)
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStore[F[_], V] extends Store[F, WithId.Aux, V] {

  self: StoreTypes.Aux[F, WithId.Aux, V] =>

  import V._

  /** FIXME: implement */
  def has(id: Id): Stream[F, Boolean] =
    idRows exists (_._1 === id)

  /**
    * Returns ''all'' `Row`s with the given `Id` (none, if not found) as an [[fs2.Stream]].
    */
  def get(id: Id): Stream[F, Row] =
    idRows filter (_._1 === id) map (_._2)

  /**
    * Like [[get]], but returns `Row`s as a [[scala.collection.immutable.List List]]
    * FIXME move to `Nel`
    */
  def getList(id: Id): Stream[F, List[Row]] =
    get(id) through Store.listPipe

  /**
    * FIXME move to `Nem`
    */
  final def getMap[K2: Order, V2](id: Id)(implicit
                                          asK2V2: V <~< (K2, V2)): Stream[F, Map[K2, V2]] =
    for (values <- getList(id))
      yield SortedMap(values map (asK2V2 coerce _): _*)

  /**
    */
  final def put(value: Value): Stream[F, Id] =
    append(value)

  /**
    */
  final def putList(values: List[Value]): Stream[F, Id] =
    values.headOption.fold(Stream.empty[F]: Stream[F, Id])(value => append(value, (values drop 1): _*))

  /**
    */
  final def putMap[K2: Order, V2](k2v2s: Map[K2, V2])(implicit
                                                      asValue: (K2, V2) <~< V): Stream[F, Id] =
    putList(k2v2s.toList map (asValue coerce _))

  /**
    */
  final def putNel(values: NonEmptyList[Value]): Stream[F, Id] =
    append(values.head, values.tail: _*)

  /**
    */
  final def putNem[K2: Order, V2](k2v2s: NonEmptyMap[K2, V2])(implicit
                                                              asValue: (K2, V2) <~< Value): Stream[F, Id] =
    putNel(k2v2s.toNel map (asValue coerce _))
}

/** placeholder */
object ValueStore

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait KeyValueStore[F[_], K, V] extends Store[F, WithKey.Aux[K, *], V] {

  self: StoreTypes.Aux[F, WithKey.Aux[K, *], V] =>

  import V._, Key._

  protected def idRowsWith(key: Key): Stream[F, (Id, Row)] =
    idRows filter (_._2._1 === key)

  /**
    */
  protected def updateExistingKey(key: Key, maybeValue: Option[Value]): Stream[F, Id] =
    for {
      _  <- exists(key)
      id <- append(key -> maybeValue)
    } yield id

  /**
    */
  def exists(key: Key): Stream[F, Id] =
    idRowsWith(key) map (_._1)

  /**
    */
  def select(key: Key): Stream[F, Value] =
    selectAll(key) take 1

  /** Returns '''all''' un-[[delete]]d `Value`s for the given `Key`. */
  def selectAll(key: Key): Stream[F, Value] =
    for {
      value <- (idRowsWith(key) map (_._2._2)).unNoneTerminate
    } yield value

  /**
    */
  def selectList(key: Key): Stream[F, List[Value]] =
    selectAll(key) through Store.listPipe

  /**
    */
  def selectMap[K2: Order, V2](key: Key)(implicit
                                         asK2V2: Value <~< (K2, V2)): Stream[F, Map[K2, V2]] =
    for (values <- selectList(key))
      yield SortedMap(values map (asK2V2 coerce _): _*)

  /**
    */
  def insert(key: Key, value: Value): Stream[F, Id] =
    exists(key).last flatMap (_.fold(append(key -> value.some))(_ => Stream.empty))

  /**
    */
  def update(key: Key, value: Value): Stream[F, Id] =
    updateExistingKey(key, value.some)

  /** Empty `Value` memorializes (persists) `delete` for a given `key` - this row gets an `Id`! */
  def delete(key: Key): Stream[F, Id] =
    updateExistingKey(key, none)

  /**
    */
  def upsert(key: Key, value: Value): Stream[F, Id] =
    append(key -> value.some)

  /**
    */
  def upsertNel(key: Key, values: NonEmptyList[Value]): Stream[F, NonEmptyList[Id]] =
    (values map (v => append(key -> v.some))).sequence

  /**
    */
  def upsertNem[K2: Order, V2](key: Key, k2v2s: NonEmptyMap[K2, V2])(implicit
                                                                     asValue: (K2, V2) <~< Value): Stream[F, NonEmptyList[Id]] =
    upsertNel(key, k2v2s.toNel map (asValue coerce _))
}

/** placeholder */
object KeyValueStore
