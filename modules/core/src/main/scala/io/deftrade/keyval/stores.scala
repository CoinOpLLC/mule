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

import fs2.{ Pipe, Stream }

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
  )(implicit override val F: Sync[F], override val X: ContextShift[F])
      extends StoreTypes {

    final override type ValueCompanionType[x] = W[x]
    final type EffectType[x]                  = F[x]
    final type ValueType                      = V
  }
}

/**
  * Note the only way to get an Id is as the result of an effectful mutation or probe.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait Store[F[_], W[_] <: WithValue, V] {

  self: StoreTypes.Aux[F, W, V] =>

  import V._

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    */
  protected def idZippedRows: Stream[F, (Id, Row)]

  /**
    * Note this returns a ''single'' `Id` for the whole sequence of `Row`s.
    *
    * This feature - the ability to assign multiple rows a single `Id` computed over those all
    * of those rows - is why ''this'' method is the abstract primitive (and not [[append]]).
    *
    * Appends to the backing store whether or not there is a duplicate (no checking).
    *
    * FIXME: not thread safe, put a queue in front of single thread-contained appender?
    */
  final protected def append(row: Row, rows: Row*): F[Id] =
    for {
      id <- F delay nextId(row, rows: _*)
      rs <- F delay (row +: rows).toList
      _ <- (Stream evals (F delay (rs map { r =>
            cache(id, r)
            (id, r)
          })) through persist).compile.drain
    } yield id

  /**
    */
  protected def cache(id: Id, row: Row): Unit

  /**
    */
  protected def persist: Pipe[F, (Id, Row), Unit]

  /** FIXME obviously... this works, not obvously, that's the problem */
  protected var prev: Id =
    Refined unsafeApply [String, IsSha] "7hereWazAPharmrHadADogNBingoWuzHizN4m3oB1NGo"

  /**
    */
  protected def fresh: Fresh[Id, Row]

  /** note this is pure and not effectful! */
  protected def nextId(row: Row, rows: Row*): Id =
    fresh.nextAll(prev, row, rows: _*)

  /**
    */
  def rows: Stream[F, Row] = idZippedRows map (_._2)

  /**
    */
  def has(id: Id): F[Boolean] =
    (idZippedRows exists (_._1 === id)).compile.lastOrError

  /**
    */
  def get(id: Id): F[Option[Row]] =
    (idZippedRows filter (_._1 === id) map (_._2)).compile.last

  /**
    */
  def put(row: Row): F[(Id, Boolean)] =
    puts(row)

  /**
    * Get `Stream`.
    *
    * Returns ''all'' `Row`s with the given `Id` (none, if not found) as an [[fs2.Stream]].
    */
  def gets(id: Id): Stream[F, Row] =
    idZippedRows filter (_._1 === id) map (_._2)

  /**
    * Put `Seq`.
    *
    * TODO: Ugly double hash calc can be optimized later.
    * Also, is ''never'' necessary for chained hash.
    */
  def puts(row: Row, rows: Row*): F[(Id, Boolean)] = {
    val id = nextId(row, rows: _*)
    for {
      x   <- has(id)
      ret <- if (x) (id, false).pure[F] else append(row, rows: _*) map ((_, true))
    } yield ret
  }
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

  /**
    * Like [[get]], but returns `Row`s as a [[scala.collection.immutable.List List]]
    */
  def getList(id: Id): Stream[F, List[Row]] =
    gets(id) through Store.listPipe

  /**
    * Like [[get]], but returns `Row`s as a [[scala.collection.immutable.Map Map[K2, V2]]]
    */
  def getMap[K2: Order, V2](id: Id)(implicit asK2V2: V <~< (K2, V2)): Stream[F, Map[K2, V2]] =
    for (values <- getList(id))
      yield SortedMap(values map (asK2V2 coerce _): _*)

  /**
    * @param values possibly empty
    * @return `None` if values was empty; Id otherwise
    */
  def putList(values: List[Value]): F[Option[(Id, Boolean)]] =
    values.headOption.fold(
      none.pure[F]: F[Option[(Id, Boolean)]]
    ) { v =>
      puts(v, (values drop 1): _*) map (_.some)
    }

  /**
    */
  def putMap[K2: Order, V2](
      k2v2s: Map[K2, V2]
  )(implicit asValue: (K2, V2) <~< V): F[Option[(Id, Boolean)]] =
    putList(k2v2s.toList map (asValue coerce _))

  /**
    */
  def putNel(values: NonEmptyList[Value]): F[(Id, Boolean)] =
    puts(values.head, values.tail: _*)

  /**
    */
  def putNem[K2: Order, V2](
      k2v2s: NonEmptyMap[K2, V2]
  )(implicit asValue: (K2, V2) <~< Value): F[(Id, Boolean)] =
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

  @inline private def idiomize: ((Id, Boolean)) => Option[Id] = {
    case (id, true) => id.some
    case _          => none
  }

  protected def idZippedRowsWith(key: Key): Stream[F, (Id, Row)] =
    idZippedRows filter (_._2._1 === key)

  /** TODO: revisit the implementation, which is tricky
    */
  protected def updateExistingKey(key: Key, maybeValue: Option[Value]): F[Option[Id]] =
    exists(key) flatMap (_.fold(none[Id].pure[F])(_ => append(key -> maybeValue) map (_.some)))

  /**
    */
  def exists(key: Key): F[Option[Id]] =
    existsAll(key).compile.last

  /**
    */
  def select(key: Key): F[Option[Value]] =
    selectAll(key).compile.last

  /**
    */
  def insert(key: Key, value: Value): F[Option[Id]] =
    exists(key) flatMap (_.fold(put(key -> value.some) map idiomize)(_ => none.pure[F]))

  /**
    */
  def update(key: Key, value: Value): F[Option[Id]] =
    updateExistingKey(key, value.some)

  /** Empty `Value` memorializes (persists) `delete` for a given `key` - this row gets an `Id`!
    */
  def delete(key: Key): F[Option[Id]] =
    updateExistingKey(key, none)

  /**
    */
  def upsert(key: Key, value: Value): F[Id] =
    append(key -> value.some)

  /**
    */
  def existsAll(key: Key): Stream[F, Id] =
    idZippedRowsWith(key) map (_._1)

  /** Returns '''all''' un-[[delete]]d `Value`s for the given `Key`. */
  def selectAll(key: Key): Stream[F, Value] =
    for {
      value <- (idZippedRowsWith(key) map (_._2._2)).unNoneTerminate
    } yield value

  /**
    */
  def selectList(key: Key): F[List[Value]] =
    selectAll(key).compile.toList

  /**
    */
  def selectMap[K2: Order, V2](
      key: Key
  )(implicit asK2V2: Value <~< (K2, V2)): F[Map[K2, V2]] =
    for (values <- selectList(key))
      yield SortedMap(values map (asK2V2 coerce _): _*)

  /**
    */
  def upsertNel(key: Key, values: NonEmptyList[Value]): F[Id] =
    append((key -> values.head.some), (values.tail map (key -> _.some)): _*)

  /**
    */
  def upsertNem[K2: Order, V2](
      key: Key,
      k2v2s: NonEmptyMap[K2, V2]
  )(implicit asValue: (K2, V2) <~< Value): F[Id] =
    upsertNel(key, k2v2s.toNel map (asValue coerce _))
}

/** placeholder */
object KeyValueStore {}
