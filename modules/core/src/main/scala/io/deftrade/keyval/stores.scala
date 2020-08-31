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
  * Note the only way to get an Id is as the result of an effectful mutation or probe.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait Store {

  /**
    */
  val V: WithValue

  import V._

  /**
    */
  type EffectType[_]

  /**
    */
  final type StreamF[A] = Stream[EffectType, A]

  /**
    */
  final type PipeF[A, B] = Pipe[EffectType, A, B]

  /**
    */
  implicit def F: Sync[EffectType]

  /**
    */
  implicit def X: ContextShift[EffectType]

  /**
    */
  type Spec

  /** What the api client can get a copy of.
    */
  type Shape[_]

  /** Complete In-memory representation.
    */
  type Repr[_]

  /**
    */
  final type Record = (Id, Row)

  /**
    */
  def load: StreamF[Record] => Repr[Spec] = _ => ???
  def save: Repr[Spec] => StreamF[Record] = _ => ???

  /**
    */
  def ingest: Shape[Spec] => Repr[Spec]   = _ => ???
  def snapshot: Repr[Spec] => Shape[Spec] = _ => ???

  /**
    */
  def has(id: Id): EffectType[Boolean] =
    (records exists (_._1 === id)).compile.lastOrError

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    */
  protected def records: StreamF[Record]

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
  final protected def append(row: Row): EffectType[Id] =
    for {
      id <- F delay nextId(row)
      r  <- F delay row
      _ <- (Stream eval F.delay {
               cache(id, r)
               (id, r)
             } through persist).compile.drain
    } yield id

  /** overrideable with default nop
    */
  protected def cache(id: Id, row: Row): Unit = ()

  /**
    */
  protected def persist: Pipe[EffectType, (Id, Row), Unit]

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
  def rows: StreamF[Row] = records map (_._2)
}

/** Types that represent the items that can be stored at the API level.
  */
object Store {

  /**
    */
  abstract class AuxV[F[_], W[_] <: WithValue, V](
      final val V: W[V]
  )(implicit
      final val F: Sync[F],
      final val X: ContextShift[F]
  ) extends Store {

    final type EffectType[x] = F[x]
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  abstract class AuxL[F[_], W[_] <: WithValue, V](
      final val V: W[V]
  )(implicit
      final val F: Sync[F],
      final val X: ContextShift[F]
  ) extends Store {

    final type EffectType[x] = F[x]

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
    final protected def appends(row: V.Row, rows: V.Row*): F[V.Id] =
      for {
        id <- F delay nextId(row, rows: _*)
        rs <- F delay (row +: rows).toList
        _ <- (Stream evals (F delay (rs map { r =>
                 cache(id, r)
                 (id, r)
               })) through persist).compile.drain
      } yield id
  }

  /**
    */
  def listPipe[F[_], A]: Stream[F, A] => Stream[F, List[A]] =
    _.fold(List.empty[A])((vs, v) => v :: vs) map (_.reverse)

}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreV[F[_], V] {

  self: Store.AuxV[F, WithId.Aux, V] =>

  import V._

  final type Repr[A] = Map[Id, A]

  /** overrideable
    */
  protected def lookup(id: Id): EffectType[Option[Row]] = none.pure[EffectType]

  /**
    */
  def get(id: Id): EffectType[Option[Row]] =
    for {
      cache <- lookup(id)
      store <- (records filter (_._1 === id) map (_._2)).compile.last
    } yield cache.fold(store)(_ => cache) // lawful evil

  /**
    */
  def put(row: Row): EffectType[(Id, Boolean)] = {
    val id = nextId(row)
    for {
      x <- has(id)
      ret <- if (x) (id, false).pure[F]
             else append(row) map ((_, true))
    } yield ret
  }
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreM[F[_], V] {

  self: Store.AuxL[F, WithId.Aux, V] =>

  import V._

  // import syntax._

  final type Repr[A] = Map[Id, A]

  /**
    */
  protected def lookups(id: Id): StreamF[Row] = Stream.empty

  /**
    * Get `Stream`.
    *
    * Returns ''all'' `Row`s with the given `Id` (none, if not found) as an [[fs2.Stream]].
    */
  def gets(id: Id): StreamF[Row] =
    for {
      cache  <- lookups(id).noneTerminate
      misses <- records filter (_._1 === id) map (_._2)
    } yield cache.fold(misses)(identity)

  /**
    * Like [[get]], but returns `Row`s as a [[scala.collection.immutable.List List]]
    */
  def getl(id: Id): StreamF[List[Row]] =
    gets(id) through Store.listPipe

  /**
    * Like [[get]], but returns `Row`s as a [[scala.collection.immutable.Map Map[K2, V2]]]
    */
  def getm[K2: Order, V2](id: Id)(implicit asK2V2: Value <~< (K2, V2)): StreamF[Map[K2, V2]] =
    for (values <- getl(id))
      yield SortedMap(values map (asK2V2 coerce _): _*)

  /**
    * Like [[getm]], but for `Map[K2, NEL[V2]]`
    */
  def getml[K2: Order, V2](
      id: Id
  )(implicit
      asK2V2: V <~< (K2, V2)
  ): StreamF[Map[K2, List[V2]]] =
    for (values <- getl(id))
      yield values.groupBy(v => (asK2V2 coerce v)._1) map {
        case (k2, vs) => (k2, vs map (v => (asK2V2 coerce v)._2))
      }

  /**
    * Put `Seq`.
    *
    * TODO: Ugly double hash calc can be optimized later.
    * Also, is ''never'' necessary for chained hash.
    */
  def puts(row: Row, rows: Row*): F[(Id, Boolean)] = {
    val id = nextId(row, rows: _*)
    for {
      haz <- has(id)
      ret <- if (haz) (id, false).pure[F]
             else appends(row, rows: _*) map ((_, true))
    } yield ret
  }

  /**
    */
  def putl(values: NonEmptyList[V]): F[(Id, Boolean)] =
    puts(values.head, values.tail: _*)

  /**
    */
  def putm[K2: Order, V2](
      k2v2s: NonEmptyMap[K2, V2]
  )(implicit asV: (K2, V2) <~< V): F[(Id, Boolean)] =
    putl(k2v2s.toNel map (asV coerce _))

  /** FIXME: evidently need a way to encode Option[V2] fuuUUUUuu
    */
  def putml[K2: Order, V2](
      k2lv2s: NonEmptyMap[K2, List[V2]]
  )(implicit asV: (K2, V2) <~< V): F[(Id, Boolean)] =
    putl(k2lv2s.toNel flatMap {
      case (k2, hv2 :: tv2s) =>
        NonEmptyList(asV coerce k2 -> hv2, tv2s drop 1 map (v2 => asV coerce k2 -> v2))
      case (k2, Nil) =>
        NonEmptyList one (asV coerce k2 -> ???)
    })
}

/**
  */
object ValueStore

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait KeyValueStoreV[F[_], K, V] {

  self: Store.AuxV[F, WithKey.Aux[K, *], V] =>

  import V._, Key._

  /**
    */
  final type Repr[A] = Map[K, Option[(Id, A)]]

  /**
    */
  protected def recordsWith(key: Key): StreamF[Record] =
    records filter (_._2._1 === key)

  /** TODO: revisit the implementation, which is tricky
    */
  protected def updateExistingKey(key: Key, maybeValue: Option[Value]): F[Option[Id]] =
    exists(key) flatMap (_.fold(none[Id].pure[F])(_ => append(key -> maybeValue) map (_.some)))

  /**
    */
  def existsAll(key: Key): StreamF[Id] =
    recordsWith(key) map (_._1)

  /**
    */
  def exists(key: Key): F[Option[Id]] =
    existsAll(key).compile.last

  /** Returns '''all''' un-[[delete]]d `Value`s for the given `Key`. */
  def gets(key: Key): StreamF[Value] =
    for {
      value <- (recordsWith(key) map (_._2._2)).unNoneTerminate
    } yield value

  /**
    */
  def get(key: Key): F[Option[Value]] =
    gets(key).compile.last

  /**
    */
  def let(key: Key, value: Value): F[Option[Id]] =
    exists(key) flatMap (_.fold(append(key -> value.some) map (_.some))(_ => none.pure[F]))

  /**
    */
  def set(key: Key, value: Value): F[Option[Id]] =
    updateExistingKey(key, value.some)

  /** Empty `Value` memorializes (persists) `delete` for a given `key` - this row gets an `Id`!
    */
  def del(key: Key): F[Option[Id]] =
    updateExistingKey(key, none)

  /**
    */
  def put(key: Key, value: Value): F[Id] =
    append(key -> value.some)
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait KeyValueStoreM[F[_], K, V] {

  self: Store.AuxL[F, WithKey.Aux[K, *], V] =>

  import V._

  final type Repr[A] = Map[Key, (Id, A)]

  /** FIXME: dedupe */
  def gets(key: Key): StreamF[Value] = ???

  /**
    */
  def getl(key: Key): F[List[Value]] =
    gets(key).compile.toList

  /**
    */
  def getm[K2: Order, V2](
      key: Key
  )(implicit asK2V2: Value <~< (K2, V2)): F[Map[K2, V2]] =
    for (values <- getl(key))
      yield SortedMap(values map (asK2V2 coerce _): _*)

  /**
    */
  def putl(key: Key, values: NonEmptyList[Value]): F[Id] =
    appends((key -> values.head.some), (values.tail map (key -> _.some)): _*)

  /**
    */
  def putm[K2: Order, V2](
      key: Key,
      k2v2s: NonEmptyMap[K2, V2]
  )(implicit asValue: (K2, V2) <~< Value): F[Id] =
    putl(key, k2v2s.toNel map (asValue coerce _))
}

/**
  */
object KeyValueStore
