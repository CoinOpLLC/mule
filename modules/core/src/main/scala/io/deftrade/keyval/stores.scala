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
import cats.effect.Effect
import io.getquill.ast.Val

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
  final type Record = (Id, Row)

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

  /** Spec[V] => type function of V specifying what is returned from a `get` or stored by a `put`
    */
  type Spec

  /** Complete In-memory representation.
    */
  type Repr[_]

  /**
    */
  def loadSpec: StreamF[Record] => (Spec, StreamF[Record]) = ???
  def load: Record PipeF Spec                              = _ => ???

  def saveSpec: Spec => StreamF[Record] = _ => ???
  def save: Spec PipeF Record           = _ => ???

  def view: StreamF[Spec] => Repr[Spec] = _ => ???
  def dump: Repr[Spec] => StreamF[Spec] = _ => ???

  /**
    */
  def has(id: Id): EffectType[Boolean] =
    (records exists (_._1 === id)).compile.lastOrError

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    */
  protected def records: StreamF[Record]

  // /**
  //   */
  // final protected def append(row: Row): EffectType[Id] = append(row)

  /**
    * Note this returns a ''single'' `Id` for the whole sequence of `Row`s.
    *
    * This feature - the ability to assign multiple rows a single `Id` computed over those all
    * of those rows - is why ''this'' method is the abstract primitive (and not [[append]]).
    *
    * Appends to the backing store whether or not there is a duplicate (no checking).
    *
    * FIXME: not thread safe: put a `Ref` based queue in front
    */
  final protected def append(row: Row, rows: Row*): EffectType[Id] =
    for {
      id <- F delay nextId(row, rows: _*)
      rs <- F delay (row +: rows).toList
      _ <- (Stream evals (F delay (rs map { r =>
               cache(id, r)
               (id, r)
             })) through persist).compile.drain
    } yield id

  /** overrideable with default nop
    */
  protected def cache(id: Id, row: Row, rows: Row*): Unit = ()

  /**
    */
  protected def persist: Record PipeF Unit

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
  abstract class Aux[F[_], W[_] <: WithValue, V](
      final val V: W[V]
  )(implicit
      final val F: Sync[F],
      final val X: ContextShift[F]
  ) extends Store {

    final type EffectType[x] = F[x]

    /**
      */
    def listPipe[F[_], A]: Stream[F, A] => Stream[F, List[A]] =
      _.fold(List.empty[A])((vs, v) => v :: vs) map (_.reverse)
  }
}

@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStore[F[_], V] {
  self: Store.Aux[F, WithId.Aux, V] =>

  import V._

  /**
    * Binds `Repr`, but not `Spec`
    */
  final type Repr[SP] = Map[Id, SP]

  /** overrideable
    */
  protected def lookup(id: Id): EffectType[List[Row]] = // lookups(id).compile.toList
    List.empty.pure[EffectType]

  /** define whichever is easer
    */
  protected def lookups(id: Id): StreamF[Row] = Stream evalSeq lookup(id)
  // Stream.empty

  /**
    * Get `Stream`.
    *
    * Returns ''all'' `Row`s with the given `Id` (none, if not found) as an [[fs2.Stream]].
    */
  final protected def rows(id: Id): StreamF[Row] =
    for {
      cache  <- lookups(id).noneTerminate
      misses <- records filter (_._1 === id) map (_._2)
    } yield cache.fold(misses)(identity)

  /**
    */
  def get(id: Id): EffectType[Option[Spec]]

  /**
    */
  def put(spec: Spec): EffectType[(Id, Boolean)]
}

// protected final type SpecNEM[K2, V2]  = NonEmptyMap[K2, V2]
// protected final type SpecML[K2, V2]   = Map[K2, List[V2]]
// protected final type SpecNEML[K2, V2] = NonEmptyMap[K2, List[V2]]

/**
  */
object ValueStore {}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreV[F[_], V] extends ValueStore[F, V] {

  self: Store.Aux[F, WithId.Aux, V] =>

  import V._

  /**
    */
  final type Spec = Value

  /**
    */
  def get(id: Id): EffectType[Option[Spec]] =
    for {
      cache <- lookup(id)
      store <- (records filter (_._1 === id) map (_._2)).compile.last
    } yield cache.headOption.fold(store)(_ => cache.headOption) // lawful evil

  /**
    */
  def put(spec: Spec): EffectType[(Id, Boolean)] = {
    val id = nextId(spec)
    for {
      x <- has(id)
      ret <- if (x) (id, false).pure[F]
             else append(spec) map ((_, true))
    } yield ret
  }
}

@SuppressWarnings(Array("org.wartremover.warts.Any"))
protected trait VSnel[F[_], V] extends ValueStore[F, V] {

  self: Store.Aux[F, WithId.Aux, V] =>

  import V._

  /**
    */
  protected def getNel(id: Id): EffectType[Option[NonEmptyList[V]]] =
    for {
      cache <- lookups(id).compile.toList
      store <- rows(id).compile.toList
    } yield cache.headOption.fold(store.toNel)(_ => cache.toNel) // lawful evil

  /**
    */
  protected def putNel(spec: NonEmptyList[V]): EffectType[(Id, Boolean)] = {
    val id = nextId(spec.head, spec.tail: _*)
    for {
      x <- has(id)
      ret <- if (x) (id, false).pure[F]
             else append(spec.head, spec.tail: _*) map ((_, true))
    } yield ret
  }
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreNEL[F[_], V] extends VSnel[F, V] {

  self: Store.Aux[F, WithId.Aux, V] =>

  import V._

  /**
    */
  final type Spec = NonEmptyList[Value]

  /**
    */
  @inline final def get(id: Id): EffectType[Option[Spec]] =
    getNel(id)

  /**
    */
  @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
    putNel(spec)
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreNEM[F[_], K2, V2] extends VSnel[F, (K2, V2)] {

  self: Store.Aux[F, WithId.Aux, (K2, V2)] =>

  import V._

  implicit def k2v2AsV: (K2, V2) === Value =
    Is unsafeFromPredef implicitly[(K2, V2) =:= Value]

  /**
    */
  final type Spec = NonEmptyMap[K2, V2]

  /**
    */
  @inline final def get(id: Id): EffectType[Option[Spec]] =
    ??? // getNel(id)

  /**
    */
  @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
    ??? // putNel(spec)
  //     /**
  //   */
  // def putm[K2: Order, V2](
  //     k2v2s: NonEmptyMap[K2, V2]
  // )(implicit asV: (K2, V2) <~< V): F[(Id, Boolean)] =
  //   putl(k2v2s.toNel map (asV coerce _))

}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreNEML[F[_], K2, V2] extends VSnel[F, (K2, Option[V2])] {

  self: Store.Aux[F, WithId.Aux, (K2, Option[V2])] =>

  import V._

  implicit def k2lv2AsV: (K2, Option[V2]) === Value =
    Is unsafeFromPredef implicitly[(K2, Option[V2]) =:= Value]

  /**
    */
  final type Spec = NonEmptyMap[K2, List[V2]]

  /**
    */
  @inline final def get(id: Id): EffectType[Option[Spec]] =
    ??? // getNel(id)
  // def getml[K2: Order, V2](
  //     id: Id
  // )(implicit
  //     asK2V2: V <~< (K2, V2)
  // ): EffectType[Map[K2, List[V2]]] =
  //   for (values <- getl(id))
  //     yield values.groupBy(v => (asK2V2 coerce v)._1) map {
  //       case (k2, vs) => (k2, vs map (v => (asK2V2 coerce v)._2))
  //     }

  /**
    */
  @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
    ??? // putNel(spec)
  //     /**
  //   */
  // def putml[K2: Order, V2](
  //     k2lv2s: NonEmptyMap[K2, List[V2]]
  // )(implicit asV: (K2, Option[V2]) <~< V): F[(Id, Boolean)] =
  //   putl(k2lv2s.toNel flatMap {
  //     case (k2, hv2 :: tv2s) =>
  //       NonEmptyList(asV coerce k2 -> hv2.some, tv2s map (v2 => asV coerce k2 -> v2.some))
  //     case (k2, Nil) =>
  //       NonEmptyList one (asV coerce k2 -> none)
  //   })

}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
protected trait VSlist[F[_], V, V2] extends ValueStore[F, V] {

  self: Store.Aux[F, WithId.Aux, V] =>

  import V._

  /**
    */
  protected implicit def IsV: Option[V2] === V

  /**
    */
  protected def getList(id: Id): EffectType[Option[List[V2]]] =
    for {
      cache <- lookups(id).compile.toList
      store <- rows(id).compile.toList
    } yield (cache.headOption.fold(store)(_ => cache) map IsV.flip.coerce).sequence

  /** FIXME: abstract the duplicate folding
    */
  protected def putList(spec: List[V2]): EffectType[(Id, Boolean)] = {
    val id = spec.headOption.fold(
      nextId(IsV coerce none[V2])
    )(h => nextId(IsV coerce h.some, spec drop 1 map (IsV coerce _.some): _*))
    for {
      x <- has(id)
      ret <- if (x) (id, false).pure[F]
             else
               spec.headOption.fold(
                 append(IsV coerce none[V2])
               )(h => append(IsV coerce h.some, spec drop 1 map (IsV coerce _.some): _*)) map {
                 (_, true)
               }
    } yield ret
  }
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreL[F[_], V, V2] extends VSlist[F, V, V2] {

  self: Store.Aux[F, WithId.Aux, V] =>

  import V._

  /**
    */
  final type Spec = List[V2]

  /**
    */
  @inline final def get(id: Id): EffectType[Option[Spec]] =
    getList(id)

  /**
    */
  @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
    putList(spec)
}

/** `Option[(K2, V2)] === V`
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreM[F[_], V, K2, V2] extends VSlist[F, V, (K2, V2)] {

  self: Store.Aux[F, WithId.Aux, V] =>

  import V._

  implicit def K2: Order[K2]

  /**
    */
  final type Spec = Map[K2, V2]

  /**
    */
  @inline final def get(id: Id): EffectType[Option[Spec]] =
    getList(id) map {
      _ map { SortedMap(_: _*) }
    }

  /**
    */
  @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
    putList(spec.toList)
}

/** Option[(K2, Option[V2])] === V
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait ValueStoreML[F[_], V, K2, V2] extends VSlist[F, V, (K2, Option[V2])] {

  self: Store.Aux[F, WithId.Aux, V] =>

  import V._

  /**
    */
  final type Spec = Map[K2, List[V2]]

  /**
    */
  @inline final def get(id: Id): EffectType[Option[Spec]] =
    ??? // getList(id)

  /**
    */
  @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
    ??? // putList(spec)
  // /**
  //   */
  // def putml[K2: Order, V2](
  //     k2lv2s: Map[K2, List[V2]]
  // )(implicit asV: Option[(K2, Option[V2])] <~< V): F[(Id, Boolean)] =
  //   putl[(K2, Option[V2])](k2lv2s.toList flatMap {
  //     case (k2, Nil) => List(k2 -> none)
  //     case (k2, v2s) => v2s map (v2 => k2 -> v2.some)
  //   })
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait KeyValueStoreV[F[_], K, V] {

  self: Store.Aux[F, WithKey.Aux[K, *], V] =>

  import V._, Key._

  /**
    */
  final type Repr[A] = Map[K, Option[(Id, A)]]

  /** What the api client can get a copy of.
    */
  type Shape[_]

  /**
    */
  def snapshot: Shape[Spec] => StreamF[Spec] = _ => ???

  /**
    */
  def spinup: StreamF[Spec] => Shape[Spec] = _ => ???

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

  self: Store.Aux[F, WithKey.Aux[K, *], V] =>

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
    append((key -> values.head.some), (values.tail map (key -> _.some)): _*)

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
