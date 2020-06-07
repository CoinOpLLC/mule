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

import cats.effect.{ ContextShift, Sync }

import eu.timepit.refined
import refined.api.Refined
import refined.cats.refTypeOrder

import fs2.{ Stream }

/** */
protected trait StoreTypes {

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
protected[deftrade] object StoreTypes {

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
  protected def indexFrom(pr: (Id, Row)): Index

  /**
    * Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    *
    * TODO: This needs to evolve to use [[Result]]
    */
  def idRows: EffectStream[(Id, Row)]

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
    idRows filter (ir => indexFrom(ir) == x) map (_._2)

  /**
    * Like [[getAll]], but returns `Row`s as a [[scala.collection.immutable.List List]]
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getList(x: Index): EffectStream[List[Row]] =
    getAll(x) through listPipe

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
  // import V._
}

/**  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait KeyValueStore[F[_], K, V] extends Store[F, WithKey.Aux[K, *], V] {
  self: StoreTypes.Aux[F, WithKey.Aux[K, *], V] =>

  import V._

  /** */
  final protected def indexFrom(pr: (Id, Row)): Index =
    pr match {
      case (_, (key, _)) => key
    }

  /** */
  def select(key: Key): EffectStream[Value]

  /** */
  final def selectAll(key: Key): EffectStream[Value] =
    for {
      ov    <- getAll(key) map (_._2) takeWhile (_.isDefined)
      value <- Stream evals F.delay { ov }
    } yield value

  /**  */
  final def selectList(key: Key): EffectStream[List[Value]] =
    selectAll(key) through listPipe

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
  final def upsertAll(key: Key, value: Value, values: Value*): EffectStream[Id] =
    append(key -> value.some)
}
