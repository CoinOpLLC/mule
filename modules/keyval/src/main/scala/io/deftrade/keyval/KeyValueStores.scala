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

import cats.implicits._
import cats.{ Order }
import cats.kernel.Monoid
import cats.data.{ NonEmptyList }

import cats.effect.Sync

import eu.timepit.refined
import refined.cats.refTypeOrder

import shapeless.labelled.FieldType
import shapeless.syntax.singleton._

import io.circe.{ Decoder, Encoder }

import scala.collection.immutable.SortedMap

/**
  */
abstract class KeyValueStores[K: Order, V] extends Stores[V] { self =>

  /** `Foo` tables are indexed by `Foo.Key`s
    */
  final type Key = K

  /** [[Key]] column type literal witness - same purpose as [[id]].
    */
  final val key = Symbol("key").witness

  /** The ''labeled'' type of the [[Key]] column.
    */
  final type KeyField = FieldType[key.T, K]

  /** A type representing the sum of all `Spec`s for a given `Key`.
    */
  type NelSpec

  protected implicit def nelSpecMonoid: Monoid[NelSpec]

  implicit val IsV: ValueSpec =:= V

  /** `NonEmptyList[Option[ValueSpec[K, V]]]` is an idiom for a natural number of csv rows
    */
  def fromSpec(os: Option[Spec]): NonEmptyList[Option[ValueSpec]]

  /**
    */
  def toSpec(vs: NonEmptyList[ValueSpec]): Spec

  /**
    */
  def toNelSpec(spec: Spec): NelSpec

  /** Whether we return a `Some`thing or a `Nothing` depends on the `Spec`.
    */
  def empty: Option[Spec]

  /**
    */
  trait KeyValueStore[F[_]] extends Store[F] {

    final type Row = (K, Option[V])

    /** Uncached.
      *
      * Note: `peek` cannot be used to distinguish whether an `Id` was never written,
      * or has been written and subsequently [[del]]eted. Both cases return `F[None]`.
      */
    final def peek(id: Id)(implicit F: Sync[F]): F[Option[(Key, Spec)]] =
      rows(id) map {
        case Nil => none
        // case ((h @ (k, _)) :: t) =>  // FIXME: triggers `scalac` bug!
        //   (k -> (self toSpec (NonEmptyList(h, t) map {
        case ((h @ (_, _)) :: t) =>
          (h._1 -> (self toSpec (NonEmptyList(h, t) map {
            case (_, Some(v)) => IsV.flip apply v
            case _            => ??? // trusting the write path
          }))).some
      }

    /**
      */
    final def probe(key: Key)(implicit F: Sync[F]): F[Option[Id]] =
      probeAll(key) map (_.headOption)

    /** Nota Bene - Uncached.
      */
    final def probeAll(key: Key)(implicit F: Sync[F]): F[List[Id]] =
      storeLookup(key) map { _ map { case (id, _) => id } }

    /** @return data from all [[Spec]]s ever associated with the given [[Key]],
      * aggregated as a single [[NelSpec]] via a [[cats.kernel.Monoid]] instance for that type.
      */
    final def getAll(key: Key)(implicit F: Sync[F]): F[Option[NelSpec]] =
      for {
        ivs <- storeLookup(key)
      } yield
        ivs match {
          case Nil => none
          case (_, Nil) :: Nil =>
            self.empty.fold(none[NelSpec])(spec => (self toNelSpec spec).some)
          case h :: t =>
            def specs = NonEmptyList(h, t) map {
              case (_, Nil)    => ??? // won't reach here: see List[V] === Nil case above
              case (_, h :: t) => self toSpec (IsV substituteContra NonEmptyList(h, t))
            }
            specs.map(self toNelSpec _).fold.some
        }

    /** @return the [[Spec]] associated with the given [[Key]], if present.
      */
    final def get(key: Key)(implicit F: Sync[F]): F[Option[Spec]] =
      for {
        hit  <- cacheLookup(key)
        miss <- storeLookup(key)
        last = if (hit.isEmpty) miss.headOption else none
        _ <- last match {
              case Some((id, vs @ (_ :: _))) => cacheFill(id, vs map (key -> _.some))
              case _                         => ().pure[F]
            }
      } yield {
        val mkSpec: List[Value] => Option[Spec] = {
          case Nil    => self.empty
          case h :: t => self.toSpec(IsV substituteContra NonEmptyList(h, t)).some
        }
        hit
          .map(_._2)
          .fold {
            miss.map(_._2).reduceLeftOption((r, _) => r).fold(self.empty)(mkSpec)
          }(mkSpec)
      }

    /** Fast. No uncached reads in the path.
      */
    final def put(key: Key, spec: Spec)(implicit F: Sync[F]): F[Id] = {
      val NonEmptyList(h, t) = IsV.liftCo substituteCo (self fromSpec spec.some)
      append(key -> h, (t map (key -> _)): _*)
    }

    /** Uncached.
      *
      * TODO: revisit this. Could do cheap in-memory set inclusion for `key`s.
      */
    final def let(key: Key, spec: Spec)(implicit F: Sync[F]): F[Option[Id]] =
      probe(key)
        .flatMap {
          case None    => put(key, spec) map (_.some)
          case Some(_) => none.pure[F]
        }

    /** Uncached. TODO: revisit this.
      */
    final def set(key: Key, spec: Spec)(implicit F: Sync[F]): F[Option[Id]] =
      probe(key)
        .flatMap {
          case None    => none.pure[F]
          case Some(_) => put(key, spec) map (_.some)
        }

    /** Empty `Value` memorializes (persists) `delete` for a given `key`:
      * this row gets an `Id`!
      *
      * Uncached. TODO: revisit this.
      */
    final def del(key: Key)(implicit F: Sync[F]): F[Option[Id]] =
      probe(key)
        .flatMap {
          case None    => none.pure[F]
          case Some(_) => append(key -> none) map (_.some)
        }

    /** Default behavior of "always miss" is overrideable.
      */
    protected def cacheLookup(key: Key)(implicit F: Sync[F]): F[Option[(Id, List[Value])]] = // F[Option[Spec]] =
      none.pure[F]

    /**
      */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final protected def storeLookup(key: Key)(implicit F: Sync[F]): F[List[(Id, List[Value])]] =
      // import Key._
      records
        .filter { case (_, (k, _)) => k === key }
        .groupAdjacentBy({ case (id, _) => id })
        .fold(List.empty[(Id, List[Value])]) {
          case (acc, (id, chunks)) =>
            chunks.map { case (_, (_, ov)) => ov }.toList match {
              case None :: Nil => List.empty[(Id, List[Value])] // buh bye
              case ovs         => (id, ovs map (_.fold(???)(identity))) :: acc // reverses order
            }
        }
        .compile
        .lastOrError
  }
}

/**
  */
final object KeyValueStores {

  /**
    */
  abstract class Codec[K: Order, T, U](
      f: Option[T] => NonEmptyList[Option[U]],
      g: NonEmptyList[U] => T
  )(
      implicit final val IsV: U =:= U
  ) extends KeyValueStores[K, U] {

    final type Spec      = T
    final type NelSpec   = NonEmptyList[T]
    final type ValueSpec = U

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      g(vs)

    final def fromSpec(os: Option[Spec]): NonEmptyList[Option[ValueSpec]] =
      f(os)

    final def toNelSpec(spec: Spec): NelSpec =
      NonEmptyList one spec

    final def empty: Option[Spec] =
      none

    final implicit protected def nelSpecMonoid: Monoid[NelSpec] =
      Monoid[NonEmptyList[Spec]]
  }

  /**
    */
  abstract class SimpleCodec[K: Order, T, U](
      f: T => U,
      g: U => T
  ) extends KeyValueStores.Codec[K, T, U](
        ot => ot.fold(NonEmptyList one none[U])(t => NonEmptyList one f(t).some),
        us => g(us.head)
      )

  /**
    */
  abstract class SADT[K: Order, V: Encoder: Decoder]
      extends KeyValueStores.SimpleCodec[K, V, keyval.SADT](
        v => SADT from v,
        u => { val Right(ret) = u.json.as[V]; ret }
      )

  /** (Single, scalar) `Value`
    */
  abstract class KV[K: Order, V](
      implicit
      final val IsV: V =:= V
  ) extends KeyValueStores[K, V] {

    final type Spec      = V
    final type NelSpec   = NonEmptyList[V]
    final type ValueSpec = V

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      vs match {
        case NonEmptyList(v2, Nil) => v2
        case _                     => ??? // trusting the write path
      }

    final def fromSpec(os: Option[Spec]): NonEmptyList[Option[ValueSpec]] =
      NonEmptyList(os, Nil)

    final def toNelSpec(spec: Spec): NelSpec =
      NonEmptyList one spec

    final def empty: Option[Spec] =
      none

    final implicit protected def nelSpecMonoid: Monoid[NelSpec] =
      Monoid[NonEmptyList[Spec]]
  }

  /** Map of `K2`s and `V2`s
    */
  abstract class MKV[K: Order, V, K2: Order, V2](
      implicit
      final val IsV: (K2, V2) =:= V
  ) extends KeyValueStores[K, V] {

    final type Spec      = Map[K2, V2]
    final type NelSpec   = Map[K2, NonEmptyList[V2]]
    final type ValueSpec = (K2, V2)

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      SortedMap(vs.toList: _*)

    final def fromSpec(os: Option[Spec]): NonEmptyList[Option[ValueSpec]] = {

      def delNel = NonEmptyList(none[(K2, V2)], Nil) // idiom for `delete` command memo

      os.fold(delNel) {
        _.toList match {
          case Nil       => delNel
          case kv :: kvs => NonEmptyList(kv.some, kvs map (_.some))
        }
      }
    }

    final def toNelSpec(spec: Spec): NelSpec =
      spec map { case (k, v) => k -> (NonEmptyList one v) }

    final def empty: Option[Spec] =
      SortedMap.empty[K2, V2].some

    final implicit protected def nelSpecMonoid: Monoid[NelSpec] =
      Monoid[Map[K2, NonEmptyList[V2]]]
  }
}
