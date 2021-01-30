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
import cats.data.{ NonEmptyList, NonEmptyMap }

import io.circe.{ Decoder, Encoder }

import scala.collection.immutable.SortedMap

/**
  */
abstract class ValueStores[V] extends Stores[V] { param =>

  final type Row = V

  /**
    */
  type Spec

  /**
    */
  type ValueSpec

  /**
    */
  implicit val IsV: ValueSpec =:= V

  /**
    */
  def fromSpec(s: Spec): NonEmptyList[ValueSpec]

  /**
    */
  def toSpec(vs: NonEmptyList[ValueSpec]): Spec

  /**
    */
  final def toSpecOption(vs: List[ValueSpec]): Option[Spec] =
    vs match {
      case Nil    => none
      case h :: t => toSpec(NonEmptyList(h, t)).some
    }

  /**
    */
  trait ValueStore[F[_]] extends Store[F] {

    final def get(id: Id): F[Option[Spec]] =
      for {
        row <- cachedRows(id)
      } yield param toSpecOption (param.IsV substituteContra row)

    final def put(spec: Spec): F[(Id, Boolean)] = {

      val NonEmptyList(h, t) = param.IsV.liftCo(param fromSpec spec)

      val id = nextId(h, t: _*)
      for {
        x <- hasId(id)
        ret <- if (x) (id, false).pure[F]
              else append(h, t: _*) map ((_, true))
      } yield ret
    }

    /** overrideable
      */
    protected def cacheLookup(id: Id): F[List[Row]] =
      List.empty.pure[F]

    /** interrogates and fills cacheFill
      */
    final protected def cachedRows(id: Id): F[List[Row]] =
      for {
        hit  <- cacheLookup(id)
        miss <- rows(id)
        _    <- if (hit.isEmpty) cacheFill(id, miss) else ().pure[F]
      } yield if (hit.nonEmpty) hit else miss
  }
}

/**
  */
object ValueStores {

  /**
    */
  abstract class Codec[T, U](
      f: T => NonEmptyList[U],
      g: NonEmptyList[U] => T
  )(implicit final val IsV: U =:= U)
      extends ValueStores[U] {

    final type Key2   = Nothing
    final type Value2 = T

    final type Spec      = T
    final type ValueSpec = U

    final def fromSpec(s: Spec): NonEmptyList[ValueSpec] =
      f(s)

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      g(vs)
  }

  /** Serialized ADTs
    *
    *  `SADT` stores are content-addressed: entries are indexed with their own `sha`.
    *
    * Therefore, if you have a `sha` (from a [[model.Transaction]], for instance) ''and'' access to
    * a `SADT` key value store containing the value, you have access to the value itself.
    *
    * Note this value is forgery resistant (up to the strength of the `sha`).
    */
  abstract class SADT[V: Encoder: Decoder](
      implicit
      isV: V =:= V
  ) extends ValueStores.Codec[V, keyval.SADT.Aux[V]](
        v => NonEmptyList one (SADT from v),
        nel => {
          val Right(ret) = nel.head.decoded
          ret
        }
      )

  /** Values
    */
  abstract class VS[V](
      implicit
      final val IsV: V =:= V
  ) extends ValueStores[V] {

    final type Spec      = V
    final type ValueSpec = V

    final def fromSpec(s: Spec): NonEmptyList[ValueSpec] =
      NonEmptyList one s

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      vs match {
        case NonEmptyList(v, Nil) => v
        case _                    => ???
      }
  }

  /** Lists of Values
    */
  abstract class LV[V, W](
      implicit
      final val IsV: Option[W] =:= V
  ) extends ValueStores[V] {

    final type Spec      = List[W]
    final type ValueSpec = Option[W]

    final def fromSpec(s: Spec): NonEmptyList[ValueSpec] =
      s match {
        case Nil    => NonEmptyList one none
        case h :: t => NonEmptyList(h, t) map (_.some)
      }

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      vs match {
        case NonEmptyList(_, Nil) => List.empty
        case nel                  => nel.toList map (_.fold(???)(identity))
      }
  }

  /** Maps of `(K2 -> V2)`s
    */
  abstract class MKV[V, K2: Order, V2](
      implicit
      final val IsV: Option[(K2, V2)] =:= V
  ) extends ValueStores[V] {

    final type Spec      = Map[K2, V2]
    final type ValueSpec = Option[(K2, V2)]

    final def fromSpec(s: Map[K2, V2]): NonEmptyList[ValueSpec] =
      s.toList match {
        case Nil    => NonEmptyList one none
        case h :: t => NonEmptyList(h, t) map (_.some)
      }

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      vs match {
        case NonEmptyList(None, Nil) => Map.empty
        case nel                     => (nel.toList map (_.fold(???)(identity))).toMap
      }
  }

  /** Maps of (Key -> List[Value])s
    */
  abstract class MKLV[V, K2: Order, V2](
      implicit
      final val IsV: Option[(K2, Option[V2])] =:= V
  ) extends ValueStores[V] {

    final type Spec      = Map[K2, List[V2]]
    final type ValueSpec = Option[(K2, Option[V2])]

    final def fromSpec(s: Spec): NonEmptyList[ValueSpec] =
      s.toList flatMap {
        case (k, Nil) => (k -> none[V2]).some :: Nil
        case (k, vs)  => vs map (v => (k -> v.some).some)
      } match {
        case Nil    => NonEmptyList one none
        case h :: t => NonEmptyList(h, t)
      }

    /**
      */
    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      vs match {
        case NonEmptyList(None, Nil)            => Map.empty
        case NonEmptyList(Some((k, None)), Nil) => Map(k -> List.empty)
        case NonEmptyList(h, t) =>
          h :: t map (_.fold(???)(identity)) groupBy (_._1) map {
            case (k, Nil) => k -> Nil
            case (k, kvos) =>
              k -> kvos.map {
                case (_, Some(v)) => v
                case _            => ???
              }
          }
      }
  }

  /** Non Empty Lists of Values
    */
  abstract class NELV[V](
      implicit
      final val IsV: V =:= V
  ) extends ValueStores[V] {

    final type Spec      = NonEmptyList[V]
    final type ValueSpec = V

    final def fromSpec(s: Spec): NonEmptyList[ValueSpec] =
      s

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      vs
  }

  /** Non Empty Maps of (Key -> Values)s
    */
  abstract class NEMKV[V, K2: Order, V2](
      implicit
      final val IsV: (K2, V2) =:= V
  ) extends ValueStores[V] {

    final type Spec      = NonEmptyMap[K2, V2]
    final type ValueSpec = (K2, V2)

    final def fromSpec(s: Spec): NonEmptyList[ValueSpec] =
      s.toNel

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      vs.toNem
  }

  /** Non Empty Maps of (Key -> List[Value])s
    */
  abstract class NEMKLV[V, K2: Order, V2](
      implicit
      final val IsV: (K2, Option[V2]) =:= V
  ) extends ValueStores[V] {
    final type Spec      = NonEmptyMap[K2, List[V2]]
    final type ValueSpec = (K2, Option[V2])

    final def fromSpec(s: Spec): NonEmptyList[ValueSpec] =
      s.toNel flatMap {
        (_: (K2, List[V2])) match {
          case (k, Nil)    => NonEmptyList one (k -> none)
          case (k, h :: t) => NonEmptyList((k -> h.some), t map (v => (k -> v.some)))
        }
      }

    final def toSpec(vs: NonEmptyList[ValueSpec]): Spec =
      vs match {
        case NonEmptyList((k, None), Nil)    => NonEmptyMap(k -> Nil, SortedMap.empty)
        case NonEmptyList((k, Some(v)), Nil) => NonEmptyMap(k -> List(v), SortedMap.empty)
        case _                               => ???
      }
  }
}
