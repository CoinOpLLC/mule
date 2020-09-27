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
import cats.{ Eq, Order, Show }
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.evidence._
import cats.effect.{ ContextShift, Sync }

import eu.timepit.refined
import refined.api.Refined
import refined.cats.refTypeOrder

import fs2.{ Pipe, Stream }

import scala.collection.immutable.SortedMap

/**
  * FIXME: `trait` implementation necessitates duplication of the Spec[K2, V2] === V defninitions
  * for all the `Spec`s, in the form of self type restrictions. Eliminate or prove we can't.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class Store[F[_], W[_] <: WithValue, V](
    final val V: W[V]
)(implicit
    final val F: Sync[F],
    final val X: ContextShift[F]
) {

  import V._

  /** Spec[V] => type function of V specifying what is returned from a `get` or stored by a `put`
    */
  type Spec

  /** Complete In-memory representation.
    */
  type Repr[_]

  /**
    */
  final type Record = (Id, Row)

  /**
    */
  final type EffectType[A] = F[A]

  /**
    */
  final type StreamF[A] = Stream[EffectType, A]

  /**
    */
  final type PipeF[A, B] = Pipe[EffectType, A, B]

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
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
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

  /**
    */
  protected def listPipe[A]: A PipeF List[A] =
    _.fold(List.empty[A])((vs, v) => v :: vs) map (_.reverse)

}

/**
  */
object Store {}

/**
  * Note the only way to get an Id is as the result of an effectful mutation or probe.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class ValueStore[F[_]: Sync: ContextShift, V](
    v: WithId.Aux[V]
) extends Store[F, WithId.Aux, V](v) {

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

/**
  */
object ValueStore {

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  sealed trait Param { param =>

    /**
      */
    type Spec[K, V]

    /**
      */
    type ValueSpec[K, V]

    /**
      */
    def fromSpec[K, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]]

    /**
      */
    def toSpec[K, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V]

    /**
      */
    final def toSpecOption[K, V](vs: List[ValueSpec[K, V]]): Option[Spec[K, V]] =
      vs match {
        case Nil    => none
        case h :: t => toSpec(NonEmptyList(h, t)).some
      }

    /**
      */
    abstract class DependentTypeThunk[V, K2, V2](
        v: WithId.Aux[V]
    )(implicit
        IsV: param.ValueSpec[K2, V2] === V
    ) {

      /**
        */
      trait ValueStore[F[_]] { self: keyval.ValueStore[F, V] =>

        import V._

        final type Spec = param.Spec[K2, V2]

        /**
          */
        def get(id: Id): F[Option[Spec]] =
          for {
            cache <- lookup(id)
            store <- (records filter (_._1 === id) map (_._2)).compile.toList
          } yield param toSpecOption (IsV.flip substitute cache.headOption.fold(store)(_ => cache))

        /**
          */
        def put(spec: Spec): F[(Id, Boolean)] = {
          val NonEmptyList(h, t) = param fromSpec spec map (IsV coerce _)
          val id                 = nextId(h, t: _*)
          for {
            x <- has(id)
            ret <- if (x) (id, false).pure[F]
                   else append(h, t: _*) map ((_, true))
          } yield ret
        }
      }
    }

  }

  /**
    */
  object Param {

    /**
      */
    case object V extends Param {
      final type Spec[K, V]      = V
      final type ValueSpec[K, V] = V

      final def fromSpec[K, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        NonEmptyList one s

      final def toSpec[K, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        vs match {
          case NonEmptyList(v, Nil) => v
          case _                    => ???
        }
    }

    /**
      */
    case object LV extends Param {
      final type Spec[K, V]      = List[V]
      final type ValueSpec[K, V] = Option[V]

      final def fromSpec[K, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        s match {
          case Nil    => NonEmptyList one none
          case h :: t => NonEmptyList(h.some, t map (_.some))
        }

      /**
        * FIXME: the typing is subtle here: only 2 cases
        * - List.empty[None]
        * - List(x.some... y.some)
        * => This feels like `Stream.unNone`
        */
      final def toSpec[K, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        vs.toList map {
          case Some(v) => v
          case _       => ???
        }
    }

    /**
      */
    case object MKV extends Param {
      final type Spec[K, V]      = Map[K, V]
      final type ValueSpec[K, V] = Option[(K, V)]
      final def fromSpec[K, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] = ???
      final def toSpec[K, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V]  = ???
    }

    /**
      */
    case object MKLV extends Param {
      final type Spec[K, V]      = Map[K, List[V]]
      final type ValueSpec[K, V] = Option[(K, Option[V])]
      final def fromSpec[K, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] = ???
      final def toSpec[K, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V]  = ???
    }

    /**
      */
    case object NELV extends Param {
      final type Spec[K, V]      = NonEmptyList[V]
      final type ValueSpec[K, V] = V
      final def fromSpec[K, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] = ???
      final def toSpec[K, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V]  = ???
    }

    /**
      */
    case object NEMKV extends Param {
      final type Spec[K, V]      = NonEmptyMap[K, V]
      final type ValueSpec[K, V] = (K, V)
      final def fromSpec[K, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] = ???
      final def toSpec[K, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V]  = ???
    }

    /**
      */
    case object NEMKLV extends Param {
      final type Spec[K, V]      = NonEmptyMap[K, V]
      final type ValueSpec[K, V] = (K, Option[V])
      final def fromSpec[K, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] = ???
      final def toSpec[K, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V]  = ???
    }
  }

  /**
    */
  def thunk[V: Eq: Show, K2: Order: Show, V2: Eq: Show](
      v: WithId.Aux[V],
      p: Param
  )(implicit
      isV: p.ValueSpec[K2, V2] === V
  ) =
    new p.DependentTypeThunk(v) { lazy val IsV: p.ValueSpec[K2, V2] === V = isV }

  import Param._

  /**
    */
  def v[V: Eq: Show, K2: Order: Show, V2: Eq: Show](
      v: WithId.Aux[V]
  )(implicit
      isV: V.ValueSpec[K2, V2] === V
  ) = thunk[V, K2, V2](v, V)

  /**
    */
  def lv[V: Eq: Show, K2: Order: Show, V2: Eq: Show](
      v: WithId.Aux[V]
  )(implicit
      isV: LV.ValueSpec[K2, V2] === V
  ) = thunk[V, K2, V2](v, LV)

  /**
    */
  def mkv[V: Eq: Show, K2: Order: Show, V2: Eq: Show](
      v: WithId.Aux[V]
  )(implicit
      isV: MKV.ValueSpec[K2, V2] === V
  ) = thunk[V, K2, V2](v, MKV)

}
// trait Wut {

//   @SuppressWarnings(Array("org.wartremover.warts.Any"))
//   protected trait VSnel[F[_], X] extends ValueStore[F] {

//     protected implicit def IsV: X === Value

//     type Yuft[x] = EffectType[Option[NonEmptyList[x]]]

//     /**
//       */
//     protected def getNel(id: Id): EffectType[Option[NonEmptyList[X]]] =
//       IsV.flip substitute [Yuft] (for {
//         cache <- lookups(id).compile.toList
//         store <- rows(id).compile.toList
//       } yield cache.headOption.fold(store.toNel)(_ => cache.toNel)) // lawful evil

//     /**
//       */
//     protected def putNel(spec: NonEmptyList[X]): EffectType[(Id, Boolean)] = {
//       val id = nextId(IsV coerce spec.head, IsV substitute spec.tail: _*)
//       for {
//         x <- has(id)
//         ret <- if (x) (id, false).pure[F]
//                else append(IsV coerce spec.head, IsV substitute spec.tail: _*) map ((_, true))
//       } yield ret
//     }
//   }

//   /**
//     */
//   @SuppressWarnings(Array("org.wartremover.warts.Any"))
//   trait ValueStoreNELV[F[_]] extends VSnel[F, Vega] {

//     /**
//       */
//     final type Spec = NonEmptyList[Vega]

//     /**
//       */
//     @inline final def get(id: Id): EffectType[Option[Spec]] =
//       getNel(id)

//     /**
//       */
//     @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
//       putNel(spec)
//   }

//   /**
//     */
//   @SuppressWarnings(Array("org.wartremover.warts.Any"))
//   trait ValueStoreNEMK2V2[F[_]] extends VSnel[F, (Kappa, Vega)] {

//     /**
//       */
//     final type Spec = NonEmptyMap[K2, V2]

//     /**
//       */
//     @inline final def get(id: Id): EffectType[Option[Spec]] =
//       ??? // getNel(id)

//     /**
//       */
//     @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
//       ??? // putNel(spec)
//     //     /**
//     //   */
//     // def putm[K2: Order, V2](
//     //     k2v2s: NonEmptyMap[K2, V2]
//     // )(implicit asV: (K2, V2) <~< V): F[(Id, Boolean)] =
//     //   putl(k2v2s.toNel map (asV coerce _))

//   }

//   /**
//     */
//   @SuppressWarnings(Array("org.wartremover.warts.Any"))
//   trait ValueStoreNEMK2LV2[F[_]] extends VSnel[F, (Kappa, Option[Vega])] {

//     /**
//       */
//     final type Spec = NonEmptyMap[Kappa, List[Vega]]

//     /**
//       */
//     @inline final def get(id: Id): EffectType[Option[Spec]] =
//       ??? // getNel(id)
//     // def getml[K2: Order, V2](
//     //     id: Id
//     // )(implicit
//     //     asK2V2: V <~< (K2, V2)
//     // ): EffectType[Map[K2, List[V2]]] =
//     //   for (values <- getl(id))
//     //     yield values.groupBy(v => (asK2V2 coerce v)._1) map {
//     //       case (k2, vs) => (k2, vs map (v => (asK2V2 coerce v)._2))
//     //     }

//     /**
//       */
//     @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
//       ??? // putNel(spec)
//     //     /**
//     //   */
//     // def putml[K2: Order, V2](
//     //     k2lv2s: NonEmptyMap[K2, List[V2]]
//     // )(implicit asV: (K2, Option[V2]) <~< V): F[(Id, Boolean)] =
//     //   putl(k2lv2s.toNel flatMap {
//     //     case (k2, hv2 :: tv2s) =>
//     //       NonEmptyList(asV coerce k2 -> hv2.some, tv2s map (v2 => asV coerce k2 -> v2.some))
//     //     case (k2, Nil) =>
//     //       NonEmptyList one (asV coerce k2 -> none)
//     //   })

//   }

//   /**
//     */
//   @SuppressWarnings(Array("org.wartremover.warts.Any"))
//   trait VSlist[F[_], X] extends ValueStore[F] {

//     /**
//       */
//     protected implicit def IsV: Option[X] === Value

//     /**
//       */
//     protected def getList(id: Id): EffectType[Option[List[X]]] =
//       for {
//         cache <- lookups(id).compile.toList
//         store <- rows(id).compile.toList
//       } yield (cache.headOption.fold(store)(_ => cache) map IsV.flip.coerce).sequence

//     /** FIXME: abstract the duplicate folding
//       */
//     protected def putList(spec: List[X]): EffectType[(Id, Boolean)] = {
//       val id = spec.headOption.fold(
//         nextId(IsV coerce none[X])
//       )(h => nextId(IsV coerce h.some, spec drop 1 map (IsV coerce _.some): _*))
//       for {
//         x <- has(id)
//         ret <- if (x) (id, false).pure[F]
//                else
//                  spec.headOption.fold(
//                    append(IsV coerce none[X])
//                  )(h => append(IsV coerce h.some, spec drop 1 map (IsV coerce _.some): _*)) map {
//                    (_, true)
//                  }
//       } yield ret
//     }
//   }

//   /**
//     */
//   trait ValueStoreLV[F[_]] extends VSlist[F, Vega] {

//     /**
//       */
//     final type Spec = List[Vega]

//     /**
//       */
//     @inline final def get(id: Id): EffectType[Option[Spec]] =
//       getList(id)

//     /**
//       */
//     @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
//       putList(spec)
//   }

//   /** `Option[(K2, V2)] === V`
//     */
//   trait ValueStoreMK2V2[F[_]] extends VSlist[F, (Kappa, Vega)] {

//     implicit def K2: Order[Kappa]

//     /**
//       */
//     final type Spec = Map[Kappa, Vega]

//     /**
//       */
//     @inline final def get(id: Id): EffectType[Option[Spec]] =
//       getList(id) map {
//         _ map { SortedMap(_: _*) }
//       }

//     /**
//       */
//     @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
//       putList(spec.toList)
//   }

//   /** Option[(K2, Option[V2])] === V
//     */
//   @SuppressWarnings(Array("org.wartremover.warts.Any"))
//   trait ValueStoreMK2LV2[F[_]] extends VSlist[F, (Kappa, Option[Vega])] {

//     /**
//       */
//     final type Spec = Map[K2, List[V2]]

//     /**
//       */
//     @inline final def get(id: Id): EffectType[Option[Spec]] =
//       ??? // getList(id)

//     /**
//       */
//     @inline final def put(spec: Spec): EffectType[(Id, Boolean)] =
//       ??? // putList(spec)
//     // /**
//     //   */
//     // def putml[K2: Order, V2](
//     //     k2lv2s: Map[K2, List[V2]]
//     // )(implicit asV: Option[(K2, Option[V2])] <~< V): F[(Id, Boolean)] =
//     //   putl[(K2, Option[V2])](k2lv2s.toList flatMap {
//     //     case (k2, Nil) => List(k2 -> none)
//     //     case (k2, v2s) => v2s map (v2 => k2 -> v2.some)
//     //   })
//   }

//   /**
//     */
//   @SuppressWarnings(Array("org.wartremover.warts.Any"))
//   trait KeyValueStoreV[F[_]] {

//     /**
//       */
//     final type Repr[A] = Map[K, Option[(Id, A)]]

//     /** What the api client can get a copy of.
//       */
//     type Shape[_]

//     /**
//       */
//     def snapshot: Shape[Spec] => StreamF[Spec] = _ => ???

//     /**
//       */
//     def spinup: StreamF[Spec] => Shape[Spec] = _ => ???

//     /**
//       */
//     protected def recordsWith(key: Key): StreamF[Record] =
//       records filter (_._2._1 === key)

//     /** TODO: revisit the implementation, which is tricky
//       */
//     protected def updateExistingKey(key: Key, maybeValue: Option[Value]): F[Option[Id]] =
//       exists(key) flatMap (_.fold(none[Id].pure[F])(_ => append(key -> maybeValue) map (_.some)))

//     /**
//       */
//     def existsAll(key: Key): StreamF[Id] =
//       recordsWith(key) map (_._1)

//     /**
//       */
//     def exists(key: Key): F[Option[Id]] =
//       existsAll(key).compile.last

//     /** Returns '''all''' un-[[delete]]d `Value`s for the given `Key`. */
//     def gets(key: Key): StreamF[Value] =
//       for {
//         value <- (recordsWith(key) map (_._2._2)).unNoneTerminate
//       } yield value

//     /**
//       */
//     def get(key: Key): F[Option[Value]] =
//       gets(key).compile.last

//     /**
//       */
//     def let(key: Key, value: Value): F[Option[Id]] =
//       exists(key) flatMap (_.fold(append(key -> value.some) map (_.some))(_ => none.pure[F]))

//     /**
//       */
//     def set(key: Key, value: Value): F[Option[Id]] =
//       updateExistingKey(key, value.some)

//     /** Empty `Value` memorializes (persists) `delete` for a given `key` - this row gets an `Id`!
//       */
//     def del(key: Key): F[Option[Id]] =
//       updateExistingKey(key, none)

//     /**
//       */
//     def put(key: Key, value: Value): F[Id] =
//       append(key -> value.some)
//   }

//   /**
//     */
//   @SuppressWarnings(Array("org.wartremover.warts.Any"))
//   trait KeyValueStoreM[F[_], K, V] {

//     self: Store.Aux[F, WithKey.Aux[K, *], V] =>

//     import V._

//     final type Repr[A] = Map[Key, (Id, A)]

//     /** FIXME: dedupe */
//     def gets(key: Key): StreamF[Value] = ???

//     /**
//       */
//     def getl(key: Key): F[List[Value]] =
//       gets(key).compile.toList

//     /**
//       */
//     def getm[K2: Order, V2](
//         key: Key
//     )(implicit asK2V2: Value <~< (K2, V2)): F[Map[K2, V2]] =
//       for (values <- getl(key))
//         yield SortedMap(values map (asK2V2 coerce _): _*)

//     /**
//       */
//     def putl(key: Key, values: NonEmptyList[Value]): F[Id] =
//       append((key -> values.head.some), (values.tail map (key -> _.some)): _*)

//     /**
//       */
//     def putm[K2: Order, V2](
//         key: Key,
//         k2v2s: NonEmptyMap[K2, V2]
//     )(implicit asValue: (K2, V2) <~< Value): F[Id] =
//       putl(key, k2v2s.toNel map (asValue coerce _))
//   }
// }
/**
  */
abstract class KeyValueStore[F[_]: Sync: ContextShift, K, V](
    v: WithKey.Aux[K, V]
) extends Store[F, WithKey.Aux[K, *], V](v)

/**
  */
object KeyValueStore {}
