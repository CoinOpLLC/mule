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
import cats.{ Contravariant, Eq, Order, Show }
import cats.kernel.CommutativeGroup
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.evidence._
import cats.effect.{ ContextShift, Sync }

import eu.timepit.refined
import refined.api.Refined
import refined.cats.refTypeOrder

import fs2.{ Pipe, Stream }

import scala.collection.immutable.SortedMap
import cats.kernel.Monoid

/** FIXME: `trait` implementation necessitates duplication of the Spec[K2, V2] === V defninitions
  * for all the `Spec`s, in the form of self type restrictions. Eliminate or prove we can't.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class Store[F[_], W[_] <: WithValue, V](
    final val V: W[V]
)(implicit final val F: Sync[F], final val X: ContextShift[F]) {

  import V._

  /**
    */
  final type Record = (Id, Row)

  /** What is returned from a `get` or stored by a `put`.
    */
  type Spec

  /**
    */
  final type StreamF[A] = Stream[F, A]

  /**
    */
  final type PipeF[A, B] = Pipe[F, A, B]

  /**
    */
  def rows: StreamF[Row] =
    records map (_._2)

  /**
    */
  def hasId(id: Id): F[Boolean] =
    (records exists (_._1 === id)).compile.lastOrError

  /** Returns a Stream of all persisted `Row`s prefaces with their `Id`s.
    */
  protected def records: StreamF[Record]

  /** overrideable with default nop
    * empty List entails `del`etion (only applicable to [[KeyValueStore]])
    */
  protected def cacheFill(id: Id, rows: List[Row]): F[Unit] =
    ().pure[F]

  /**
    */
  protected def persist: Record PipeF Unit

  /** FIXME obviously... this works, not obvously, that's the problem
    */
  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  protected var prev: Id =
    Refined unsafeApply [String, IsSha] "7hereWazAPharmrHadADogNBingoWuzHizN4m3oB1NGo"

  /**
    */
  protected def fresh: Fresh[Id, Row]

  /** note this is pure and not effectful! */
  protected def nextId(row: Row, rows: Row*): Id =
    fresh.nextAll(prev, row, rows: _*)

  /** Note this returns a ''single'' `Id` for the whole sequence of `Row`s.
    *
    * This feature - the ability to assign multiple rows a single `Id` computed over those all
    * of those rows - is why ''this'' method is the abstract primitive (and not [[append]]).
    *
    * Appends to the backing store whether or not there is a duplicate (no checking).
    *
    * FIXME: not thread safe: put a `Ref` based queue in front
    */
  final protected def append(row: Row, rows: Row*): F[Id] =
    for {
      id <- F delay nextId(row, rows: _*)
      rs <- F delay (row +: rows).toList
      _  <- cacheFill(id, rs)
      _  <- (Stream evals (F delay { rs map (id -> _) }) through persist).compile.drain
    } yield id

  /** Returns ''all'' `Row`s with the given `Id` (none, if not found) as an [[fs2.Stream]].
    */
  final protected def rows(id: Id): F[List[Row]] =
    (records filter (_._1 === id) map (_._2)).compile.toList
}

/**
  */
object Store {}

/** Note the only way to get an Id is as the result of an effectful mutation
  * or [[KeyValueStore#peek]].
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class ValueStore[F[_]: Sync: ContextShift, V](
    v: WithId.Aux[V]
) extends Store[F, WithId.Aux, V](v) {

  import V._

  /**
    */
  def get(id: Id): F[Option[Spec]]

  /**
    */
  def put(spec: Spec): F[(Id, Boolean)]

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
    def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]]

    /**
      */
    def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V]

    /**
      */
    final def toSpecOption[K: Order, V](vs: List[ValueSpec[K, V]]): Option[Spec[K, V]] =
      vs match {
        case Nil    => none
        case h :: t => toSpec(NonEmptyList(h, t)).some
      }

    /**
      */
    sealed abstract class DependentTypeThunk[V](
        v: WithId.Aux[V]
    ) {

      /**
        */
      def deriveV[V2: Show](
          implicit
          isV: param.ValueSpec[Nothing, V2] === V
      ) = {
        implicit def nothingOrder: Order[Nothing] = ???

        new SubThunk[Nothing, V2] {}
      }

      /**
        */
      def deriveKV[K2: Order: Show, V2: Show](
          implicit
          isV: param.ValueSpec[K2, V2] === V
      ) =
        new SubThunk[K2, V2] {}

      /**
        */
      abstract class SubThunk[K2: Order, V2]()(
          implicit
          IsV: param.ValueSpec[K2, V2] === V
      ) {

        /**
          */
        trait ValueStore[F[_]] {

          self: keyval.ValueStore[F, V] =>

          import V._

          final type Spec      = param.Spec[K2, V2]
          final type ValueSpec = param.ValueSpec[K2, V2]

          /**
            */
          final def get(id: Id): F[Option[Spec]] =
            for {
              row <- cachedRows(id)
            } yield param toSpecOption (IsV.flip substitute row)

          /**
            */
          final def put(spec: Spec): F[(Id, Boolean)] = {
            val NonEmptyList(h, t) = IsV substitute (param fromSpec spec)
            val id                 = nextId(h, t: _*)
            for {
              x <- hasId(id)
              ret <- if (x) (id, false).pure[F]
                    else append(h, t: _*) map ((_, true))
            } yield ret
          }
        }
      }
    }
  }

  /**
    */
  object Param {

    case class Aux[T, U](
        f: T => NonEmptyList[U],
        g: NonEmptyList[U] => T
    ) extends Param {
      final type Spec[K, V]      = T
      final type ValueSpec[K, V] = U

      final def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        f(s)

      final def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        g(vs)
    }

    /** Values
      */
    case object V extends Param {
      final type Spec[K, V]      = V
      final type ValueSpec[K, V] = V

      final def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        NonEmptyList one s

      final def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        vs match {
          case NonEmptyList(v, Nil) => v
          case _                    => ???
        }
    }

    /** Lists of Values
      */
    case object LV extends Param {
      final type Spec[K, V]      = List[V]
      final type ValueSpec[K, V] = Option[V]

      /**
        */
      final def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        s match {
          case Nil    => NonEmptyList one none
          case h :: t => NonEmptyList(h.some, t map (_.some))
        }

      /** FIXME: get rid of hole - the typing is subtle here: only 2 cases
        * - Nel(None)
        * - Nel(x.some, ...)
        *
        * feels like `Stream.unNone`
        */
      final def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        vs match {
          case NonEmptyList(None, Nil) => List.empty
          case nel                     => nel.toList map (_.fold(???)(identity))
        }
    }

    /** Maps of (Key -> Values)s
      */
    case object MKV extends Param {
      final type Spec[K, V]      = Map[K, V]
      final type ValueSpec[K, V] = Option[(K, V)]

      final def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        LV fromSpec [Nothing, (K, V)] s.toList

      final def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        (LV toSpec [Nothing, (K, V)] vs).toMap
    }

    /** Maps of (Key -> List[Value])s
      */
    case object MKLV extends Param {
      final type Spec[K, V]      = Map[K, List[V]]
      final type ValueSpec[K, V] = Option[(K, Option[V])]

      final def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        LV fromSpec [Nothing, (K, List[V])] s.toList flatMap {
          (_: Option[(K, List[V])]) match {
            case None           => NonEmptyList one none
            case Some((k, Nil)) => NonEmptyList one (k -> none).some
            case Some((k, h :: t)) =>
              NonEmptyList(
                (k -> h.some).some,
                t map (v => (k -> v.some).some)
              )
          }
        }

      /**
        */
      final def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        LV toSpec [Nothing, (K, Option[V])] vs groupBy (_._1) map {
          case (k, Nil) => k -> Nil
          case (k, kvos) =>
            k -> kvos.map {
              case (_, Some(v)) => v
              case _            => ???
            }
        }
    }

    /** Non Empty Lists of Values
      */
    case object NELV extends Param {
      final type Spec[K, V]      = NonEmptyList[V]
      final type ValueSpec[K, V] = V

      final def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        s

      final def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        vs
    }

    /** Non Empty Maps of (Key -> Values)s
      */
    case object NEMKV extends Param {
      final type Spec[K, V]      = NonEmptyMap[K, V]
      final type ValueSpec[K, V] = (K, V)

      final def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        NELV fromSpec [Nothing, (K, V)] s.toNel

      final def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        NELV toSpec [Nothing, (K, V)] vs match {
          case NonEmptyList(h, t) => NonEmptyMap(h, SortedMap(t: _*))
        }
    }

    /** Non Empty Maps of (Key -> List[Value])s
      */
    case object NEMKLV extends Param {
      final type Spec[K, V]      = NonEmptyMap[K, List[V]]
      final type ValueSpec[K, V] = (K, Option[V])

      final def fromSpec[K: Order, V](s: Spec[K, V]): NonEmptyList[ValueSpec[K, V]] =
        NELV fromSpec [Nothing, (K, List[V])] s.toNel flatMap {
          (_: (K, List[V])) match {
            case (k, Nil)    => NonEmptyList one (k -> none)
            case (k, h :: t) => NonEmptyList((k -> h.some), t map (v => (k -> v.some)))
          }
        }

      final def toSpec[K: Order, V](vs: NonEmptyList[ValueSpec[K, V]]): Spec[K, V] =
        NELV toSpec [Nothing, (K, Option[V])] vs match {
          case NonEmptyList((k, None), Nil)    => NonEmptyMap(k -> Nil, SortedMap.empty)
          case NonEmptyList((k, Some(v)), Nil) => NonEmptyMap(k -> List(v), SortedMap.empty)
          case _                               => ???
        }

    }
  }

  /**
    */
  def apply[V: Show](v: WithId.Aux[V], p: Param) =
    new p.DependentTypeThunk(v) {}

  import Param._
  implicit def nothingOrder: Order[Nothing] = ???
  implicit def nothingShow: Show[Nothing]   = ???

  /**
    */
  def v[V, V2: Show](
      v: WithId.Aux[V]
  )(implicit isV: V.ValueSpec[Nothing, V2] === V) = {
    implicit def showV = Contravariant[Show].contramap(Show[V2])(isV.flip coerce _)
    ValueStore(v, V).deriveKV[Nothing, V2]
  }

  /**
    */
  def lv[V, V2: Show](
      v: WithId.Aux[V]
  )(implicit isV: LV.ValueSpec[Nothing, V2] === V) = {
    implicit def showV: Show[V] =
      Contravariant[Show].contramap(Show[LV.ValueSpec[Nothing, V2]])(isV.flip coerce _)
    ValueStore(v, LV).deriveKV[Nothing, V2]
  }

  /**
    */
  def mkv[V, K2: Order: Show, V2: Show](
      v: WithId.Aux[V]
  )(implicit isV: MKV.ValueSpec[K2, V2] === V) = {
    implicit def showV = Contravariant[Show].contramap(Show[Option[(K2, V2)]])(isV.flip coerce _)
    ValueStore(v, MKV).deriveKV[K2, V2]
  }

  /**
    */
  def mklv[V, K2: Order: Show, V2: Show](
      v: WithId.Aux[V]
  )(implicit isV: MKLV.ValueSpec[K2, V2] === V) = {
    implicit def showV: Show[V] =
      Contravariant[Show].contramap(Show[MKLV.ValueSpec[K2, V2]])(isV.flip coerce _)
    ValueStore(v, MKLV).deriveKV[K2, V2]
  }

  /**
    */
  def nelv[V, V2: Show](
      v: WithId.Aux[V]
  )(implicit isV: NELV.ValueSpec[Nothing, V2] === V) = {
    implicit def showV: Show[V] =
      Contravariant[Show].contramap(Show[NELV.ValueSpec[Nothing, V2]])(isV.flip coerce _)
    ValueStore(v, NELV).deriveKV[Nothing, V2]
  }

  /**
    */
  def nemkv[V, K2: Order: Show, V2: Show](
      v: WithId.Aux[V]
  )(implicit isV: NEMKV.ValueSpec[K2, V2] === V) = {
    implicit def showV: Show[V] =
      Contravariant[Show].contramap(Show[NEMKV.ValueSpec[K2, V2]])(isV.flip coerce _)
    ValueStore(v, NEMKV).deriveKV[K2, V2]
  }

  /**
    */
  def nemklv[V, K2: Order: Show, V2: Show](
      v: WithId.Aux[V]
  )(implicit isV: NEMKLV.ValueSpec[K2, V2] === V) = {
    implicit def showV: Show[V] =
      Contravariant[Show].contramap(Show[NEMKLV.ValueSpec[K2, V2]])(isV.flip coerce _)
    ValueStore(v, NEMKLV).deriveKV[K2, V2]
  }
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
abstract class KeyValueStore[F[_]: Sync: ContextShift, K, V](
    v: WithKey.Aux[K, V]
) extends Store[F, WithKey.Aux[K, *], V](v) {

  import V._

  import Key._

  /**
    */
  type NelSpec

  /**
    */
  def peek(id: Id): F[Option[(Key, Spec)]]

  /**
    */
  final def probe(key: Key): F[Option[Id]] =
    probeAll(key) map (_.headOption)

  /** Nota Bene - Uncached.
    */
  final def probeAll(key: Key): F[List[Id]] =
    storeLookup(key) map { _ map { case (id, _) => id } }

  /** Returns '''all''' un-[[delete]]d `Value`s for the given `Key`.
    */
  def getAll(key: Key): F[Option[NelSpec]]

  /**
    */
  def get(key: Key): F[Option[Spec]]

  /** Could also call this method `subscribe`, where [[put]] would be the corresponding `publish`.
    */
  def out(key: Key): StreamF[Option[Spec]]

  /**
    */
  def let(key: Key, spec: Spec): F[Option[Id]]

  /**
    */
  def put(key: Key, spec: Spec): F[Id]

  /**
    */
  def set(key: Key, spec: Spec): F[Option[Id]]

  /** Empty `Value` memorializes (persists) `delete` for a given `key` - this row gets an `Id`!
    */
  def del(key: Key): F[Option[Id]]

  /** Default behavior of "always miss" is overrideable.
    */
  protected def cacheLookup(key: Key): F[Option[(Id, List[Value])]] = // F[Option[Spec]] =
    none.pure[F]

  /**
    */
  final protected def storeLookup(key: Key): F[List[(Id, List[Value])]] =
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

/**
  */
object KeyValueStore {

  /**
    */
  sealed trait Param { param =>

    type Spec[K2, V2]
    type ValueSpec[K2, V2]

    final type NelSpec[K2, V2] = param.Spec[K2, NonEmptyList[V2]]

    protected implicit def nelSpecMonoid[K2, V2]: Monoid[NelSpec[K2, V2]]

    /** `NonEmptyList[Option[ValueSpec[K, V]]]` is an idiom for a natural number of csv rows
      */
    def fromSpec[K2: Order, V2](
        os: Option[Spec[K2, V2]]
    ): NonEmptyList[Option[ValueSpec[K2, V2]]]

    /**
      */
    def toSpec[K2: Order, V2](
        vs: NonEmptyList[ValueSpec[K2, V2]]
    ): Spec[K2, V2]

    /**
      */
    def toNelSpec[K2: Order, V2](spec: Spec[K2, V2]): NelSpec[K2, V2]

    /** Whether we return a `Some`thing or a `Nothing` depends on the `Spec`.
      */
    def empty[K2: Order, V2]: Option[Spec[K2, V2]]

    /**
      */
    sealed abstract class DependentTypeThunk[K, V](
        v: WithKey.Aux[K, V]
    ) { thunk =>

      /**
        */
      def deriveV[V2: Show](implicit isV: param.ValueSpec[Nothing, V2] === V) = {
        implicit def nothingOrder: Order[Nothing] = ???

        new SubThunk[Nothing, V2] {}
      }

      /**
        */
      def deriveKV[K2: Order: Show, V2: Show](implicit isV: param.ValueSpec[K2, V2] === V) =
        new SubThunk[K2, V2] {}

      /** Dervives `K2` and `V2`.
        */
      abstract class SubThunk[K2: Order, V2]()(
          implicit
          IsV: param.ValueSpec[K2, V2] === V
      ) { subThunk =>

        /**
          */
        @SuppressWarnings(Array("org.wartremover.warts.Any"))
        trait KeyValueStore[F[_]] { self: keyval.KeyValueStore[F, K, V] =>

          import V._

          final type Spec      = param.Spec[K2, V2]
          final type ValueSpec = param.ValueSpec[K2, V2]
          final type NelSpec   = param.NelSpec[K2, V2]

          /** Uncached.
            *
            * Note: `peek` cannot be used to distinguish whether an `Id` was never written,
            * or has been written and subsequently [[delete]]d. Both cases return `F[None]`.
            */
          final def peek(id: Id): F[Option[(Key, Spec)]] =
            rows(id) map {
              case Nil => none
              case ((h @ (k, _)) :: t) =>
                (k -> (param toSpec (NonEmptyList(h, t) map {
                  case (_, Some(v)) => IsV.flip coerce v
                  case _            => ??? // trusting the write path
                }))).some
            }

          /** Returns '''all''' un-[[delete]]d `Value`s for the given `Key`.
            *
            * Uncached.
            */
          final def getAll(key: Key): F[Option[NelSpec]] =
            for {
              ivs <- storeLookup(key)
            } yield
              ivs match {
                case Nil => none
                case (_, Nil) :: Nil =>
                  param.empty[K2, V2].fold(none[NelSpec])(spec => (param toNelSpec spec).some)
                case h :: t =>
                  val specs = NonEmptyList(h, t) map {
                    _._2 match {
                      case Nil    => ??? // won't reach here: see List[V] === Nil case above
                      case h :: t => param toSpec (IsV.flip substitute NonEmptyList(h, t))
                    }
                  }
                  specs.map(param toNelSpec _).fold.some
              }

          /** Cached.
            */
          final def get(key: Key): F[Option[Spec]] = {
            val mkSpec: List[V] => Option[Spec] = {
              case Nil    => param.empty[K2, V2]
              case h :: t => param.toSpec(IsV.flip substitute NonEmptyList(h, t)).some
            }
            for {
              hit  <- cacheLookup(key)
              miss <- storeLookup(key)
              last = if (hit.isEmpty) miss.headOption else none
              _ <- last match {
                    case Some((id, vs @ (_ :: _))) => cacheFill(id, vs map (key -> _.some))
                    case _                         => ().pure[F]
                  }
            } yield
              hit
                .map(_._2)
                .fold {
                  miss.map(_._2).reduceLeftOption((r, _) => r).fold(param.empty[K2, V2])(mkSpec)
                }(mkSpec)
          }

          /**
            */
          final def out(key: Key): StreamF[Spec] = ???

          /** Uncached.
            *
            * TODO: revisit this. Could do cheap in-memory set inclusion for `key`s.
            */
          final def let(key: Key, spec: Spec): F[Option[Id]] =
            probe(key)
              .flatMap {
                case None    => put(key, spec) map (_.some)
                case Some(_) => none.pure[F]
              }

          /** Fast. No uncached reads in the path.
            */
          final def put(key: Key, spec: Spec): F[Id] = {
            val NonEmptyList(h, t) = IsV.lift substitute (param fromSpec spec.some)
            append(key -> h, (t map (key -> _)): _*)
          }

          /** Uncached. TODO: revisit this.
            */
          final def set(key: Key, spec: Spec): F[Option[Id]] =
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
          final def del(key: Key): F[Option[Id]] =
            probe(key)
              .flatMap {
                case None    => none.pure[F]
                case Some(_) => append(key -> none) map (_.some)
              }
        }
      }
    }
  }

  /** What kind of values are we storing in our [[KeyValueStore]]?
    */
  object Param {

    /**
      */
    def apply[K, V: Show](v: WithKey.Aux[K, V], p: Param) =
      new p.DependentTypeThunk(v) {}

    /** (Single, scalar) `Value`
      */
    case object V extends Param {
      final type Spec[K2, V2]      = V2
      final type ValueSpec[K2, V2] = V2

      final def toSpec[K2: Order, V2](
          vs: NonEmptyList[ValueSpec[K2, V2]]
      ): Spec[K2, V2] =
        vs match {
          case NonEmptyList(v2, Nil) => v2
          case _                     => ??? // trusting the write path
        }

      final def fromSpec[K2: Order, V2](
          os: Option[Spec[K2, V2]]
      ): NonEmptyList[Option[ValueSpec[K2, V2]]] =
        NonEmptyList(os, Nil)

      final def toNelSpec[K2: Order, V2](spec: Spec[K2, V2]): NelSpec[K2, V2] =
        NonEmptyList one spec

      final def empty[K2: Order, V2]: Option[Spec[K2, V2]] =
        none

      final implicit protected def nelSpecMonoid[K2, V2]: Monoid[NelSpec[K2, V2]] =
        Monoid[NonEmptyList[V2]]
    }

    /** Map of `K2`s and `V2`s
      */
    case object MKV extends Param {

      final type Spec[K2, V2]      = Map[K2, V2]
      final type ValueSpec[K2, V2] = (K2, V2)

      final def toSpec[K2: Order, V2](
          vs: NonEmptyList[ValueSpec[K2, V2]]
      ): Spec[K2, V2] =
        SortedMap(vs.toList: _*)

      final def fromSpec[K2: Order, V2](
          os: Option[Spec[K2, V2]]
      ): NonEmptyList[Option[ValueSpec[K2, V2]]] = {
        def delNel = NonEmptyList(none[(K2, V2)], Nil) // idiom for `delete` command memo
        os.fold(delNel) {
          _.toList match {
            case Nil       => delNel
            case kv :: kvs => NonEmptyList(kv.some, kvs map (_.some))
          }
        }
      }

      final def toNelSpec[K2: Order, V2](spec: Spec[K2, V2]): NelSpec[K2, V2] =
        spec map { case (k, v) => k -> (NonEmptyList one v) }

      final def empty[K2: Order, V2]: Option[Spec[K2, V2]] =
        SortedMap.empty[K2, V2].some

      final implicit protected def nelSpecMonoid[K2, V2]: Monoid[NelSpec[K2, V2]] =
        Monoid[Map[K2, NonEmptyList[V2]]]
    }
  }

  /**
    */
  def apply[K: Order: Show, V: Show](v: WithKey.Aux[K, V], p: Param) =
    new p.DependentTypeThunk(v) {}

  import Param._

  implicit def nothingOrder: Order[Nothing] = ??? // type combinator shim, never called
  implicit def nothingShow: Show[Nothing]   = ??? // type combinator shim, never called

  /**
    */
  def v[K: Order: Show, V, V2: Show](
      v: WithKey.Aux[K, V]
  )(implicit isV: V.ValueSpec[Nothing, V2] === V) = {
    implicit def showV = Contravariant[Show].contramap(Show[V2])(isV.flip coerce _)
    KeyValueStore(v, V).deriveKV[Nothing, V2]
  }

  /**
    */
  def mkv[K: Order: Show, V, K2: Order: Show, V2: Show](
      v: WithKey.Aux[K, V]
  )(implicit isV: MKV.ValueSpec[K2, V2] === V) = {
    implicit def showV = Contravariant[Show].contramap(Show[(K2, V2)])(isV.flip coerce _)
    KeyValueStore(v, MKV).deriveKV[K2, V2]
  }
}
