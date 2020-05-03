package io.deftrade
package keyval

import io.deftrade.syntax._

import cats.implicits._

import cats.{ ~>, Order }
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.arrow.FunctionK
import cats.free.Free
import cats.evidence._

import cats.effect.{ ContextShift, IO, Sync }

import shapeless.{ HList }

import scala.collection.immutable.SortedMap

/** */
sealed abstract class Command[F[_], A]

/** */
object Command

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed abstract class FreeValueStore[Effect[_], Value](val Value: WithId[Value]) {

  protected final type Id = Value.Id

  protected sealed abstract class Cmd[A] extends Command[Effect, A]

  protected case class Get(id: Id)                     extends Cmd[Value]
  protected case class GetList(id: Id)                 extends Cmd[List[Value]]
  protected case class Has(id: Id)                     extends Cmd[Boolean]
  protected case class Put(value: Value)               extends Cmd[Id]
  protected case class PutNel(vs: NonEmptyList[Value]) extends Cmd[Id]

  final def get(id: Id): Free[Command[Effect, *], Value]    = Get(id)    |> Free.liftF
  final def has(id: Id): Free[Command[Effect, *], Boolean]  = Has(id)    |> Free.liftF
  final def put(value: Value): Free[Command[Effect, *], Id] = Put(value) |> Free.liftF

  final def getList(id: Id): Free[Command[Effect, *], List[Value]] =
    GetList(id) |> Free.liftF

  final def putNel(values: NonEmptyList[Value]): Free[Command[Effect, *], Id] =
    PutNel(values) |> Free.liftF

  /** */
  final def getMap[K2: Order, V2](id: Id)(
      implicit asK2V2: Value <~< (K2, V2)
  ): Free[Command[Effect, *], Map[K2, V2]] =
    for (values <- getList(id)) yield (values map (asK2V2 coerce _)).toMap

  /** */
  final def putNem[K2: Order, V2](k2v2s: NonEmptyMap[K2, V2])(
      implicit asValue: (K2, V2) <~< Value
  ): Free[Command[Effect, *], Id] =
    putNel(k2v2s.toNel map (asValue coerce _))

  /** */
  def compilerFor[HV <: HList](
      valueStore: ModuleTypes.Aux[Effect, WithId[*], Value, HV] with ValueStore[Effect, Value, HV]
  ): Cmd ~> valueStore.EffectStream =
    new FunctionK[Cmd, valueStore.EffectStream] {

      /** TODO: consider splitting up read and write */
      def apply[A](command: Cmd[A]): valueStore.EffectStream[A] = command match {
        case Get(id)     => valueStore get id
        case GetList(id) => valueStore getList id
        case Has(id)     => for { b <- valueStore.permRows exists (_._1 /*=*/ == id) } yield b
        case Put(v)      => valueStore append v
        case PutNel(vs)  => valueStore appendAll (vs.head, vs.tail: _*)
      }
    }
}

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
object FreeValueStore {

  /** */
  def apply[F[_]: Sync: ContextShift, V](V: WithId[V]): FreeValueStore[F, V] =
    new FreeValueStore[F, V](V) {}

  /** */
  def withIO[V](V: WithId[V])(implicit csio: ContextShift[IO]): FreeValueStore[IO, V] =
    apply(V)
}

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed abstract class FreeKeyValueStore[Effect[_], Key, Value](val V: WithKey.Aux[Key, Value]) {

  final type Id = V.Id

  protected sealed abstract class Cmd[A] extends Command[Effect, A]

  protected case class Get(key: Key)               extends Cmd[Value]
  protected case class GetList(key: Key)           extends Cmd[List[Value]]
  protected case class Has(key: Key)               extends Cmd[Id]
  protected case class Let(key: Key, value: Value) extends Cmd[Id]
  protected case class Set(key: Key, value: Value) extends Cmd[Id]
  protected case class Put(key: Key, value: Value) extends Cmd[Id]
  protected case class Del(key: Key)               extends Cmd[Id]

  final def get(key: Key): Free[Command[Effect, *], Value]           = Get(key)     |> Free.liftF
  final def getList(key: Key): Free[Command[Effect, *], List[Value]] = GetList(key) |> Free.liftF
  final def has(key: Key): Free[Command[Effect, *], Id]              = Has(key)     |> Free.liftF
  final def del(key: Key): Free[Command[Effect, *], Id]              = Del(key)     |> Free.liftF

  final def let(key: Key, value: Value): Free[Command[Effect, *], Id] =
    Let(key, value) |> Free.liftF

  final def set(key: Key, value: Value): Free[Command[Effect, *], Id] =
    Set(key, value) |> Free.liftF

  final def put(key: Key, value: Value): Free[Command[Effect, *], Id] =
    Put(key, value) |> Free.liftF

  final def putNel(key: Key, values: NonEmptyList[Value]): Free[Command[Effect, *], NonEmptyList[Id]] =
    values
      .map(put(key, _))
      .sequence

  /** */
  final def getMap[K2: Order, V2](key: Key)(
      implicit asK2V2: Value <~< (K2, V2)
  ): Free[Command[Effect, *], Map[K2, V2]] =
    for (values <- getList(key)) yield SortedMap(values map (asK2V2 coerce _): _*)

  /** */
  final def putNem[K2: Order, V2](key: Key, k2v2s: NonEmptyMap[K2, V2])(
      implicit asValue: (K2, V2) <~< Value
  ): Free[Command[Effect, *], NonEmptyList[Id]] =
    putNel(key, k2v2s.toNel map (asValue coerce _))

  /** */
  def compilerFor[HV <: HList](
      kvs: ModuleTypes.Aux[
        Effect,
        WithKey.Aux[Key, *],
        Value,
        HV
      ] with KeyValueStore[Effect, Key, Value, HV]
  ): Cmd ~> kvs.EffectStream =
    new FunctionK[Cmd, kvs.EffectStream] {

      /** TODO: consider splitting up read and write */
      def apply[A](command: Cmd[A]): kvs.EffectStream[A] = command match {
        case Get(k)     => kvs select k
        case GetList(k) => kvs selectList k
        case Has(k)     => for { v <- kvs select k; id <- kvs update (k, v) } yield id
        case Let(k, v)  => kvs insert (k, v)
        case Set(k, v)  => kvs update (k, v)
        case Put(k, v)  => kvs upsert (k, v)
        case Del(k)     => kvs delete k
      }
    }
}

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
object FreeKeyValueStore {

  /** */
  def apply[F[_]: Sync: ContextShift, K, V](
      V: WithKey.Aux[K, V]
  ): FreeKeyValueStore[F, K, V] =
    new FreeKeyValueStore[F, K, V](V) {}

  /** */
  def withIO[K, V](
      V: WithKey.Aux[K, V]
  )(
      implicit
      csio: ContextShift[IO]
  ): FreeKeyValueStore[IO, K, V] =
    apply(V)
}
