package io.deftrade
package keyval
package wip

import io.deftrade.syntax._

import cats.implicits._

import cats.{ ~>, Order }
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.arrow.FunctionK
import cats.free.Free
import cats.evidence._

import cats.effect.{ ContextShift, IO, Sync }

import scala.collection.immutable.SortedMap

/**
  */
protected sealed abstract class Command[F[_], A]

/**
  */
protected object Command

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
protected sealed abstract class FreeValueStore[F[_], Value](val Value: WithId.Aux[Value]) {

  protected final type Id = Value.Id

  protected sealed abstract class Cmd[A] extends Command[F, A]

  protected case class Get(id: Id)                     extends Cmd[Value]
  protected case class GetList(id: Id)                 extends Cmd[List[Value]]
  protected case class Has(id: Id)                     extends Cmd[Boolean]
  protected case class Put(value: Value)               extends Cmd[Id]
  protected case class PutNel(vs: NonEmptyList[Value]) extends Cmd[Id]

  final def get(id: Id): Free[Command[F, *], Value]    = Get(id)    |> Free.liftF
  final def has(id: Id): Free[Command[F, *], Boolean]  = Has(id)    |> Free.liftF
  final def put(value: Value): Free[Command[F, *], Id] = Put(value) |> Free.liftF

  /**
    */
  final def getList(id: Id): Free[Command[F, *], List[Value]] =
    GetList(id) |> Free.liftF

  /**
    */
  final def getMap[K2: Order, V2](id: Id)(implicit asK2V2: Value <~< (K2, V2)): Free[Command[F, *], Map[K2, V2]] =
    for (values <- getList(id)) yield (values map (asK2V2 coerce _)).toMap

  /**
    */
  final def putNel(values: NonEmptyList[Value]): Free[Command[F, *], Id] =
    PutNel(values) |> Free.liftF

  /**
    */
  final def putNem[K2: Order, V2](k2v2s: NonEmptyMap[K2, V2])(implicit asValue: (K2, V2) <~< Value): Free[Command[F, *], Id] =
    putNel(k2v2s.toNel map (asValue coerce _))

  /**
    */
  def compilerFor(
      valueStore: StoreTypes.Aux[F, WithId.Aux, Value] with ValueStore[F, Value]
  ): Cmd ~> valueStore.StreamF =
    new FunctionK[Cmd, valueStore.StreamF] {

      /** TODO: consider splitting up read and write */
      def apply[A](command: Cmd[A]): valueStore.StreamF[A] =
        command match {
          case Get(id)     => valueStore get id
          case GetList(id) => valueStore getList id
          case Has(id)     => valueStore has id
          case Put(v)      => valueStore put v
          case PutNel(vs)  => valueStore putNel NonEmptyList(vs.head, vs.tail)
        }
    }
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
protected object FreeValueStore {

  /**
    */
  def apply[F[_]: Sync: ContextShift, V](V: WithId.Aux[V]): FreeValueStore[F, V] =
    new FreeValueStore[F, V](V) {}

  /**
    */
  def withIO[V](V: WithId.Aux[V])(implicit csio: ContextShift[IO]): FreeValueStore[IO, V] =
    apply(V)
}

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
protected sealed abstract class FreeKeyValueStore[F[_], Key, Value](val V: WithKey.Aux[Key, Value]) {

  final type Id = V.Id

  protected sealed abstract class Cmd[A] extends Command[F, A]

  protected case class Get(key: Key)               extends Cmd[Value]
  protected case class GetList(key: Key)           extends Cmd[List[Value]]
  protected case class Has(key: Key)               extends Cmd[Id]
  protected case class Let(key: Key, value: Value) extends Cmd[Id]
  protected case class Set(key: Key, value: Value) extends Cmd[Id]
  protected case class Put(key: Key, value: Value) extends Cmd[Id]
  protected case class Del(key: Key)               extends Cmd[Id]

  final def get(key: Key): Free[Command[F, *], Value]           = Get(key)     |> Free.liftF
  final def getList(key: Key): Free[Command[F, *], List[Value]] = GetList(key) |> Free.liftF
  final def has(key: Key): Free[Command[F, *], Id]              = Has(key)     |> Free.liftF
  final def del(key: Key): Free[Command[F, *], Id]              = Del(key)     |> Free.liftF

  final def let(key: Key, value: Value): Free[Command[F, *], Id] =
    Let(key, value) |> Free.liftF

  final def set(key: Key, value: Value): Free[Command[F, *], Id] =
    Set(key, value) |> Free.liftF

  final def put(key: Key, value: Value): Free[Command[F, *], Id] =
    Put(key, value) |> Free.liftF

  final def putNel(key: Key, values: NonEmptyList[Value]): Free[Command[F, *], NonEmptyList[Id]] =
    values
      .map(put(key, _))
      .sequence

  /**
    */
  final def getMap[K2: Order, V2](key: Key)(implicit asK2V2: Value <~< (K2, V2)): Free[Command[F, *], Map[K2, V2]] =
    for (values <- getList(key)) yield SortedMap(values map (asK2V2 coerce _): _*)

  /**
    */
  final def putNem[K2: Order, V2](key: Key, k2v2s: NonEmptyMap[K2, V2])(
      implicit
      asValue: (K2, V2) <~< Value
  ): Free[Command[F, *], NonEmptyList[Id]] =
    putNel(key, k2v2s.toNel map (asValue coerce _))

  /**
    */
  def compilerFor(
      kvs: StoreTypes.Aux[
        F,
        WithKey.Aux[Key, *],
        Value
      ] with KeyValueStore[F, Key, Value]
  ): Cmd ~> kvs.StreamF =
    new FunctionK[Cmd, kvs.StreamF] {

      /** TODO: consider splitting up read and write */
      def apply[A](command: Cmd[A]): kvs.StreamF[A] =
        command match {
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

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
protected object FreeKeyValueStore {

  /**
    */
  def apply[F[_]: Sync: ContextShift, K, V](
      V: WithKey.Aux[K, V]
  ): FreeKeyValueStore[F, K, V] =
    new FreeKeyValueStore[F, K, V](V) {}

  /**
    */
  def withIO[K, V](
      V: WithKey.Aux[K, V]
  )(implicit csio: ContextShift[IO]): FreeKeyValueStore[IO, K, V] =
    apply(V)
}
