package io.deftrade
package keyval

import io.deftrade.implicits._

import cats.implicits._

import cats.{ ~>, Order }
import cats.arrow.FunctionK
import cats.free.Free
import cats.evidence._

import cats.effect.{ ContextShift, IO, Sync }

import shapeless.{ HList }

import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global

/** */
sealed abstract class Command[F[_], A]

/** placeholder */
object Command

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed abstract class FreeValueStore {

  type EffectType[_]

  type Id

  type Value

  /** */
  final type FreeCommand[A] = Free[Command[EffectType, *], A]

  case class Get(id: Id)       extends Command[EffectType, Value]
  case class Has(id: Id)       extends Command[EffectType, Boolean]
  case class Put(value: Value) extends Command[EffectType, Id]

  /**   */
  case class Gets(id: Id)              extends Command[EffectType, List[Value]]
  case class Puts(values: List[Value]) extends Command[EffectType, Id]

  final def get(id: Id): FreeCommand[Value]    = Get(id)    |> Free.liftF
  final def has(id: Id): FreeCommand[Boolean]  = Has(id)    |> Free.liftF
  final def put(value: Value): FreeCommand[Id] = Put(value) |> Free.liftF

  final def gets(id: Id): FreeCommand[List[Value]]     = Gets(id)     |> Free.liftF
  final def puts(values: List[Value]): FreeCommand[Id] = Puts(values) |> Free.liftF
}

/** */
object FreeValueStore {

  /** FIXME: this ain't right */
  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  /** */
  final class Aux[F[_], V, I] extends FreeValueStore {
    final type EffectType[A] = F[A]
    final type Value         = V
    final type Id            = I
  }

  /** */
  def apply[F[_]: Sync: ContextShift, V](
      V: WithId[V]
  ): FreeValueStore.Aux[F, V, V.Id] =
    new FreeValueStore.Aux

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def withIO[V](
      V: WithId[V]
  ): FreeValueStore.Aux[IO, V, V.Id] =
    apply(V)

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def compiler[F[_]: Sync: ContextShift, V, HV <: HList](
      vs: ModuleTypes.Aux[F, WithId[*], V, HV] with ValueStore[F, V, HV]
  ): Command[F, *] ~> vs.EffectStream =
    new FunctionK[Command[F, *], vs.EffectStream] {

      val fvs = FreeValueStore(vs.V)
      import fvs._

      /** TODO: consider splitting up read and write */
      def apply[A](command: Command[F, A]): vs.EffectStream[A] = command match {
        case Get(id) => vs get id
        case Has(id) => for { found <- vs.permRows exists (_._1 /*=*/ == id) } yield found
        case Put(v)  => vs append (v)
        case wut     => wut |> discardValue; ??? // FIXME: why not exhaustive match?
      }
    }
}

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
sealed abstract class FreeKeyValueStore {

  type EffectType[_]

  type Id

  type Key

  type Value

  /** */
  final type FreeCommand[A] = Free[Command[EffectType, *], A]

  case class Get(key: Key)               extends Command[EffectType, Value]
  case class Has(key: Key)               extends Command[EffectType, Id]
  case class Let(key: Key, value: Value) extends Command[EffectType, Id]
  case class Set(key: Key, value: Value) extends Command[EffectType, Id]
  case class Put(key: Key, value: Value) extends Command[EffectType, Id]
  case class Del(key: Key)               extends Command[EffectType, Id]

  /** This should be all we need. */
  case class Gets(key: Key) extends Command[EffectType, List[Value]]

  final def get(key: Key): FreeCommand[Value]            = Get(key)        |> Free.liftF
  final def has(key: Key): FreeCommand[Id]               = Has(key)        |> Free.liftF
  final def let(key: Key, value: Value): FreeCommand[Id] = Let(key, value) |> Free.liftF
  final def set(key: Key, value: Value): FreeCommand[Id] = Set(key, value) |> Free.liftF
  final def put(key: Key, value: Value): FreeCommand[Id] = Put(key, value) |> Free.liftF
  final def del(key: Key): FreeCommand[Id]               = Del(key)        |> Free.liftF

  /** */
  final def gets(key: Key): FreeCommand[List[Value]] = Gets(key) |> Free.liftF

  /** */
  final def puts(key: Key, values: List[Value]): FreeCommand[List[Id]] =
    values
      .map(put(key, _))
      .sequence

  /** */
  final def getMap[K2: Order, V2](key: Key)(
      implicit asK2V2: Value <~< (K2, V2)
  ): FreeCommand[Map[K2, V2]] =
    for (values <- gets(key)) yield SortedMap(values map (asK2V2 coerce _): _*)

  /** */
  final def putMap[K2: Order, V2](key: Key, k2v2s: Map[K2, V2])(
      implicit asValue: (K2, V2) <~< Value
  ): FreeCommand[List[Id]] =
    puts(key, k2v2s.toList map (asValue coerce _))
}

/** */
object FreeKeyValueStore {

  /** FIXME: this ain't right */
  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  /** */
  final class Aux[F[_], K, V, I] extends FreeKeyValueStore {
    final override type EffectType[A] = F[A]
    final override type Key           = K
    final override type Value         = V
    final override type Id            = I
  }

  /** */
  def apply[F[_]: Sync: ContextShift, K, V](
      V: WithKey.Aux[K, V]
  ): FreeKeyValueStore.Aux[F, K, V, V.Id] =
    new FreeKeyValueStore.Aux

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def withIO[K, V](
      V: WithKey.Aux[K, V]
  ): FreeKeyValueStore.Aux[IO, K, V, V.Id] =
    apply(V)

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def kvsCompiler[F[_]: Sync: ContextShift, K, V, HV <: HList](
      kvs: ModuleTypes.Aux[F, WithKey.Aux[K, *], V, HV] with KeyValueStore[F, K, V, HV]
  ): Command[F, *] ~> kvs.EffectStream =
    new FunctionK[Command[F, *], kvs.EffectStream] {

      val fkvs = FreeKeyValueStore(kvs.V)
      import fkvs._

      /** TODO: consider splitting up read and write */
      def apply[A](command: Command[F, A]): kvs.EffectStream[A] = command match {
        case Get(k)    => kvs select k
        case Has(k)    => for { v <- kvs select k; id <- kvs update (k, v) } yield id
        case Let(k, v) => kvs insert (k, v)
        case Set(k, v) => kvs update (k, v)
        case Put(k, v) => kvs upsert (k, v)
        case Del(k)    => kvs delete k
        case wut       => wut |> discardValue; ??? // FIXME: why not exhaustive match?
      }
    }
}
