package io.deftrade
package keyval
package layers

import io.deftrade.implicits._

import cats.{ ~> }
import cats.arrow.FunctionK
import cats.free.Free
import cats.free.Free.liftF

import cats.effect.{ ContextShift, IO, Sync }

import shapeless.{ HList }

import scala.concurrent.ExecutionContext.Implicits.global

/** */
trait freestore {

  /** */
  sealed trait Command[F[_], A]

  /** */
  object Command

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  trait FreeKeyValueStore {

    type EffectType[_]

    type Id

    type Key

    type Value

    /** */
    final type FreeCommand[A] = Free[Command[EffectType, *], A]

    case class Get(key: Key)               extends Command[EffectType, Value]
    case class Let(key: Key, value: Value) extends Command[EffectType, Id]
    case class Set(key: Key, value: Value) extends Command[EffectType, Id]
    case class Put(key: Key, value: Value) extends Command[EffectType, Id]
    case class Del(key: Key)               extends Command[EffectType, Id] // (sic)

    final def get(key: Key): FreeCommand[Value]            = Get(key)        |> liftF
    final def let(key: Key, value: Value): FreeCommand[Id] = Let(key, value) |> liftF
    final def set(key: Key, value: Value): FreeCommand[Id] = Set(key, value) |> liftF
    final def put(key: Key, value: Value): FreeCommand[Id] = Put(key, value) |> liftF
    final def del(key: Key): FreeCommand[Id]               = Del(key)        |> liftF
  }

  /** */
  object FreeKeyValueStore {

    implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

    trait Aux[F[_], K, V, I] extends FreeKeyValueStore {
      final override type EffectType[A] = F[A]
      final override type Key           = K
      final override type Value         = V
      final override type Id            = I
    }

    /** */
    def apply[F[_]: Sync: ContextShift, K, V](
        V: WithKey.Aux[K, V]
    ): FreeKeyValueStore.Aux[F, K, V, V.Id] =
      new FreeKeyValueStore.Aux[F, K, V, V.Id] {}

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def withIO[K, V](
        V: WithKey.Aux[K, V]
    ): FreeKeyValueStore.Aux[IO, K, V, V.Id] =
      apply(V)

    /** TODO: needs work */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def kvsCompiler[F[_]: Sync: ContextShift, K, V, HV <: HList](
        kvs: ModuleTypes.Aux[F, WithKey.Aux[K, *], V, HV] with KeyValueStore[F, K, V, HV]
    ): Command[F, *] ~> kvs.EffectStream =
      new FunctionK[Command[F, *], kvs.EffectStream] {

        val fkvs = FreeKeyValueStore(kvs.V)
        import fkvs._

        /** TODO: consider splitting up read and write */
        def apply[A](ca: Command[F, A]): kvs.EffectStream[A] = ca match {
          case Get(k)    => kvs select k
          case Let(k, v) => kvs insert (k, v)
          case Set(k, v) => kvs update (k, v)
          case Put(k, v) => kvs upsert (k, v)
          case Del(k)    => kvs delete k
          case _         => ??? // FIXME: why not exhaustive match?
        }
      }
  }
}
