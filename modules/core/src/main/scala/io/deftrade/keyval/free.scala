package io.deftrade
package keyval

import io.deftrade.implicits._

import cats.{ ~> }
import cats.free.Free
import cats.free.Free.liftF

import cats.effect.{ ContextShift, IO, Sync }

import shapeless.{ HList, LabelledGeneric }

import scala.concurrent.ExecutionContext.Implicits.global

import fs2.Stream

import scala.language.higherKinds

/** */
trait freestore {

  /** */
  sealed trait Command[F[_], A]

  /** */
  object Command

  /** */
  trait FreeKeyValueStore[
      F[_],
      K,
      V,
      HV <: HList
  ] { self: ModuleTypes.Aux[F, ({ type W[v] = WithKey.Aux[K, v] })#W, V, HV] =>

    import V._

    /** */
    final type EffectCommand[A] = Command[EffectType, A]

    /** */
    type FreeCommand[A] = Free[EffectCommand, A]

    /** */
    def impl: EffectCommand ~> EffectStream

    case class Get(key: Key)               extends EffectCommand[Value]
    case class Let(key: Key, value: Value) extends EffectCommand[Id]
    case class Set(key: Key, value: Value) extends EffectCommand[Id]
    case class Put(key: Key, value: Value) extends EffectCommand[Id]
    case class Del(key: Key)               extends EffectCommand[Id] // (sic)

    /** */
    def get(key: Key): FreeCommand[Value] = Get(key) |> liftF

    /** */
    def let(key: Key, value: Value): FreeCommand[Id] = Let(key, value) |> liftF

    /** */
    def set(key: Key, value: Value): FreeCommand[Id] = Set(key, value) |> liftF

    /** */
    def put(key: Key, value: Value): FreeCommand[Id] = Put(key, value) |> liftF

    /** */
    def del(key: Key): FreeCommand[Id] = Del(key) |> liftF
  }

  /** */
  object FreeKeyValueStore {

    /** */
    def apply[
        F[_]: Sync: ContextShift,
        K,
        V,
        HV <: HList
    ](
        V: WithKey.Aux[K, V],
        impl: Command[F, *] ~> Stream[F, *]
    )(
        implicit
        lgv: LabelledGeneric.Aux[V, HV]
    ): FreeKeyValueStore[F, K, V, HV] = new FKVS(V, impl) {}

    /** */
    def withIO[K, V, HV <: HList](
        V: WithKey.Aux[K, V],
        impl: Command[IO, *] ~> Stream[IO, *]
    )(
        implicit lgv: LabelledGeneric.Aux[V, HV]
    ): FreeKeyValueStore[IO, K, V, HV] = apply[IO, K, V, HV](V, impl)

    /** */
    def compiler[
        F[_]: Sync: ContextShift,
        K,
        V,
        HV <: HList
    ](
        V: WithKey.Aux[K, V]
    )(
        kvs: ModuleTypes.Aux[F, WithKey.Aux[K, *], V, HV] with KeyValueStore[F, K, V, HV]
    )(
        implicit fkvs: FreeKeyValueStore[F, K, V, HV]
    ): fkvs.EffectCommand ~> kvs.EffectStream =
      new ~>[fkvs.EffectCommand, kvs.EffectStream] {
        import fkvs._, kvs._

        def apply[A](ca: EffectCommand[A]): EffectStream[A] = ca match {
          case Get(k) => kvs select k
          // case Let(k, v) => kvs insert (k, v)
          // case Set(k, v) => kvs update (k, v)
          // case Put(k, v) => kvs upsert (k, v)
          // case Del(k)    => kvs delete k
          case _ => ???
        }
      }

    /** */
    implicit def contextShiftIO: ContextShift[IO] = IO contextShift global
  }

  /** */
  object FKVS

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private sealed abstract class FKVS[
      F[_]: Sync: ContextShift,
      K,
      V,
      HV <: HList
  ] private[freestore] (
      final override val V: WithKey.Aux[K, V],
      // final override val impl: ({ type C[t] = Command[F, t] })#C ~> ({ type S[r] = Stream[F, r] })#S
      final override val impl: Command[F, *] ~> Stream[F, *]
  )(
      implicit
      final override val lgv: LabelledGeneric.Aux[V, HV]
  ) extends ModuleTypes.Aux(V)
      with FreeKeyValueStore[F, K, V, HV]
}

/** */
object freestore extends freestore

//
// old api:
//
// case class Select(key: Key)               extends Command[Option[Value]]
// case class Create(key: Key, value: Value) extends Command[Option[Id]]
// case class Update(key: Key, value: Value) extends Command[Boolean]
// case class Upsert(key: Key, value: Value) extends Command[Option[Id]] // update() == true
// case class Delete(key: Key)               extends Command[Boolean]
