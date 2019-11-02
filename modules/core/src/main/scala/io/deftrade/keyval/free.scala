package io.deftrade
package keyval

import cats.{ ~> }
import cats.free.Free
import cats.free.Free.liftF

import cats.effect.{ ContextShift, IO, Sync }

import shapeless.{ HList, LabelledGeneric }

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
    final type EffectCommand[x] = Command[EffectType, x]

    /** */
    type FreeCommand[A] = Free[EffectCommand, A]

    /** */
    def impl: EffectCommand ~> EffectStream

    case class Get(key: Key)               extends EffectCommand[Result[Value]]
    case class Let(key: Key, value: Value) extends EffectCommand[Result[Id]]
    case class Set(key: Key, value: Value) extends EffectCommand[Result[Boolean]]
    case class Put(key: Key, value: Value) extends EffectCommand[Result[Id]]
    case class Del(key: Key)               extends EffectCommand[Result[Boolean]]

    /** */
    def get(key: Key): FreeCommand[Result[Value]] =
      liftF[EffectCommand, Result[Value]](Get(key))

    /** */
    def let(key: Key, value: Value): FreeCommand[Result[Id]] =
      liftF[EffectCommand, Result[Id]](Let(key, value))

    /** */
    def set(key: Key, value: Value): FreeCommand[Result[Boolean]] =
      liftF[EffectCommand, Result[Boolean]](Set(key, value))

    /** */
    def put(key: Key, value: Value): FreeCommand[Result[Id]] =
      liftF[EffectCommand, Result[Id]](Put(key, value))

    /** */
    def del(key: Key): FreeCommand[Result[Boolean]] =
      liftF[EffectCommand, Result[Boolean]](Del(key))
  }

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  sealed abstract class XKVS[
      F[_]: Sync: ContextShift,
      K,
      V,
      HV <: HList
  ] private (
      final override val V: WithKey.Aux[K, V],
      final override val impl: ({ type C[t] = Command[F, t] })#C ~> ({ type S[r] = Stream[F, r] })#S
  )(
      implicit
      final override val lgv: LabelledGeneric.Aux[V, HV]
  ) extends ModuleTypes.Aux(V)
      with FreeKeyValueStore[F, K, V, HV]

  /** */
  object XKVS {

    /** */
    def apply[
        F[_]: Sync: ContextShift,
        K,
        V,
        HV <: HList
    ](
        V: WithKey.Aux[K, V],
        impl: ({ type C[t] = Command[F, t] })#C ~> ({ type S[r] = Stream[F, r] })#S
    )(
        implicit lgv: LabelledGeneric.Aux[V, HV]
    ) = new XKVS(V, impl) {}

    /** */
    implicit def FIXME: ContextShift[IO] = ???

    /** */
    def withIO[K, V, HV <: HList](
        V: WithKey.Aux[K, V],
        impl: ({ type C[t] = Command[IO, t] })#C ~> ({ type S[r] = Stream[IO, r] })#S
    )(
        implicit lgv: LabelledGeneric.Aux[V, HV]
    ) = apply[IO, K, V, HV](V, impl)
  }
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
