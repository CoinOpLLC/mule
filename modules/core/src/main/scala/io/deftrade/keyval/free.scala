package io.deftrade
package keyval

import cats.implicits._
import cats.{ ~> }
import cats.free.Free
import cats.free.Free.liftF

import cats.effect.{ ContextShift, IO, Sync }

import shapeless.labelled._
import shapeless.{ HList, LabelledGeneric }

import fs2.Stream

import scala.language.higherKinds

object stoars extends stores

import stoars.ModuleTypes

sealed abstract case class Foo private (
    i: Int,
    s: String
)

object Foo extends WithOpaqueKey[Long, Foo] {
  def apply(
      i: Int,
      s: String
  ) = new Foo(i, s) {}
}

sealed trait Command[A]
object Command {
  type IOStream[x] = Stream[IO, x]
}
abstract class FreeKeyValueStore[
    K,
    V,
    HV <: HList
](
    override final val V: WithKey.Aux[K, V]
)(
    implicit
    final val whatTheActualFuck: ContextShift[IO],
    final override val lgv: LabelledGeneric.Aux[V, HV],
    // final val impl: Command ~> ({ type SF[x] = Stream[IO, x] })#SF
    final val impl: Command ~> Command.IOStream
) extends ModuleTypes.Aux[
      IO,
      ({ type W[v] = WithKey.Aux[K, v] })#W,
      V,
      HV
    ](V) {

  import V._

  // case class Select(key: Key)               extends Command[Option[Value]]
  // case class Create(key: Key, value: Value) extends Command[Option[Id]]
  // case class Update(key: Key, value: Value) extends Command[Boolean]
  // case class Upsert(key: Key, value: Value) extends Command[Option[Id]] // check update() == true
  // case class Delete(key: Key)               extends Command[Boolean]

  case class Get(key: Key)               extends Command[Result[Value]]
  case class Let(key: Key, value: Value) extends Command[Result[Id]]
  case class Set(key: Key, value: Value) extends Command[Result[Boolean]]
  case class Put(key: Key, value: Value) extends Command[Result[Id]] // check update() == true
  case class Del(key: Key)               extends Command[Result[Boolean]]

  type FreeCommand[A] = Free[Command, A]

  /** */
  def get(key: Key): FreeCommand[Result[Value]] =
    liftF[Command, Result[Value]](Get(key))

  /** */
  def let(key: Key, value: Value): FreeCommand[Result[Id]] =
    liftF[Command, Result[Id]](Let(key, value))

  /** */
  def set(key: Key, value: Value): FreeCommand[Result[Boolean]] =
    liftF[Command, Result[Boolean]](Set(key, value))

  /** */
  def put(key: Key, value: Value): FreeCommand[Result[Id]] =
    liftF[Command, Result[Id]](Put(key, value))

  /** */
  def del(key: Key): FreeCommand[Result[Boolean]] =
    liftF[Command, Result[Boolean]](Del(key))
}

trait Wut {
  implicit def whatTheActualFuck: ContextShift[IO] = ???
  implicit def anotherFuckingHeadSmack: Command ~> Command.IOStream = ???
  val x          = Foo
  implicit val y = LabelledGeneric[Foo]
  class FooStoar extends FreeKeyValueStore(x)
}
