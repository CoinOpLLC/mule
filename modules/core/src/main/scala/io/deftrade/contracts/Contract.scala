package io.deftrade
package contracts

import money.Financial

import cats.{ Eval, Group }
import Eval.later

/**  This trait intentionally left blank. */
sealed trait Contract

/**
  * ADT definitions and constructors form the `Contract` specification DSL.
  *
  * Note internally the `Contract` instances are lazy, via [[cats.Eval]].
  *
  * Implements the [[cats.Group]] typeclass instance for `Contract`.
  */
object Contract {

  type LzCon = Eval[Contract]

  case object Zero                                                            extends Contract
  sealed abstract case class One(n: Numéraire)                                extends Contract
  sealed abstract case class Scale[N: Financial](o: Oracle[N], c: LzCon)      extends Contract
  sealed abstract case class Give(c: LzCon)                                   extends Contract
  sealed abstract case class When(o: Oracle[Boolean], c: LzCon)               extends Contract
  sealed abstract case class Until(o: Oracle[Boolean], c: LzCon)              extends Contract
  sealed abstract case class Anytime(o: Oracle[Boolean], c: LzCon)            extends Contract
  sealed abstract case class Both(cA: LzCon, cB: LzCon)                       extends Contract
  sealed abstract case class Pick(cA: LzCon, cB: LzCon)                       extends Contract
  sealed abstract case class Branch(o: Oracle[Boolean], cT: LzCon, cF: LzCon) extends Contract

  implicit lazy val contractGroup: Group[Contract] =
    new Group[Contract] {
      def empty: Contract                             = contracts.zero
      def combine(x: Contract, y: Contract): Contract = contracts.both(x, y)
      def inverse(a: Contract): Contract              = contracts give a
    }

  /** Tucked in here so `Contract` can stay `sealed` in source file.
    */
  trait primitives {

    /** Party immediately acquires one unit of `Numéraire` from counterparty.
      */
    final def unitOf(base: Numéraire): Contract =
      new One(base) {}

    /** Party acquires `c` multiplied by `n`.
      */
    final def scale[N: Financial](n: Oracle[N])(c: => Contract): Contract =
      new Scale(n, later(c)) {}

    /** Party assumes role of counterparty with respect to `c`.
      */
    final def give(c: => Contract): Contract =
      new Give(later(c)) {}

    /** Party will acquire c as soon as `b` is observed `true`.
      */
    final def when(b: Oracle[Boolean])(c: => Contract): Contract =
      new When(b, later(c)) {}

    /** Party acquires `c` with the obligation to abandon it when `o` is observed `true`.
      */
    final def until(b: Oracle[Boolean], c: => Contract): Contract =
      new Until(b, later(c)) {}

    /** Once you acquire anytime obs c, you may acquire c at any time the observable obs is true.
      */
    final def anytime(b: Oracle[Boolean])(c: => Contract): Contract =
      new Anytime(b, later(c)) {}

    /** Party acquires `cT` if `b` is `true` ''at the moment of acquistion'', else acquires `cF`.
      */
    final def branch(b: Oracle[Boolean])(cT: => Contract)(cF: => Contract): Contract =
      new Branch(b, later(cT), later(cF)) {}

    /** Party immediately receives both `cA` and `cB`.
      */
    final def both(cA: => Contract, cB: => Contract): Contract =
      new Both(later(cA), later(cB)) {}

    /** Party immediately chooses between `cA` or `cB`.
      */
    final def pick(cA: => Contract, cB: => Contract): Contract =
      new Pick(later(cA), later(cB)) {}
  }
}
