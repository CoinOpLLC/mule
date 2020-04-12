package io.deftrade
package contracts

import cats.{ Eval, Group }

/**  This trait intentionally left blank. */
sealed trait Contract

/**
  * ADT definitions and constructors form the `Contract` specification DSL.
  *
  * Note internally the `Contract` instances are lazy, via [[cats.Eval]].
  *
  * Implements the [[cats.Group]] typeclass instance for `Contract`.
  *
  * TODO: consider:
  *   - require "consideration" in every contract representing a `Trade`
  *       - which can be constructed after the fact if need be
  *   - just like in the real world, there's always this:
  *       - ''Good and valuable consideration, the receipt and sufficiency of which
  *       is hearby acknowledged'', which looks like this in code:
  *       - {{{ object GavcTrasowiha extends Contract /* another Zero */ }}}
  *   - implement {{{ implicit def mc: Monoid[Contract]}}}
  *       - rolls up a combined `Contract` for any two Counterparties.
  */
object Contract {

  type LzCon = Eval[Contract]
  import Eval.later

  case object Zero                                                                extends Contract
  sealed abstract case class One(n: Numéraire)                                    extends Contract
  sealed abstract case class Scale(o: Observable[Double], c: LzCon)               extends Contract
  sealed abstract case class Give(c: LzCon)                                       extends Contract
  sealed abstract case class When(o: Observable[Boolean], c: LzCon)               extends Contract
  sealed abstract case class Until(o: Observable[Boolean], c: LzCon)              extends Contract
  sealed abstract case class Anytime(o: Observable[Boolean], c: LzCon)            extends Contract
  sealed abstract case class Both(c1: LzCon, c2: LzCon)                           extends Contract
  sealed abstract case class Pick(c1: LzCon, c2: LzCon)                           extends Contract
  sealed abstract case class Branch(o: Observable[Boolean], cT: LzCon, cF: LzCon) extends Contract

  implicit lazy val contractGroup: Group[Contract] =
    new Group[Contract] {
      def empty: Contract                             = contracts.zero
      def combine(x: Contract, y: Contract): Contract = contracts both (x, y)
      def inverse(a: Contract): Contract              = contracts give a
    }

  /** Tucked in here so `Contract` can stay `sealed` in source file. */
  trait primitives {

    /** Party immediately acquires one unit of `Numéraire` from counterparty. */
    final def unitOf(base: Numéraire): Contract = new One(base) {}

    /** Party acquires `c` multiplied by `n`. */
    final def scale(n: Observable[Double])(c: => Contract): Contract = new Scale(n, later(c)) {}

    /** Party acquires `cT` if `b` is `true` ''at the moment of acquistion'', else acquires `cF`. */
    final def branch(b: Observable[Boolean])(
        cT: => Contract
    )(
        cF: => Contract
    ): Contract =
      new Branch(b, later(cT), later(cF)) {}

    /** Party assumes role of counterparty with respect to `c`. */
    final def give(c: => Contract): Contract = new Give(later(c)) {}

    /** Party will acquire c as soon as `b` is observed `true`.  */
    final def when(b: Observable[Boolean])(c: => Contract): Contract = new When(b, later(c)) {}

    /** Party acquires `c` with the obligation to abandon it when `o` is observed `true`. */
    final def until(b: Observable[Boolean], c: => Contract): Contract = new Until(b, later(c)) {}

    /** Once you acquire anytime obs c, you may acquire c at any time the observable obs is true. */
    final def anytime(b: Observable[Boolean])(c: => Contract): Contract = new Anytime(b, later(c)) {}

    /** Party immediately receives both `c1` and `c2`. */
    final def both(c1: => Contract, c2: => Contract): Contract = new Both(later(c1), later(c2)) {}

    /** Party immediately chooses between `c1` or `c2`. */
    final def pick(c1: => Contract, c2: => Contract): Contract = new Pick(later(c1), later(c2)) {}
  }
}
