package io.deftrade
package contracts

import spire.implicits._
import spire.algebra.Field

import cats.{ Eq, Eval, Group, Show }
import Eval.later

/**  This trait intentionally left blank. */
sealed trait Contract extends Product

/**
  * ADT definitions and constructors form the `Contract` specification DSL.
  *
  * Note internally the `Contract` instances are lazy, via [[cats.Eval]].
  *
  * Implements the [[cats.Group]] typeclass instance for `Contract`.
  */
object Contract {

  implicit lazy val contractEq: Eq[Contract]     = Eq.fromUniversalEquals[Contract]
  implicit lazy val contractShow: Show[Contract] = Show.show[Contract](_.toString)

  type LzCon = Eval[Contract]

  case object Zero extends Contract

  sealed abstract case class One private (n: Numéraire) extends Contract
  object One { def apply(n: Numéraire): One = new One(n) {} }

  sealed abstract case class Give private (c: LzCon) extends Contract
  object Give { def apply(c: LzCon): Give = new Give(c) {} }

  sealed abstract case class Scale[N] private (
      o: Oracle[N],
      c: LzCon
  )(
      implicit val N: Field[N]
  ) extends Contract

  object Scale {
    def apply[N: Field](o: Oracle[N], c: LzCon): Scale[N] = new Scale(o, c) {}
  }

  sealed abstract case class When private (o: Oracle[Boolean], c: LzCon) extends Contract
  object When { def apply(o: Oracle[Boolean], c: LzCon): When = new When(o, c) {} }

  sealed abstract case class Until private (o: Oracle[Boolean], c: LzCon) extends Contract
  object Until { def apply(o: Oracle[Boolean], c: LzCon): Until = new Until(o, c) {} }

  sealed abstract case class Anytime private (o: Oracle[Boolean], c: LzCon) extends Contract
  object Anytime { def apply(o: Oracle[Boolean], c: LzCon): Anytime = new Anytime(o, c) {} }

  sealed abstract case class Both private (cA: LzCon, cB: LzCon) extends Contract
  object Both { def apply(cA: LzCon, cB: LzCon): Both = new Both(cA, cB) {} }

  sealed abstract case class Pick private (cA: LzCon, cB: LzCon) extends Contract
  object Pick { def apply(cA: LzCon, cB: LzCon): Pick = new Pick(cA, cB) {} }

  sealed abstract case class Branch private (o: Oracle[Boolean], cT: LzCon, cF: LzCon)
      extends Contract

  object Branch {
    def apply(o: Oracle[Boolean], cT: LzCon, cF: LzCon): Branch =
      new Branch(o, cT, cF) {}
  }

  /** FIXME: Use `Group[Eval[Contract]]` instead!!!
    */
  implicit lazy val contractGroup: Group[Contract] =
    new Group[Contract] {
      def empty: Contract                             = contracts.zero
      def combine(x: Contract, y: Contract): Contract = contracts.both(x)(y)
      def inverse(a: Contract): Contract              = contracts.give(a)
    }

  /** Tucked in here so `Contract` can stay `sealed` in source file.
    */
  trait primitives {

    /** Party acquires no rights or responsibilities.
      */
    final def zero: Contract =
      Zero

    /** Party immediately acquires one unit of `Numéraire` from counterparty.
      */
    final def unitOf(base: Numéraire): Contract =
      One(base)

    /** Party assumes role of counterparty with respect to `c`.
      */
    final def give(c: => Contract): Contract =
      Give(later(c))

    /** Party acquires `c` multiplied by `n`.
      */
    final def scale[N: Field](n: Oracle[N])(c: => Contract): Contract =
      Scale(n, later(c))

    /** Party will acquire c as soon as `b` is observed `true`.
      */
    final def when(b: Oracle[Boolean])(c: => Contract): Contract =
      When(b, later(c))

    /** Party acquires `c` with the obligation to abandon it when `b` is observed `true`.
      */
    final def until(b: Oracle[Boolean])(c: => Contract): Contract =
      Until(b, later(c))

    /** Party acquires the right (but not the obligation)
      * to acquire `c` at any time the oracle `b` is true.
      */
    final def anytime(b: Oracle[Boolean])(c: => Contract): Contract =
      Anytime(b, later(c))

    /** Party acquires `cT` if `b` is `true` ''at the moment of acquistion'', else acquires `cF`.
      */
    final def branch(b: Oracle[Boolean])(cT: => Contract)(cF: => Contract): Contract =
      Branch(b, later(cT), later(cF))

    /** Party immediately receives both `cA` and `cB`.
      */
    final def both(cA: => Contract)(cB: => Contract): Contract =
      Both(later(cA), later(cB))

    /** Party immediately chooses between `cA` or `cB`.
      */
    final def pick(cA: => Contract)(cB: => Contract): Contract =
      Pick(later(cA), later(cB))
  }

  implicit class ContractOps(val c: Contract) extends AnyVal {

    def unary_- : Contract =
      contracts.give(c)

    def scaled[N: Field](n: Oracle[N]): Contract =
      contracts.scale(n)(c)

    def *[N: Field](n: Oracle[N]): Contract =
      c scaled n

    def when(b: Oracle[Boolean]): Contract =
      contracts.when(b)(c)

    def anytime(b: Oracle[Boolean]): Contract =
      contracts.anytime(b)(c)

    def until(b: Oracle[Boolean]): Contract =
      contracts.until(b)(c)

    def combine(c2: => Contract): Contract =
      contracts.both(c)(c2)
  }
}
