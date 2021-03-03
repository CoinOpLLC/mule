package io.deftrade
package contracts

import spire.implicits._
import spire.algebra.{ Bool, Field, NRoot, Order, Signed, Trig }

import cats.{ Show }

import java.time.Instant

/**
  * `Oracle` values which affect [[Contract]] evaluation.
  *
  * We follow the approach taken by [[http://netrium.org/ Netrium]]
  * (make `Oracle` an GADT.)
  *
  * TODO: Isn't `Oracle[A]` a `Functor`, at least? Would formalizing this simplify anything?
  * TODO: revisit variance, which was added for pattern matching
  */
sealed trait Oracle[+A]

/**  */
object Oracle {

  sealed abstract case class Const[A] private (a: A) extends Oracle[A]
  object Const { def apply[A](a: A): Const[A] = new Const(a) {} }

  sealed abstract case class Branch[A] private (
      oB: Oracle[Boolean],
      oT: Oracle[A],
      oF: Oracle[A]
  ) extends Oracle[A]

  object Branch {

    def apply[A](oB: Oracle[Boolean], oT: Oracle[A], oF: Oracle[A]): Branch[A] =
      new Branch(oB, oT, oF) {}
  }

  sealed abstract case class Before private (t: Instant) extends Oracle[Boolean]
  object Before { def apply(t: Instant): Before = new Before(t) {} }

  sealed abstract case class At private (t: Instant) extends Oracle[Boolean]
  object At { def apply(t: Instant): At = new At(t) {} }

  /**  */
  sealed abstract case class Unary[A] private (
      op: Unary.Op[A],
      o: Oracle[A]
  ) extends Oracle[A]

  /**  */
  object Unary {

    def apply[A](op: Op[A], o: Oracle[A]): Unary[A] =
      new Unary(op, o) {}

    sealed abstract class Op[A](final val f: A => A)

    final case class Not[A: Bool]()   extends Op[A](x => Bool[A].xor(Bool[A].one, x))
    final case class Neg[A: Field]()  extends Op[A](-_)
    final case class Abs[A: Signed]() extends Op[A](Signed[A] abs _)
    final case class Sqrt[A: NRoot]() extends Op[A](NRoot[A] sqrt _)
    final case class Exp[A: Trig]()   extends Op[A](Trig[A] exp _)
    final case class Log[A: Trig]()   extends Op[A](Trig[A] log _)
  }

  /**  */
  sealed abstract case class Binary[A, B] private (
      op: Binary.Op[A, B],
      oL: Oracle[A],
      oR: Oracle[A]
  ) extends Oracle[B]

  /**  */
  object Binary {

    def apply[A, B](
        op: Binary.Op[A, B],
        oL: Oracle[A],
        oR: Oracle[A]
    ): Binary[A, B] = new Binary(op, oL, oR) {}

    sealed abstract class Op[A, B](final val f: (A, A) => B)

    final case class And[A: Bool]()  extends Op[A, A](_ & _)
    final case class Or[A: Bool]()   extends Op[A, A](_ | _)
    final case class Xnor[A: Bool]() extends Op[A, A](_ | _)

    final case class Lt[A: Order]()  extends Op[A, Boolean](_ < _)
    final case class Lte[A: Order]() extends Op[A, Boolean](_ <= _)
    final case class Gt[A: Order]()  extends Op[A, Boolean](_ > _)
    final case class Gte[A: Order]() extends Op[A, Boolean](_ >= _)
    final case class Eq[A: Order]()  extends Op[A, Boolean](_ === _)
    final case class Neq[A: Order]() extends Op[A, Boolean](_ =!= _)

    final case class Min[A: Order]() extends Op[A, A](_ min _)
    final case class Max[A: Order]() extends Op[A, A](_ max _)

    final case class Add[A: Field]() extends Op[A, A](_ + _)
    final case class Sub[A: Field]() extends Op[A, A](_ - _)
    final case class Mul[A: Field]() extends Op[A, A](_ * _)
    final case class Div[A: Field]() extends Op[A, A](_ / _)
  }

  import Unary._, Binary._

  /** `const(x)` is an observable that has value x at any time. */
  def const[A](a: A): Oracle[A] = Const(a)

  def before(t: Instant): Oracle[Boolean] = Before(t)
  def at(t: Instant): Oracle[Boolean]     = At(t)

  def not[A: Bool](o: Oracle[A]): Oracle[A] = Unary[A](Not(), o)

  def abs[A: Signed](o: Oracle[A]): Oracle[A] = Unary[A](Abs(), o)

  def sqrt[A: NRoot](o: Oracle[A]): Oracle[A] = Unary[A](Sqrt(), o)

  def exp[A: Trig](o: Oracle[A]): Oracle[A] = Unary[A](Exp(), o)
  def log[A: Trig](o: Oracle[A]): Oracle[A] = Unary[A](Log(), o)

  def atOrAfter(t: Instant): Oracle[Boolean] = not(before(t))

  implicit class BooleanOps(val oL: Oracle[Boolean]) extends AnyVal {
    def branch(cT: => Contract)(cF: => Contract): Contract = contracts.branch(oL)(cT)(cF)

    def branch[A](oT: => Oracle[A])(oF: => Oracle[A]): Oracle[A] = Branch(oL, oT, oF)
  }

  implicit class BoolOps[A: Bool](oL: Oracle[A]) {

    def unary_! : Oracle[A] = not(oL)

    def and(oR: Oracle[A]): Oracle[A] = Binary[A, A](And(), oL, oR)
    def or(oR: Oracle[A]): Oracle[A]  = Binary[A, A](Or(), oL, oR)
    def ===(oR: Oracle[A]): Oracle[A] = Binary[A, A](Xnor(), oL, oR)
  }

  implicit class OrderOps[A: Order](val oL: Oracle[A]) {

    def <(oR: Oracle[A]): Oracle[Boolean]   = Binary[A, Boolean](Lt(), oL, oR)
    def <=(oR: Oracle[A]): Oracle[Boolean]  = Binary[A, Boolean](Lte(), oL, oR)
    def >(oR: Oracle[A]): Oracle[Boolean]   = Binary[A, Boolean](Gt(), oL, oR)
    def >=(oR: Oracle[A]): Oracle[Boolean]  = Binary[A, Boolean](Gte(), oL, oR)
    def ===(oR: Oracle[A]): Oracle[Boolean] = Binary[A, Boolean](Eq(), oL, oR)
    def !==(oR: Oracle[A]): Oracle[Boolean] = Binary[A, Boolean](Neq(), oL, oR)

    def min(oR: Oracle[A]): Oracle[A] = Binary[A, A](Min(), oL, oR)
    def max(oR: Oracle[A]): Oracle[A] = Binary[A, A](Max(), oL, oR)
  }

  implicit class FieldOps[A: Field](val oL: Oracle[A]) {

    def unary_- : Oracle[A] = Unary[A](Neg(), oL)

    def +(oR: Oracle[A]): Oracle[A] = Binary[A, A](Add(), oL, oR)
    def -(oR: Oracle[A]): Oracle[A] = Binary[A, A](Sub(), oL, oR)
    def *(oR: Oracle[A]): Oracle[A] = Binary[A, A](Mul(), oL, oR)
    def /(oR: Oracle[A]): Oracle[A] = Binary[A, A](Div(), oL, oR)
  }

  /** TODO: this needs pretty printing! */
  implicit def obsShow[A]: Show[Oracle[A]] = ???
}

/** Commonly seen oracles. */
object oracles {

  /**
    *   Extremely useful and widely referenced benchmark.
    *   - date implied by the rest of the `Contract`
    *   - time series history is necessary for backtests
    *   - parameter extraction and rate modelling is necessary for pricing
    *   - a sampling schedule should be produced by the scheduling process
    */
  def wsjPrimeRate: Oracle[Double] = ???
}
