package io.deftrade
package contracts

import spire.implicits._
import spire.algebra.{ Bool, Field, NRoot, Order, Signed, Trig }

import cats.{ Show }

import java.time.Instant
import Instant.now

/**
  * `Oracle` values which affect [[Contract]] evaluation.
  *
  * We follow the approach taken by [[http://netrium.org/ Netrium]]
  * (make `Oracle` an GADT.)
  *
  */
sealed trait Oracle[A]

/**  */
object Oracle {

  sealed abstract case class Const[A] private (final val a: A) extends Oracle[A]
  object Const {
    def apply[A](a: A): Const[A] = new Const(a) {}
  }

  sealed abstract case class Branch[A] private (oB: Oracle[Boolean], oT: Oracle[A], oF: Oracle[A])
      extends Oracle[A]
  object Branch {
    def apply[A](oB: Oracle[Boolean], oT: Oracle[A], oF: Oracle[A]): Branch[A] =
      new Branch(oB, oT, oF) {}
  }

  sealed abstract case class Before private (t: Instant) extends Oracle[Boolean]
  object Before {
    def apply(t: Instant): Before = new Before(t) {}
  }

  sealed abstract case class At private (t: Instant) extends Oracle[Boolean]
  object At {
    def apply(t: Instant): At = new At(t) {}
  }

  /**
    */
  sealed abstract case class Unary[A] private (op: Unary.Op[A], o: Oracle[A]) extends Oracle[A]

  /**
    */
  object Unary {

    def apply[A](op: Op[A], o: Oracle[A]): Unary[A] =
      new Unary(op, o) {}

    sealed abstract class Op[A](f: A => A) {
      def apply(a: A): A =
        f(a)
    }

    final case class Not[A: Bool]()   extends Op[A](x => Bool[A].xor(Bool[A].one, x))
    final case class Neg[A: Field]()  extends Op[A](-_)
    final case class Abs[A: Signed]() extends Op[A](Signed[A] abs _)
    final case class Sqrt[A: NRoot]() extends Op[A](NRoot[A] sqrt _)
    final case class Exp[A: Trig]()   extends Op[A](Trig[A] exp _)
    final case class Log[A: Trig]()   extends Op[A](Trig[A] log _)
  }

  /**
    */
  sealed abstract case class Binary[A] private (
      op: Binary.Op[A],
      oL: Oracle[A],
      oR: Oracle[A]
  ) extends Oracle[A]

  /**  */
  object Binary {

    def apply[A](
        op: Binary.Op[A],
        oL: Oracle[A],
        oR: Oracle[A]
    ): Binary[A] =
      new Binary(op, oL, oR) {}

    sealed abstract class Op[A](f: (A, A) => A) {
      def apply(l: A, r: A): A =
        f(l, r)
    }

    final case class And[A: Bool]()  extends Op[A](_ & _)
    final case class Or[A: Bool]()   extends Op[A](_ | _)
    final case class Xnor[A: Bool]() extends Op[A](_ | _)

    final case class Min[A: Order]() extends Op[A](_ min _)
    final case class Max[A: Order]() extends Op[A](_ max _)

    final case class Add[A: Field]() extends Op[A](_ + _)
    final case class Sub[A: Field]() extends Op[A](_ - _)
    final case class Mul[A: Field]() extends Op[A](_ * _)
    final case class Div[A: Field]() extends Op[A](_ / _)
  }

  /**
    */
  sealed abstract case class Predicate[A] private (
      op: Predicate.Op[A],
      oL: Oracle[A],
      oR: Oracle[A]
  ) extends Oracle[Boolean] {
    final type ArgType = A
  }

  /**  */
  object Predicate {

    def apply[A](
        op: Predicate.Op[A],
        oL: Oracle[A],
        oR: Oracle[A]
    ): Predicate[A] =
      new Predicate(op, oL, oR) {}

    sealed abstract class Op[A](final val p: (A, A) => Boolean) {
      def apply(l: A, r: A): Boolean =
        p(l, r)
    }

    final case class Lt[A: Order]()  extends Op[A](_ < _)
    final case class Lte[A: Order]() extends Op[A](_ <= _)
    final case class Gt[A: Order]()  extends Op[A](_ > _)
    final case class Gte[A: Order]() extends Op[A](_ >= _)
    final case class Eq[A: Order]()  extends Op[A](_ === _)
    final case class Neq[A: Order]() extends Op[A](_ =!= _)
  }

  def eval[A](o: Oracle[A]): A = o match {
    case Const(a)           => a
    case Branch(b, oT, oF)  => if (eval(b)) eval(oT) else eval(oF)
    case Before(t)          => now isBefore t
    case At(t)              => (now isAfter t) && (now isBefore (t plusSeconds 60 * 60 * 24))
    case Unary(op, o)       => op(eval(o))
    case Binary(op, oL, oR) => op(eval(oL), eval(oR))

    case pred @ Predicate(op, oL, oR) =>
      op(eval[pred.ArgType](oL), eval[pred.ArgType](oR))
  }

  import Unary._, Binary._, Predicate._

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

    def and(oR: Oracle[A]): Oracle[A] = Binary[A](And(), oL, oR)
    def or(oR: Oracle[A]): Oracle[A]  = Binary[A](Or(), oL, oR)
    def ===(oR: Oracle[A]): Oracle[A] = Binary[A](Xnor(), oL, oR)
  }

  implicit class OrderOps[A: Order](val oL: Oracle[A]) {

    def <(oR: Oracle[A]): Oracle[Boolean]   = Predicate[A](Lt(), oL, oR)
    def <=(oR: Oracle[A]): Oracle[Boolean]  = Predicate[A](Lte(), oL, oR)
    def >(oR: Oracle[A]): Oracle[Boolean]   = Predicate[A](Gt(), oL, oR)
    def >=(oR: Oracle[A]): Oracle[Boolean]  = Predicate[A](Gte(), oL, oR)
    def ===(oR: Oracle[A]): Oracle[Boolean] = Predicate[A](Eq(), oL, oR)
    def !==(oR: Oracle[A]): Oracle[Boolean] = Predicate[A](Neq(), oL, oR)

    def min(oR: Oracle[A]): Oracle[A] = Binary[A](Min(), oL, oR)
    def max(oR: Oracle[A]): Oracle[A] = Binary[A](Max(), oL, oR)
  }

  implicit class FieldOps[A: Field](val oL: Oracle[A]) {

    def unary_- : Oracle[A] = Unary[A](Neg(), oL)

    def +(oR: Oracle[A]): Oracle[A] = Binary[A](Add(), oL, oR)
    def -(oR: Oracle[A]): Oracle[A] = Binary[A](Sub(), oL, oR)
    def *(oR: Oracle[A]): Oracle[A] = Binary[A](Mul(), oL, oR)
    def /(oR: Oracle[A]): Oracle[A] = Binary[A](Div(), oL, oR)
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
