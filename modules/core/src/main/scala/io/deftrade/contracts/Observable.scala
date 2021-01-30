package io.deftrade
package contracts

import time._

import cats.implicits._
import cats.{ Show }

/**
  * `Observable` values which affect [[Contract]] evaluation.
  *
  * We follow the approach taken by [[http://netrium.org/ Netrium]]
  * (make `Observable` an GADT.)
  *
  * TODO: Isn't `Observable[A]` a `Functor`, at least? Would formalizing this simplify anything?
  */
sealed trait Observable[A]

/**  */
object Observable {

  sealed abstract case class Const[A] private (a: A) extends Observable[A]
  object Const { def apply[A](a: A): Const[A] = new Const(a) {} }

  sealed abstract case class Branch[A] private (
      oB: Observable[Boolean],
      oT: Observable[A],
      oF: Observable[A]
  ) extends Observable[A]

  object Branch {
    def apply[A](
        oB: Observable[Boolean],
        oT: Observable[A],
        oF: Observable[A]
    ): Branch[A] = new Branch(oB, oT, oF) {}
  }

  sealed abstract case class Before private (t: Instant) extends Observable[Boolean]
  object Before { def apply(t: Instant): Before = new Before(t) {} }

  sealed abstract case class At private (t: Instant) extends Observable[Boolean]
  object At { def apply(t: Instant): At = new At(t) {} }

  /**  */
  sealed abstract case class Unary[A] private (
      op: Unary.Op[A],
      o: Observable[A]
  ) extends Observable[A]

  /**  */
  object Unary {

    def apply[A](
        op: Op[A],
        o: Observable[A]
    ): Unary[A] = new Unary(op, o) {}

    sealed abstract class Op[A](final val f: A => A)

    case object Not extends Op[Boolean](!_)

    case object Neg  extends Op[Double](-_)
    case object Abs  extends Op[Double](math abs _)
    case object Sqrt extends Op[Double](math sqrt _)
    case object Exp  extends Op[Double](math exp _)
    case object Log  extends Op[Double](math log _)
  }

  /**  */
  sealed abstract case class Binary[A, B] private (
      op: Binary.Op[A, B],
      oL: Observable[A],
      oR: Observable[A]
  ) extends Observable[B]

  /**  */
  object Binary {

    def apply[A, B](
        op: Binary.Op[A, B],
        oL: Observable[A],
        oR: Observable[A]
    ): Binary[A, B] = new Binary(op, oL, oR) {}

    sealed abstract class Op[A, B](final val f: (A, A) => B)

    case object And  extends Op[Boolean, Boolean](_ & _)
    case object Or   extends Op[Boolean, Boolean](_ | _)
    case object Xnor extends Op[Boolean, Boolean](_ | _)

    case object Lt  extends Op[Double, Boolean](_ < _)
    case object Lte extends Op[Double, Boolean](_ <= _)
    case object Gt  extends Op[Double, Boolean](_ > _)
    case object Gte extends Op[Double, Boolean](_ >= _)
    case object Eq  extends Op[Double, Boolean](_ === _)
    case object Neq extends Op[Double, Boolean](_ =!= _)

    case object Add extends Op[Double, Double](_ + _)
    case object Sub extends Op[Double, Double](_ - _)
    case object Mul extends Op[Double, Double](_ * _)
    case object Div extends Op[Double, Double](_ / _)
    case object Min extends Op[Double, Double](_ min _)
    case object Max extends Op[Double, Double](_ max _)
  }

  import Unary._, Binary._

  /** `const(x)` is an observable that has value x at any time. */
  def const[A](a: A): Observable[A] = Const(a)

  /** primitive */
  def before(t: Instant): Observable[Boolean] = Before(t)
  def at(t: Instant): Observable[Boolean]     = At(t)

  def not(o: Observable[Boolean]): Observable[Boolean] = Unary(Not, o)

  def abs(o: Observable[Double]): Observable[Double]  = Unary(Abs, o)
  def sqrt(o: Observable[Double]): Observable[Double] = Unary(Sqrt, o)
  def exp(o: Observable[Double]): Observable[Double]  = Unary(Exp, o)
  def log(o: Observable[Double]): Observable[Double]  = Unary(Log, o)

  /** derived */
  def atOrAfter(t: Instant): Observable[Boolean] = not(before(t))

  /** */
  implicit class BooleanOps(val oL: Observable[Boolean]) extends AnyVal {

    def unary_! : Observable[Boolean] = not(oL)

    def branch(cT: => Contract)(cF: => Contract): Contract = contracts.branch(oL)(cT)(cF)

    def branch[A](oT: => Observable[A])(oF: => Observable[A]): Observable[A] = Branch(oL, oT, oF)

    def and(oR: Observable[Boolean]): Observable[Boolean] = Binary(And, oL, oR)
    def or(oR: Observable[Boolean]): Observable[Boolean]  = Binary(Or, oL, oR)
    def ===(oR: Observable[Boolean]): Observable[Boolean] = Binary(Xnor, oL, oR)
  }

  /** */
  implicit class DoubleOps(val oL: Observable[Double]) extends AnyVal {

    def *(c: => Contract): Contract = contracts.scale(oL)(c)

    def unary_! : Observable[Double] = Unary(Neg, oL)

    def <(oR: Observable[Double]): Observable[Boolean]   = Binary(Lt, oL, oR)
    def <=(oR: Observable[Double]): Observable[Boolean]  = Binary(Lte, oL, oR)
    def >(oR: Observable[Double]): Observable[Boolean]   = Binary(Gt, oL, oR)
    def >=(oR: Observable[Double]): Observable[Boolean]  = Binary(Gte, oL, oR)
    def ===(oR: Observable[Double]): Observable[Boolean] = Binary(Eq, oL, oR)
    def !==(oR: Observable[Double]): Observable[Boolean] = Binary(Neq, oL, oR)

    def +(oR: Observable[Double]): Observable[Double]   = Binary(Add, oL, oR)
    def -(oR: Observable[Double]): Observable[Double]   = Binary(Sub, oL, oR)
    def *(oR: Observable[Double]): Observable[Double]   = Binary(Mul, oL, oR)
    def /(oR: Observable[Double]): Observable[Double]   = Binary(Div, oL, oR)
    def min(oR: Observable[Double]): Observable[Double] = Binary(Min, oL, oR)
    def max(oR: Observable[Double]): Observable[Double] = Binary(Max, oL, oR)
  }

  /** TODO: this needs pretty printing! */
  implicit def obsShow[A]: Show[Observable[A]] = ???
}

/** Commonly seen observables. */
object observables {

  /**
    *   Extremely useful and widely referenced benchmark.
    *   - date implied by the rest of the `Contract`
    *   - time series history is necessary for backtests
    *   - parameter extraction and rate modelling is necessary for pricing
    *   - a sampling schedule should be produced by the scheduling process
    */
  def wsjPrimeRate: Observable[Double] = ???
}
