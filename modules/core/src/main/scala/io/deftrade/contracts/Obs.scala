package io.deftrade
package contracts

import time._

import cats.implicits._
import cats.{ Show }

/**
  * `Obs`ervable variables which affect `Contract` evaluation.
  *
  * We follow the approach taken by [[http://netrium.org/ Netrium]]
  * (make `Obs` an GADT.)
  *
  * TODO: Isn't `Obs[A]` a `Functor`, at least? Would formalizing this simplify anything?
  */
sealed trait Obs[A]

/**  */
object Obs {

  sealed abstract case class Const[A](a: A)                                      extends Obs[A]
  sealed abstract case class Branch[A](oB: Obs[Boolean], oT: Obs[A], oF: Obs[A]) extends Obs[A]

  sealed abstract case class Before(t: Instant) extends Obs[Boolean]
  sealed abstract case class At(t: Instant)     extends Obs[Boolean]

  import Unary.{ Op => UnOp, _ }

  /**  */
  sealed abstract case class Unary[A](op: UnOp[A], o: Obs[A]) extends Obs[A]

  /**  */
  object Unary {

    sealed abstract class Op[A](val f: A => A)

    case object Not extends Op[Boolean](!_)

    case object Neg  extends Op[Double](-_)
    case object Abs  extends Op[Double](math abs _)
    case object Sqrt extends Op[Double](math sqrt _)
    case object Exp  extends Op[Double](math exp _)
    case object Log  extends Op[Double](math log _)
  }

  import Binary.{ Op => BinOp, _ }

  /**  */
  sealed abstract case class Binary[A, B](op: BinOp[A, B], oL: Obs[A], oR: Obs[A]) extends Obs[B]

  /**  */
  object Binary {

    sealed abstract class Op[A, B](val f: (A, A) => B)

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

  /** `const(x)` is an observable that has value x at any time. */
  def const[A](a: A): Obs[A] = new Const(a) {}

  /** primitive */
  def before(t: Instant): Obs[Boolean] = new Before(t) {}
  def at(t: Instant): Obs[Boolean]     = new At(t)     {}

  def not(o: Obs[Boolean]): Obs[Boolean] = new Unary(Not, o) {}

  def abs(o: Obs[Double]): Obs[Double]  = new Unary(Abs, o)  {}
  def sqrt(o: Obs[Double]): Obs[Double] = new Unary(Sqrt, o) {}
  def exp(o: Obs[Double]): Obs[Double]  = new Unary(Exp, o)  {}
  def log(o: Obs[Double]): Obs[Double]  = new Unary(Log, o)  {}

  /** derived */
  def atOrAfter(t: Instant): Obs[Boolean] = not(before(t))

  /** */
  implicit class BooleanOps(val oL: Obs[Boolean]) extends AnyVal {

    def unary_! : Obs[Boolean] = not(oL)

    def branch(cT: => Contract)(cF: => Contract): Contract = contracts.branch(oL)(cT)(cF)
    def branch[A](oT: => Obs[A])(oF: => Obs[A]): Obs[A]    = new Branch(oL, oT, oF) {}

    def and(oR: Obs[Boolean]): Obs[Boolean] = new Binary(And, oL, oR)  {}
    def or(oR: Obs[Boolean]): Obs[Boolean]  = new Binary(Or, oL, oR)   {}
    def ===(oR: Obs[Boolean]): Obs[Boolean] = new Binary(Xnor, oL, oR) {}
  }

  /** */
  implicit class DoubleOps(val oL: Obs[Double]) extends AnyVal {

    def *(c: => Contract): Contract = contracts.scale(oL)(c)

    def unary_! : Obs[Double] = new Unary(Neg, oL) {}

    def <(oR: Obs[Double]): Obs[Boolean]   = new Binary(Lt, oL, oR)  {}
    def <=(oR: Obs[Double]): Obs[Boolean]  = new Binary(Lte, oL, oR) {}
    def >(oR: Obs[Double]): Obs[Boolean]   = new Binary(Gt, oL, oR)  {}
    def >=(oR: Obs[Double]): Obs[Boolean]  = new Binary(Gte, oL, oR) {}
    def ===(oR: Obs[Double]): Obs[Boolean] = new Binary(Eq, oL, oR)  {}
    def !==(oR: Obs[Double]): Obs[Boolean] = new Binary(Neq, oL, oR) {}

    def +(oR: Obs[Double]): Obs[Double]   = new Binary(Add, oL, oR) {}
    def -(oR: Obs[Double]): Obs[Double]   = new Binary(Sub, oL, oR) {}
    def *(oR: Obs[Double]): Obs[Double]   = new Binary(Mul, oL, oR) {}
    def /(oR: Obs[Double]): Obs[Double]   = new Binary(Div, oL, oR) {}
    def min(oR: Obs[Double]): Obs[Double] = new Binary(Min, oL, oR) {}
    def max(oR: Obs[Double]): Obs[Double] = new Binary(Max, oL, oR) {}
  }

  /** TODO: this needs pretty printing! */
  implicit def obsShow[A]: Show[Obs[A]] = ???
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
  def wsjPrimeRate: Obs[Double] = ???
}
