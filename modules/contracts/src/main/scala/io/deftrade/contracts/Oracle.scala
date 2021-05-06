package io.deftrade
package contracts

import spire.implicits._
import spire.algebra.{ Bool, Field, NRoot, Order, Signed, Trig }

import cats.{ Eval }

import java.time.Instant
import Instant.now

/**
  * `Oracle` values which affect [[Contract]] evaluation.
  *
  * We follow the approach taken by [[http://netrium.org/ Netrium]]
  * (`Oracle` is a lazy, stack safe GADT)
  */
/**
  */
object Oracle {

  sealed abstract case class Election[F[_]] private (result: Eval[F[Boolean]])

  object Election {
    def apply[F[_]](result: F[Boolean]): Election[F] = new Election(Eval later result) {}
  }

  def const[A](a: => A): Eval[A] =
    Eval later a

  def branch[A](b: Eval[Boolean], t: Eval[A], f: Eval[A]): Eval[A] =
    Eval later (if (b.value) t.value else f.value)

  def before(t: Instant): Eval[Boolean] =
    Eval later (now isBefore t)

  def at(t: Instant): Eval[Boolean] =
    Eval later (now isAfter t) && (now isBefore (t plusSeconds 60 * 60 * 24))

  def not[A: Bool](a: Eval[A]): Eval[A] =
    Eval later Bool[A].xor(Bool[A].one, a.value)

  def abs[A: Signed](a: Eval[A]): Eval[A] =
    Eval later Signed[A].abs(a.value)

  def sqrt[A: NRoot](a: Eval[A]): Eval[A] =
    Eval later NRoot[A].sqrt(a.value)

  def exp[A: Trig](a: Eval[A]): Eval[A] =
    Eval later Trig[A].exp(a.value)

  def log[A: Trig](a: Eval[A]): Eval[A] =
    Eval later Trig[A].log(a.value)

  def atOrAfter(t: Instant): Eval[Boolean] =
    not(before(t))

  implicit class OrderOps[A: Order](val l: Eval[A]) {
    def <(r: Eval[A]): Eval[Boolean]   = Eval later l.value < r.value
    def <=(r: Eval[A]): Eval[Boolean]  = Eval later l.value <= r.value
    def >(r: Eval[A]): Eval[Boolean]   = Eval later l.value > r.value
    def >=(r: Eval[A]): Eval[Boolean]  = Eval later l.value >= r.value
    def ===(r: Eval[A]): Eval[Boolean] = Eval later l.value === r.value
    def =!=(r: Eval[A]): Eval[Boolean] = Eval later l.value =!= r.value
    def min(r: Eval[A]): Eval[A]       = Eval later (l.value min r.value)
    def max(r: Eval[A]): Eval[A]       = Eval later (l.value max r.value)
  }

  implicit class FieldOps[A: Field](val l: Eval[A]) {
    def unary_- : Eval[A]      = Eval later -l.value
    def +(r: Eval[A]): Eval[A] = Eval later (l.value + r.value)
    def -(r: Eval[A]): Eval[A] = Eval later (l.value - r.value)
    def *(r: Eval[A]): Eval[A] = Eval later (l.value * r.value)
    def /(r: Eval[A]): Eval[A] = Eval later (l.value / r.value)
  }

  implicit class BoolOps[A: Bool](val l: Eval[A]) {
    def unary_! : Eval[A]        = !l
    def ===(r: Eval[A]): Eval[A] = !(l ^ r)
    def &(r: Eval[A]): Eval[A]   = Eval later (l.value & r.value)
    def |(r: Eval[A]): Eval[A]   = Eval later (l.value | r.value)
    def ^(r: Eval[A]): Eval[A]   = Eval later (l.value ^ r.value)
  }

  implicit class BooleanOps(val b: Eval[Boolean]) {
    def branch(t: => Contract)(f: => Contract): Contract =
      contracts.branch(b)(t)(f)
  }
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
  def wsjPrimeRate: Eval[Double] = ???
}
