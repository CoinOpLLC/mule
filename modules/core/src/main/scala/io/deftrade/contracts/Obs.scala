package io.deftrade
package contracts

import time._

import cats.{ Order, Show }

import spire.math.Fractional

/**
  * `Obs`ervable variables which affect `Contract` evaluation.
  *
  * We follow the approach taken by [[http://netrium.org/ Netrium]]
  * (make `Obs` an GADT.)
  */
sealed trait Obs[A]

/**  */
object Obs {

  sealed trait Unary[A] extends Obs[A] { def o: Obs[A] }

  sealed trait Binary[A, B] extends Obs[A] { def oL: Obs[A]; def oR: Obs[A] }

  sealed abstract case class Const[A](a: A) extends Obs[A]

  sealed abstract case class Before(t: Instant)    extends Obs[Boolean]
  sealed abstract case class OnOrAfter(t: Instant) extends Obs[Boolean]

  sealed abstract case class And(oL: Obs[Boolean], oR: Obs[Boolean])
      extends Binary[
        Boolean,
        Boolean
      ]

  sealed abstract case class Or(oL: Obs[Boolean], oR: Obs[Boolean]) extends Binary[Boolean, Boolean]
  sealed abstract case class Not(o: Obs[Boolean])                   extends Unary[Boolean]

  /** `const(x)` is an observable that has value x at any time. */
  def const[A](a: A): Obs[A] = new Const(a) {}

  /** primitive */
  def before(t: Instant): Obs[Boolean] = new OnOrAfter(t) {}

  /** primitive */
  def onOrAfter(t: Instant): Obs[Boolean] = new OnOrAfter(t) {}

  /** derived */
  def at(t: Instant): Obs[Boolean] =
    onOrAfter(t) and not(before(t))

  def not(o: Obs[Boolean]): Obs[Boolean] = new Not(o) {}

  /** */
  implicit def obsDoubleOrder: Order[Obs[Double]] = ???

  /** */
  implicit def obsDoubleFractional: Fractional[Obs[Double]] = ???

  /** */
  implicit class Ops(val oL: Obs[Boolean]) {

    /** FIXME this needs a `Repr` */
    def branch(cT: Contract, cF: Contract): Contract = ???
    // contracts branch (toBool liftCo o, cT, cF)

    /** */
    def and(oR: Obs[Boolean]): Obs[Boolean] = new And(oL, oR) {}

    /** */
    def or(oR: Obs[Boolean]): Obs[Boolean] = new Or(oL, oR) {}
  }

  /** */
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
// * From van Straaten:
// *
// *  ''An `Obs`ervable is thus represented as a function from a starting date to a value process.
// The "time-varying" nature of an observable is captured primarily by the value process itself
// (PR a); the Date in the function's type is simply used to specify the start date
// for the resulting value process.''
// *
// * This is true, but we'll follow the approach taken by [[http://netrium.org/ Netrium]]
// * (make `Obs` an GADT.)
// * In order to align processes (`PR[A]`) which are offset in time (think calendar spreads!),
// * ''somewhere'' there has to be a function:
// * {{{
//       f: Instant => PR[A]
//     }}}
// *
// * No examples can be located where processes offset in time are supported;
// * the released Netrium package has the ability to store a single date
// * in the `Model` (and doesn't use that).
// *
// * Will just support constants for now, implementing `Obs` as an ADT but not defining
// * or implementing any Contract execution capabilities, which could change because
// *   - a move to Monadic Contract definition
// *   - distributed ledger enabling (see e.g. `Fae`)
