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

  /** `const(x)` is an observable that has value x at any time. */
  def const[A](a: A): Obs[A] = new Const(a) {}
  sealed abstract case class Const[A](a: A) extends Obs[A]

  /**
    * FIXME: van Straaten uses Obs[Date] and Netrium does not.
    * This is my best attempt at elucidation. What does this even do? How does
    * Netrium cope?
    */
  def at(t: Instant): Obs[Boolean] = ???

  /** */
  implicit def obsOrder[A: Order]: Order[Obs[A]] = ???

  /** */
  implicit def obsShow[A]: Show[Obs[A]] = ???

  /** */
  implicit def obsFractional[N: Fractional]: Fractional[Obs[N]] = ???

  /** */
  implicit class Ops[A](val o: Obs[A]) {

    /** */
    def branch(cTrue: Contract, cFalse: Contract)(implicit toBool: A =:= Boolean): Contract =
      Contract branch (toBool liftCo o, cTrue, cFalse)
  }
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
