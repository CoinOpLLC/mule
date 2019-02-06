package io.deftrade
package money

import eu.timepit.refined
import refined.api.Refined

import spire.math.{ Fractional, Integral }

import enumeratum._

import cats.kernel.{ CommutativeGroup, Order }
import cats.{ Invariant, Show }

import BigDecimal.RoundingMode._

/** */
class Financial[N] private (implicit val fractional: Fractional[N]) {

  def commutativeGroup: CommutativeGroup[N] = fractional.additive

  def fromBigDecimal(bd: BigDecimal): N = fractional.fromBigDecimal(bd)
  def toBigDecimal(n: N): BigDecimal    = fractional.toBigDecimal(n)

  def fromLong(l: Long): N = fractional.fromLong(l)

  /**
    * Someday, maybe, "as if by" will be the operative words.
    * But today, we go with "literally".
    */
  def from[T: Financial](t: T): N = t |> Financial[T].toBigDecimal |> fromBigDecimal
  def to[R: Financial](n: N): R   = n |> toBigDecimal              |> Financial[R].fromBigDecimal

  /**
    * How do we deal with scale and significant digits?
    * Simple rule: the left operand scale is "sticky" for those operations {+, -, *} that
    * ultimately result in Money.
    */
  def round[C](n: N)(implicit C: Currency[C]): N = {
    def round(bd: BigDecimal): BigDecimal = bd setScale (C.fractionDigits, C.rounding)
    n |> toBigDecimal |> round |> fromBigDecimal
  }

  /** Extractors and (any) other tools for dealing with integral quantities. */
  object IntegralIs {

    // TODO:
    def unapply[I](n: N)(implicit I: Integral[N]): Option[N] = ???
  }
}

/**
  */
object Financial {

  import Fractional._

  def apply[N: Financial]: Financial[N] = implicitly

  def derive[N: Fractional]: Financial[N] = new Financial

  implicit lazy val DoubleIsFinancial: Financial[Double] = derive

  implicit lazy val BigDecimalIsFinancial: Financial[BigDecimal] = derive

}

/*
TODO: read up

  # [XBR: Precision, Decimals and Units 1.0](http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN-2017-01-11.html)

  > 6.3
  ...Another related issue is the desire to express the exact value of certain ratios that cannot be
exactly represented in a decimal representation. This requirement arises from the Tax domain space.
Specific examples from the UK Inland Revenue (now HMRC) are marginal relief rates (varying between
9/400 and 1/40 in the late 1990s) and a special tax rate of 22.5/77.5. This implies the need for the
fractionItemType in XBRL (where the numerator and denominator are always exact).

  This suggests a Rational type e.g. from spire

  > 7.4 Representing Exact Currency Amounts
 */
