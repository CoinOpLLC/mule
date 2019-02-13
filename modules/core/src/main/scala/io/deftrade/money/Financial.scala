package io.deftrade
package money

import spire.math.{ Fractional, Integral, Rational }

import cats.kernel.CommutativeGroup

/**
  * A typeclass for number types suitable for financial calculations.
  *
  * The essential affordance is a `Currency` dependend `round`ing method.
  *
  * Beyond that, essentially a wrapper around `spire.math.Fractional`.
  *
  * While all `Financial`s are `Fractional`, the reverse is not true.
  * (At least, not true enough for this domain model architect.)
  */
class Financial[N] private (val fractional: Fractional[N]) {

  /**
    * How do we deal with scale and significant digits?
    * Simple rule: the left operand scale is "sticky" for those methods {+, -, *}
    * that return `Money`.
    */
  def round[C](n: N)(implicit C: Currency[C]): N = {
    def round(bd: BigDecimal): BigDecimal = bd setScale (C.fractionDigits, C.rounding)
    n |> toBigDecimal |> round |> fromBigDecimal
  }

  /** section: `spire.math.Fractional` proxies */
  def commutativeGroup: CommutativeGroup[N] = fractional.additive

  def fromBigDecimal(bd: BigDecimal): N = fractional.fromBigDecimal(bd)
  def toBigDecimal(n: N): BigDecimal    = fractional.toBigDecimal(n)

  def fromLong(l: Long): N = fractional.fromLong(l)

  def from[T: Financial](t: T): N = t |> fractional.fromType[T]
  def to[R: Financial](n: N): R   = n |> fractional.toType[R]

  /** Extractors and (any) other tools for dealing with integral quantities. */
  object IntegralIs {

    // TODO:
    def unapply[I](n: N)(implicit I: Integral[N]): Option[N] = ???
  }
}

/**
  */
object Financial {

  def apply[N: Financial]: Financial[N] = implicitly

  def fromFractional[N](N: Fractional[N]): Financial[N] = new Financial(N)

  // type ZeroToOne[N: Fractional] = {
  //   val N = Fractional[N]
  //   import N._
  //   Not[Less[zero]] And Not[Greater[one]]
  // }
  // implicit def refinedValidate[N: Financial, P]: Validate[N, P] = ???

  implicit lazy val DoubleIsFinancial = Financial fromFractional Fractional[Double]

  implicit lazy val BigDecimalIsFinancial = Financial fromFractional Fractional[BigDecimal]

  /**
    * TODO: read up
    *
    * # [XBR: Precision, Decimals and Units 1.0]
    * http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN- 017-01-11.html)
    *
    * > 6.3
    * ..Another related issue is the desire to express the exact value of certain ratios that
    * cannot be exactly represented in a decimal representation. This requirement arises from
    * the Tax domain space. Specific examples from the UK Inland Revenue (now HMRC) are marginal
    * relief rates (varying between 9/400 and 1/40 in the late 1990s) and a special tax rate of
    * 22.5/77.5. This implies the need for the fractionItemType in XBRL (where the numerator and
    * denominator are always exact).

    * Also:
    * > 7.4 Representing Exact Currency Amounts
    */
  implicit lazy val RationalIsFinancial = Financial fromFractional Fractional[Rational]
}
