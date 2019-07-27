package io.deftrade
package money

import eu.timepit.refined
import refined.api.{ Refined, Validate }
import refined.numeric.{ Greater, Less }
import refined.boolean.{ And, Not }
import refined.W

import spire.math.{ Fractional, Integral, Rational }
import spire.implicits._

import cats.kernel.CommutativeGroup
import cats.implicits._

/**
  * A typeclass for number types suitable for financial calculations.
  *
  * The essential affordances:
  * - a `Currency` dependend `round`ing method.
  * - some handy `Refined` types
  *
  * Beyond that, essentially a wrapper around `spire.math.Fractional`.
  *
  * While all `Financial`s are `Fractional`, the reverse is not true.
  * (At least, not true enough for this domain model architect.)
  *
  * TODO: facilities for representing / displaying percentages.
  *
  *  TODO: read up
  *
  *   [["XBR: Precision, Decimals and Units 1.0" http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN-2017-01-11.html]]
  *
  *    > 6.3
  *    > Another related issue is the desire to express the exact value of certain ratios that cannot be exactly represented in a decimal representation. This requirement arises from the Tax domain space. Specific examples from the UK Inland Revenue (now HMRC) are marginal relief rates (varying between 9/400 and 1/40 in the late 1990s) and a special tax rate of 22.5/77.5. This implies the need for the fractionItemType in XBRL (where the numerator and denominator are always exact).
  *
  *    This suggests a `Rational` type e.g. from `spire`.
  *
  * TODO: see also:
  *    > 7.4 Representing Exact Currency Amounts
  *
  */
abstract class Financial[N] private (val fractional: Fractional[N]) {

  type LiterallyZero
  type LiterallyOne

  final type Positive    = N Refined refined.numeric.Positive
  final type NonNegative = N Refined refined.numeric.NonNegative

  final type `(0,1)` = N Refined Is.`(0,1)`
  final type `[0,1)` = N Refined Is.`[0,1)`
  final type `(0,1]` = N Refined Is.`(0,1]`
  final type `[0,1]` = N Refined Is.`[0,1]`

  object Is {
    type `(0,1)` = Greater[LiterallyZero] And Less[LiterallyOne]
    type `[0,1)` = Not[Less[LiterallyZero]] And Less[LiterallyOne]
    type `(0,1]` = Greater[LiterallyZero] And Not[Greater[LiterallyOne]]
    type `[0,1]` = Not[Less[LiterallyZero]] And Not[Greater[LiterallyOne]]

  }

  /**
    * How do we deal with scale and significant digits?
    * Simple rule: the left operand scale is "sticky" for those methods {+, -, *}
    * that return `Money`.
    */
  final def round[C](n: N)(implicit C: Currency[C]): N = {
    def round(bd: BigDecimal): BigDecimal = bd setScale (C.fractionDigits, C.rounding)
    n |> toBigDecimal |> round |> fromBigDecimal
  }

  /** section: `spire.math.Fractional` proxies */
  final def commutativeGroup: CommutativeGroup[N] = fractional.additive

  final def fromBigDecimal(bd: BigDecimal): N = fractional.fromBigDecimal(bd)
  final def toBigDecimal(n: N): BigDecimal    = fractional.toBigDecimal(n)

  final def fromLong(l: Long): N = fractional.fromLong(l)

  final def from[T: Financial](t: T): N = {
    implicit val fractionalT: Fractional[T] = Financial[T].fractional
    t |> fractional.fromType[T]
  }
  final def to[R: Financial](n: N): R = {
    implicit def fractionalR = Financial[R].fractional
    fractional.toType[R](n)
  }

  final def toString(n: N): String = fractional toString n

  def fromString(s: String): N

  object WholeIs {
    implicit def N: Fractional[N] = fractional

    /**
      * Matches when `isWhole`, and round-trips with equality
      *  `[N: Fractional] => [I: Integral] => [N: Fractional]`
      * Client is responsible for choosing a type `I: Integral` that is in range
      * for the parameter `n`.
      */
    def unapply[I: Integral](n: N): Option[I] =
      if (n.isWhole) {
        val i         = N.toType[I](n)
        val roundTrip = Integral[I].toType[N](i)
        if (n === roundTrip) i.some else none
      } else none
  }

  /** Phantom type for `Validate` predicate indicating a whole number (fractional part is zero). */
  sealed abstract case class Whole[I]()

  /** Extractor for whole numbers. */
  object Whole {

    implicit def wholeValidate[I: Integral]: Validate.Plain[N, Whole[I]] = {
      val pred: N => Boolean = {
        case WholeIs(_) => true
        case _          => false
      }
      Validate.fromPredicate(pred, t => s"$t isn't Whole", new Whole[I]() {})
    }
  }
}

/**
  */
object Financial {

  abstract class Aux[N, ZED, UNO](N: Fractional[N]) extends Financial(N) {
    type LiterallyZero = ZED
    type LiterallyOne  = UNO
  }

  /** FIXME: check the `shapeless` examples here - this probably doesn't work */
  def apply[N: Financial]: Financial[N] = implicitly

  implicit lazy val DoubleIsFinancial = new Financial.Aux[
    Double,
    W.`0.0`.T,
    W.`1.0`.T
  ](Fractional[Double]) {
    def fromString(s: String): Double = java.lang.Double parseDouble s
  }

  /** FIXME: test the Aux pattern thing; also: use BigDecimal(1.0).witness ?! */
  implicit lazy val BigDecimalIsFinancial = new Financial(Fractional[BigDecimal]) {
    type LiterallyZero = W.`0.0`.T
    type LiterallyOne  = W.`1.0`.T
    def fromString(s: String): BigDecimal = BigDecimal apply s
  }

  /**
    * TODO: read up on[[XBR: Precision, Decimals and Units 1.0 http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN-017-01-11.html]]
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
  implicit lazy val RationalIsFinancial = new Financial(Fractional[Rational]) {
    type LiterallyZero = W.`0.0`.T
    type LiterallyOne  = W.`1.0`.T
    def fromString(s: String): Rational = Rational apply s
  }
}
