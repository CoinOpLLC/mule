package io.deftrade
package money

import eu.timepit.refined
import refined.api.{ Refined, Validate }
import refined.numeric.{ Greater, Less }
import refined.boolean.{ And, Not }
import refined.W

import spire.implicits._
import spire.math.{ Fractional, Integral, Rational }

import cats.implicits._
import cats.kernel.CommutativeGroup

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
  * TODO: read up on [[http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN-2017-01-11.html XBR: Precision, Decimals and Units 1.0]]
  *
  *    > 6.3
  *    > Another related issue is the desire to express the exact value of certain ratios that cannot be exactly represented in a decimal representation. This requirement arises from the Tax domain space. Specific examples from the UK Inland Revenue (now HMRC) are marginal relief rates (varying between 9/400 and 1/40 in the late 1990s) and a special tax rate of 22.5/77.5. This implies the need for the fractionItemType in XBRL (where the numerator and denominator are always exact).
  *
  *    This suggests a `Rational` type e.g. from `spire`.
  *
  * TODO: see also:
  *    > 7.4 Representing Exact Currency Amounts
  */
trait Financial[N] extends Fractional[N] { self =>

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

  /** section: `spire.math.Fractional` aliases */
  final def commutativeGroup: CommutativeGroup[N] = additive

  final def from[T: Financial](t: T): N = fromType[T](t)(Financial[T])

  final def to[R: Financial](n: N): R = toType[R](n)(Financial[R])

  def scan(s: String): Result[N]

  object IsWhole {

    implicit lazy val N: Fractional[N] = self

    /**
      * Matches when `isWhole`, and round-trips with equality
      *  `[N: Fractional] => [I: Integral] => [N: Fractional]`
      * Client is responsible for choosing a type `I: Integral` that is in range
      * for the parameter `n`.
      */
    def unapply[I: Integral](n: N): Option[I] =
      if (isWhole(n)) {
        val i         = toType[I](n)
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
        case IsWhole(_) => true
        case _          => false
      }
      Validate.fromPredicate(pred, t => s"$t isn't Whole", new Whole[I]() {})
    }
  }
}

/**
  */
object Financial {

  /**  */
  def apply[N](implicit N: Financial[N]): Financial[N] = N

  /**  */
  trait DoubleIsFinancial extends spire.math.DoubleIsFractionalHack with Financial[Double] {
    type LiterallyZero = W.`0.0`.T
    type LiterallyOne  = W.`1.0`.T
    def scan(s: String) = Result { java.lang.Double parseDouble s }
  }

  /**  */
  implicit object DoubleIsFinancial extends DoubleIsFinancial

  /** FIXME: use BigDecimal(1.0).witness ?! */
  trait BigDecimalIsFinancial extends spire.math.BigDecimalIsFractionalHack with Financial[BigDecimal] {
    type LiterallyZero = W.`0.0`.T
    type LiterallyOne  = W.`1.0`.T
    def scan(s: String) = Result { BigDecimal apply s }
  }

  /**  */
  implicit object BigDecimalIsFinancial extends BigDecimalIsFinancial

  /**
    * TODO: read up on[[http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN-017-01-11.html XBR: Precision, Decimals and Units 1.0]]
    *
    * === 6.3 ===
    * .Another related issue is the desire to express the exact value of certain ratios that
    * cannot be exactly represented in a decimal representation. This requirement arises from
    * the Tax domain space. Specific examples from the UK Inland Revenue (now HMRC) are marginal
    * relief rates (varying between 9/400 and 1/40 in the late 1990s) and a special tax rate of
    * 22.5/77.5. This implies the need for the fractionItemType in XBRL (where the numerator and
    * denominator are always exact).

    * Also:
    * > 7.4 Representing Exact Currency Amounts
    */
  trait RationalIsFinancial extends spire.math.RationalIsFractionalHack with Financial[Rational] {
    type LiterallyZero = W.`0.0`.T
    type LiterallyOne  = W.`1.0`.T
    def scan(s: String) = Result { Rational apply s }
  }

  /**  */
  implicit object RationalIsFinancial extends RationalIsFinancial
}
// trait FinancialEntailsFractional {
//   protected final implicit def financialEntailsFractional[V: Financial]: Fractional[V] =
//     Financial[V].fractional
// }
