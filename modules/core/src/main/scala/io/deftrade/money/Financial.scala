package io.deftrade
package money

import eu.timepit.refined
import refined.api.{ Refined, Validate }
import refined.numeric.{ Greater, Less }
import refined.boolean.{ And, Not }

import spire.math.{ Fractional, Integral, Rational }
import spire.implicits._

import cats.kernel.CommutativeGroup
import cats.implicits._

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
abstract class Financial[N] private (val fractional: Fractional[N]) {

  final type Positive    = N Refined refined.numeric.Positive
  final type NonNegative = N Refined refined.numeric.NonNegative

  final type `(0,1)` = N Refined r9s.`(0,1)`
  final type `[0,1)` = N Refined r9s.`[0,1)`
  final type `(0,1]` = N Refined r9s.`(0,1]`
  final type `[0,1]` = N Refined r9s.`[0,1]`

  object r9s {
    type `(0,1)` = Greater[LiterallyZero] And Less[LiterallyOne]
    type `[0,1)` = Not[Less[LiterallyZero]] And Less[LiterallyOne]
    type `(0,1]` = Greater[LiterallyZero] And Not[Greater[LiterallyOne]]
    type `[0,1]` = Not[Less[LiterallyZero]] And Not[Greater[LiterallyOne]]

  }

  type LiterallyZero
  type LiterallyOne

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

  final case class Whole[I]() // for validation FIXME finish this

  /** Extractor for whole numbers. */
  object Whole {

    implicit def wholeValidate[I: Integral]: Validate.Plain[N, Whole[I]] = {
      val pred: N => Boolean = {
        case WholeIs(_) => true
        case _          => false
      }
      Validate.fromPredicate(pred, t => s"$t isn't Whole", Whole[I]())
    }
  }
}

/**
  */
object Financial {
  import refined.W

  def apply[N: Financial]: Financial[N] = implicitly

  // def fromFractional[N](N: Fractional[N]): Financial[N] = new Financial(N)

  // type ZeroToOne[N: Fractional] = {
  //   val N = Fractional[N]
  //   import N._
  //   Not[Less[zero]] And Not[Greater[one]]
  // }
  // implicit def refinedValidate[N: Financial, P]: Validate[N, P] = ???

  implicit lazy val DoubleIsFinancial = new Financial(Fractional[Double]) {
    type LiterallyZero = W.`0.0`.T
    type LiterallyOne  = W.`1.0`.T
  }

  implicit lazy val BigDecimalIsFinancial = new Financial(Fractional[BigDecimal]) {
    type LiterallyZero = W.`0.0`.T
    type LiterallyOne  = W.`1.0`.T
  }

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
  implicit lazy val RationalIsFinancial = new Financial(Fractional[Rational]) {
    type LiterallyZero = W.`0.0`.T
    type LiterallyOne  = W.`1.0`.T
  }
}
