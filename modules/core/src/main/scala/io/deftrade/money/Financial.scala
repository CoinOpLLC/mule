/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.deftrade
package money

import syntax._

import cats.implicits._
import cats.kernel.{ CommutativeGroup, Monoid, Semigroup }

import eu.timepit.refined
import refined.api.{ Refined, Validate }
import refined.numeric._
import refined.cats._

import spire.implicits._
import spire.math.{ Fractional, Integral, Rational }

/**
  * Witnesses that `N` is a numerical type suitable for financial calculations, and provides
  * an abstract interface based on [[spire.math.Fractional]].
  *
  * Additional affordances:
  *   - a `Currency` dependend `round`ing method.
  *   - some handy `Refined` types
  *
  * While all `Financial`s are `Fractional`, the reverse is not true.
  * (At least, not true enough for this domain model architect.)
  *
  * TODO: facilities for representing / displaying percentages a la hp12c.
  *
  * TODO: read up on [[http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN-2017-01-11.html XBR: Precision, Decimals and Units 1.0]]
  *
  * > 6.3
  * Another related issue is the desire to express the exact value of certain ratios that
  * cannot be exactly represented in a decimal representation. This requirement arises from
  * the Tax domain space. Specific examples from the UK Inland Revenue (now HMRC) are marginal
  * relief rates (varying between 9/400 and 1/40 in the late 1990s) and a special tax rate of
  * 22.5/77.5. This implies the need for the fractionItemType in XBRL (where the numerator and
  * denominator are always exact).
  *
  * Also:
  * > 7.4 Representing Exact Currency Amounts
  *
  *    This motivates inclusion of the `Rational` type from `spire` among the specializations
  * provided by `Financial` implicit instances.
  */
trait Financial[N] extends Fractional[N] { self =>

  /**
    */
  implicit def positiveSemigroup: Semigroup[N Refined Positive]

  /**
    */
  implicit def nonNegativeMonoid: Monoid[N Refined NonNegative]

  /**
    * How do we deal with scale and significant digits?
    * Simple rule: the left operand scale is "sticky" for those methods {+, -, *}
    * that return `Mny`.
    *
    * TODO: Revisit this.
    */
  final def round[C](n: N)(implicit C: Currency[C]): N = {
    def round(bd: BigDecimal): BigDecimal = bd.setScale(C.fractionDigits, C.rounding)
    n |> toBigDecimal |> round |> fromBigDecimal
  }

  /** section: `spire.math.Fractional` aliases */
  final def commutativeGroup: CommutativeGroup[N] = additive

  /**
    */
  final def from[T: Financial](t: T): N = fromType[T](t)(Financial[T])

  /**
    */
  final def to[R: Financial](n: N): R = toType[R](n)(Financial[R])

  /**
    */
  def parse(s: String): Result[N]

  /** FIXME test */
  final type WholeInt = N Refined IsWhole[Int]

  /** Phantom type for `Validate` predicate indicating a whole number (fractional part is zero). */
  sealed abstract class IsWhole[I] private ()

  /**
    */
  object IsWhole {

    /**
      */
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

    /**
      */
    private def apply[I]: IsWhole[I] = new IsWhole[I]() {}

    /**
      */
    implicit def wholeValidate[I: Integral]: Validate.Plain[N, IsWhole[I]] = {
      val p: N => Boolean = n => IsWhole.unapply[I](n).fold(false)(_ => true)
      Validate.fromPredicate(p, t => s"${t.toString} isn't a whole number", IsWhole[I])
    }
  }
}

/**
  */
object Financial {

  implicit class Ops[X](val x: X) extends AnyVal {
    def to[Y: Financial](implicit X: Financial[X]) = X.to[Y](x)
  }

  /**
    */
  def apply[N](implicit N: Financial[N]): Financial[N] = N

  implicit object DoubleIsFinancial extends spire.math.DoubleIsFractionalHack with Financial[Double] {

    /**
      */ // odd that these can be declared both not implicit, and override... ;?
    override def nonNegativeMonoid = Monoid[Double Refined NonNegative]

    /**
      */
    override def positiveSemigroup = Semigroup[Double Refined Positive]

    /**
      */
    def parse(s: String) = Result safe [Double] { java.lang.Double parseDouble s }
  }

  /**
    */
  implicit object BigDecimalIsFinancial extends spire.math.BigDecimalIsFractionalHack with Financial[BigDecimal] {

    /**
      */
    def nonNegativeMonoid: Monoid[BigDecimal Refined NonNegative] = ??? // Monoid[BigDecimal Refined NonNegative]

    /**
      */
    def positiveSemigroup: Semigroup[BigDecimal Refined Positive] = ??? // Semigroup[BigDecimal Refined Positive]

    /**
      */
    def parse(s: String) = Result safe { BigDecimal apply s }
  }

  /**
    */
  sealed abstract class RationalIsFinancial extends spire.math.RationalIsFractionalHack with Financial[Rational] {

    /**
      */
    def nonNegativeMonoid: Monoid[Rational Refined NonNegative] = ??? // Monoid[Rational Refined NonNegative]

    /**
      */
    def positiveSemigroup: Semigroup[Rational Refined Positive] = ??? //Semigroup[Rational Refined Positive]

    def parse(s: String) = Result safe { Rational apply s }
  }

  /**
    */
  implicit object RationalIsFinancial extends RationalIsFinancial
}
