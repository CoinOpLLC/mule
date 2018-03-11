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
package wip

import enumeratum._
// import enumeratum.values._

//
// import eu.timepit.refined
// import refined.api.Refined
// import refined.W
import cats.kernel.{ instances, CommutativeGroup, Monoid, Order }, instances.{ BigDecimalGroup, DoubleGroup }
import cats.Show
// import refined.collection._
// import refined.numeric._
// import refined.auto._

/**

   requirements for currency / money:
   - scalazzi-9000-complient™, cats-friendly™
   - take _inspiration_ from `squants.market` (emphasis _mine_)
   - comprehend the best of other FOSS offerings (esp the moribund ones!)
   -  these chaps https://github.com/Appendium/objectlabkit
   -  but lose the whole "is market convention" thing - yagni
   - separate types for each currency (no addition across currencies! domain modeling dictates!)
   - minimal to no overhead (value classes on [N: Financial])
    - can summon implicit Monetary[Currency] typeclass instance given a currency instance
   - use `enumeratum` lib for denominations (supported currencies)
   - exploit java Currency support
   - abstract over currencies for single implicit `cats.Monoid` function
   - monetary enum as factory pattern - can use to make currency
    (Turns out, this runs into limits to abstraction.)
   - no dependencies other than `typelevel algebra`
   - use the currency conversion / cross conventions typical of financial market participants.
   (e.g. banks, and long/short credit hedge funds, which aren't much different)

   todo: treatment of what's expected to be imported implicitly

  */
/**

financially useful big decimal scaling utilities
notes from reviewing objectlabkit

val DefaultInverseScale = 20
val OneScaled = ONE setScale DefaultInverseScale

  * set to `scale`, using `rounding`
  * `divide` and `inverse` using `scale` and `rounding`

  * `calculateWeight` -> ?! magic scale of 9? wtf

  * `roundToScaleX andThen roundToScaleY` 🤔 (why HALF_UP baked in?)
  *

  general strategy on division: set scale to 20, then set it back to what makes sense for result
  on conversion: we take the scale and rounding of the result currency

  */
/**
  * - `PricedIn` is a binary typeclass: `A PricedIn B`. The domain vocabulary usage supports this.
  * - can come from a variety of sources including live market
  * - "Orderly market" invariant: `ask` < `bid`.
  * - must model disorderly markets: not everything that comes at you down the wire makes sense.
  * - note: the types parameters `A` and `B` are effectively *phantom types*
  *   - used summon a pricing instance (e.g. PricedIn[SomeShadySpeculativeInstrument, CHF]))
  */
trait PricedIn[A, B] {

  def ask: BigDecimal
  def bid: BigDecimal

  def tick: BigDecimal

  @inline final def spread = ask - bid
  @inline final def mid    = bid + spread / 2

}

sealed trait Financial[N] extends Fractional[N] with Ordering[N] with CommutativeGroup[N] {
  import BigDecimal.RoundingMode, RoundingMode.RoundingMode
  def fromBigDecimal(bd: BigDecimal): N
  def toBigDecimal(n: N): BigDecimal = BigDecimal(n.toString)

  /**
    * Someday, maybe, "as if by" will be the operative words.
    * But today, we go with "literally".
    */
  final def round(n: N, scale: Int, rounding: RoundingMode): N =
    toBigDecimal(n).setScale(scale, rounding) |> fromBigDecimal
}
object Financial {

  import Numeric.{ BigDecimalIsFractional, DoubleIsFractional }
  import Ordering.{ BigDecimalOrdering, DoubleOrdering }

  def apply[N: Financial]: Financial[N] = implicitly

  class DoubleIsFinancial extends DoubleGroup with Financial[Double] with DoubleIsFractional with DoubleOrdering {
    def fromBigDecimal(bd: BigDecimal): Double = bd.toDouble
  }
  implicit object DoubleIsFinancial extends DoubleIsFinancial

  /**
    * left operand scale is "sticky" for those operations {+, -, *} that
    * ultimately result in Money.
    */
  class BigDecimalIsFinancial extends BigDecimalGroup with Financial[BigDecimal] with BigDecimalIsFractional with BigDecimalOrdering {

    override def plus(x: BigDecimal, y: BigDecimal): BigDecimal =
      super.plus(x, y).setScale(x.scale)

    override def minus(x: BigDecimal, y: BigDecimal): BigDecimal =
      super.minus(x, y).setScale(x.scale)

    override def times(x: BigDecimal, y: BigDecimal): BigDecimal =
      super.times(x, y).setScale(x.scale)

    def fromBigDecimal(bd: BigDecimal): BigDecimal       = bd
    override def toBigDecimal(n: BigDecimal): BigDecimal = n
  }
  implicit object BigDecimalIsFinancial extends BigDecimalIsFinancial
}

/** Hobbson's polymorphism ftm lol */
private[wip] sealed trait MonetaryLike extends EnumEntry { monetary =>

  // type CurrencyType <: Currency
  // def apply[N: Financial](n: N): Money[N, CurrencyType]
  final private lazy val jc = java.util.Currency getInstance monetary.entryName

  final def currencyCode: String = jc.getCurrencyCode
  final def numericCode: Int     = jc.getNumericCode
  final def defaultFractionDigits: Int = jc.getDefaultFractionDigits match {
    case -1 => 4 // typical use case is unit-of-account; just need decent default
    case fd => fd
  }
  final def pipScale: Int       = defaultFractionDigits + 2
  final def pip: BigDecimal     = BigDecimal(0L, scale = pipScale).ulp // idiom for the idiom gods
  final def displayName: String = jc.getDisplayName
  final def symbol: String      = jc.getSymbol

  final def rounding: BigDecimal.RoundingMode.RoundingMode = monetary.entryName match {
    case "JPY" => BigDecimal.RoundingMode.DOWN
    case _     => BigDecimal.RoundingMode.HALF_UP
  }

}

object PhantomTypePerCurrency {
  // design 1: phantom types
  // problem: toString (only one java class serves as a box - the restriction is entirely a compile time scala typesystem thing.
  // Which is fine, as far as it goes, which isn't far enough to interoperate with other sw that expects toString to give distinct output for different currencies.)
  // But we can implement Show[Money[N, C]] for all the N and C we care about.

  object Monetary extends Enum[MonetaryLike] {

    final class Money[N, C] private[Money] (val amount: N) extends AnyVal {

      type MNY = Money[N, C]

      private def mny(n: N): MNY = new Money[N, C](n)

      def +(rhs: MNY)(implicit N: Financial[N])    = mny(N plus (amount, rhs.amount))
      def -(rhs: MNY)(implicit N: Financial[N])    = mny(N minus (amount, rhs.amount))
      def *(scale: N)(implicit N: Financial[N])    = mny(N times (scale, amount))
      def /(rhs: MNY)(implicit N: Financial[N]): N = N div (amount, rhs.amount)
      def unary_-(implicit N: Financial[N]): MNY   = mny(N negate amount)
    }
    object Money {
      def apply[N: Financial, C: Monetary](amount: N) = new Money[N, C](amount) // .round
    }
    import cats.kernel.{ Monoid, Order }

    implicit def orderN[N: Financial]: Order[N]                          = Order.fromOrdering[N]
    implicit def orderMoney[N: Order, C <: Currency]: Order[Money[N, C]] = Order by (_.amount)

    implicit def monoid[N: Financial, C: Monetary] = new Monoid[Money[N, C]] {
      type MNY = Money[N, C]
      override def combine(a: MNY, b: MNY): MNY = a + b
      override def empty: MNY                   = Money(Financial[N].zero)
    }

    implicit def showMoney[N: Financial, C: Monetary]: Show[Money[N, C]] =
      Show show (Format apply _)

    object Format {
      val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative
      def apply[N: Financial, C: Monetary](m: Money[N, C]): String = {
        val MC   = Monetary[C]
        val fmt  = s"%${flags}.${MC.defaultFractionDigits}f"
        val sfmt = if (Financial[N].signum(m.amount) < 0) fmt else s" $fmt "
        s"${MC.currencyCode} ${m.amount formatted sfmt}"
      }
    }

    /**
      * Domain consideration: `Currency` _exchange depends on _pricing_ of some kind.
      * One or more Market(s) determine this price.
      * - A design that doesnt' abstract over `PricedIn`, **including live data**, is useless.
      * - otoh dead simple immutable for testing / demo also required
      * Three letter codes: 26 ^ 3 = 17576
      * over two hundred assigned; several "dead" currencies (not reused)
      * Market convention: `ABC/XYZ`: buy or sell units of `ABC`, priced in `XYZ`.
      */
    final case class Cross[CA, CB]()(implicit
                                     MA: Monetary[CA],
                                     MB: Monetary[CB],
                                     P: PricedIn[CA, CB]) {
      import P._

      def buy[N](ma: Money[N, CA])(implicit N: Financial[N]): Money[N, CB] =
        MB(N.fromBigDecimal(N.toBigDecimal(ma.amount) * ask))

      def sell[N](ma: Money[N, CA])(implicit N: Financial[N]): Money[N, CB] =
        MB(N.fromBigDecimal(N.toBigDecimal(ma.amount) * bid))

      def apply[N](ma: Money[N, CA])(implicit N: Financial[N]): Money[N, CB] =
        MB(N.fromBigDecimal(N.toBigDecimal(ma.amount) * mid))

      def quote[N](implicit N: Financial[N]): (Money[N, CB], Money[N, CB]) =
        (buy(MA(N.one)), sell(MA(N.one)))

      def cross: Option[Monetary[_ <: Currency]] = ???

      def description: String = s"""
          |Quoter buys  ${MA} and sells ${MB} at ${bid}
          |Quoter sells ${MA} and buys  ${MB} at ${ask}""".stripMargin
    }

    final case class SimplePricing[CA, CB: Monetary](val ask: BigDecimal, val bid: BigDecimal) extends PricedIn[CA, CB] {
      def tick: BigDecimal = Monetary[CB].pip
    }

    final case class LiveMarketPricing[CA, CB: Monetary](cfg: String) extends PricedIn[CA, CB] {
      def bid: BigDecimal  = ???
      def ask: BigDecimal  = ???
      def tick: BigDecimal = Monetary[CB].pip / 10 // this is a thing now
    }

    // final case class FakeMarketPricing[N, CA <: Currency, CB <: Currency](
    //     quotes: (N, N)*
    // )(
    //     implicit MA: Monetary[CA],
    //     MB: Monetary[CB],
    //     N: Financial[N]
    // ) {
    //   import N._
    //   var qs                 = quotes.toList
    //   var (ask, bid): (N, N) = (zero, zero)
    //   def tick = {
    //     bid = qs.head._1
    //     ask = qs.head._2
    //     qs = qs.tail
    //   }
    //   def buy(ma: Money[N, CA]): Money[N, CB]  = { tick; MB(ma.amount * ask) }
    //   def sell(ma: Money[N, CA]): Money[N, CB] = { tick; MB(ma.amount * bid) }
    //   def quote: (Money[N, CB], Money[N, CB])  = { tick; (buy(MA(one)), sell(MA(one))) }
    // }

    sealed trait Monetary[C] extends MonetaryLike { self =>

      // intent: grant to `Monetary[C]` exclusive license to create `Money[N, C]` instances.
      private[this] implicit val C: Monetary[C] = self // this is all regrettably clever
      def apply[N: Financial](n: N)             = Money apply [N, C] n

      /**
        * implicit context to provide pricing
        */
      def /[CB](cb: Monetary[CB])(implicit P: C PricedIn CB): Cross[C, CB] = {
        implicit val CB = cb
        Cross[C, CB]
      }
    }

    /**
      * FIXME: Not sure that this has a use, actually.
      */
    sealed trait Currency

    /**
      * Given a currency (phantom) type, get a `Monetary` instance.
      */
    def apply[C: Monetary]: Monetary[C] = implicitly

    // TODO: standard major currency order - can we find a reference other than objectlabkit?
    // "EUR", "GBP", "AUD", "NZD", "USD", "CAD", "CHF", "NOK", "SEK", "JPY"

    val values = findValues

    sealed trait EUR extends Currency
    case object EUR  extends Monetary[EUR]
    implicit val eur: Monetary[EUR] = EUR

    sealed trait USD extends Currency
    case object USD  extends Monetary[USD]
    implicit val usd: Monetary[USD] = USD
  }
}

// design 2:
// make each Monetary a separate case class
// gets hung up on `import Monetary._`, which imports all the verboten `apply` instances
// without the necessary `Financial` context bound on `N`. And so overloading can't resolve.
// -> and so devolves back to the same need for a Show[Currency] class as phantom type
// (design 1)
object ClassPerCurrency {

  import scala.language.higherKinds

  object Monetary extends Enum[MonetaryLike] {

    sealed trait Currency[N] extends Any {

      def amount: N

      final def currencyCode: String = { // principle of least ghastliness
        val cs = getClass.toString
        cs substring (cs lastIndexOf '$') + 1
      }
      final override def toString: String = s"$currencyCode(${amount.toString})"
    }

    implicit class CurrencyOps[C[?] <: Currency[?]: Monetary, N: Financial](lhs: C[N]) {
      def N: Financial[N]    = implicitly
      def MC: Monetary[C]    = implicitly
      def +(rhs: C[N]): C[N] = MC apply (N plus (lhs.amount, rhs.amount))
      def -(rhs: C[N]): C[N] = MC apply (N minus (lhs.amount, rhs.amount))
      def /(rhs: C[N]): N    = N div (lhs.amount, rhs.amount)
      def *(scale: N): C[N]  = MC apply (N times (lhs.amount, scale))
      def unary_- : C[N]     = MC apply (N negate lhs.amount)
    }
    implicit class CurrencyScaleOps[N: Financial](scale: N) {
      def N: Financial[N] = implicitly
      def *[C[?] <: Currency[?]: Monetary](rhs: C[N]): C[N] =
        Monetary[C] apply (N times (scale, rhs.amount))
    }

    trait Cross[Cn[?] <: Currency[?], Cd[?] <: Currency[?]]

    sealed trait Monetary[C[?] <: Currency[?]] extends MonetaryLike { self =>

      def apply[N: Financial](n: N): C[N]

      final def /[C2[?] <: Currency[?]](base: Monetary[C2]): Cross[C, C2] =
        new Cross[C, C2] {}
    }

    implicit def showMoney[C[?] <: Currency[?]: Monetary, N: Financial]: Show[C[N]] =
      Show show (Format apply _)

    implicit def orderN[N: Financial] = Order.fromOrdering[N]
    implicit def orderMoney[N: Order, C[?] <: Currency[?]: Monetary]: Order[C[N]] =
      Order by (_.amount)

    implicit def monoid[N: Financial, C[?] <: Currency[?]: Monetary] = new Monoid[C[N]] {
      val N: Financial[N]                          = implicitly
      val MC: Monetary[C]                          = implicitly
      override def combine(a: C[N], b: C[N]): C[N] = MC apply (N plus (a.amount, b.amount))
      override lazy val empty: C[N]                = MC apply (N.zero)
    }
    object Format {

      val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative

      def apply[N: Financial, C[?] <: Currency[?]: Monetary](m: C[N]): String = {
        val MC   = Monetary[C]
        val fmt  = s"%${flags}.${MC.defaultFractionDigits}f"
        val sfmt = if (Financial[N].signum(m.amount) < 0) fmt else s" $fmt "
        s"${MC.currencyCode} ${m.amount formatted sfmt}"
      }
    }
    def values = findValues

    def apply[C[?] <: Currency[?]: Monetary]: Monetary[C] = implicitly

    final class USD[N] private (val amount: N) extends AnyVal with Currency[N]
    case object USD extends Monetary[USD] {
      def apply[N: Financial](n: N) = new USD(n)
    }
    implicit val usd: Monetary[USD] = USD

    final class EUR[N] private (val amount: N) extends AnyVal with Currency[N]
    case object EUR extends Monetary[EUR] {
      def apply[N: Financial](n: N) = new EUR(n)
    }
    implicit val eur: Monetary[EUR] = EUR

  }
}

/*

# [XBR: Precision, Decimals and Units 1.0](http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN-2017-01-11.html)

> 6.3
...Another related issue is the desire to express the exact value of certain ratios that cannot be exactly represented in a decimal representation. This requirement arises from the Tax domain space. Specific examples from the UK Inland Revenue (now HMRC) are marginal relief rates (varying between 9/400 and 1/40 in the late 1990s) and a special tax rate of 22.5/77.5. This implies the need for the fractionItemType in XBRL (where the numerator and denominator are always exact).

This suggests a Rational type e.g. from spire

7.4 Representing Exact Monetary Amounts


 */

// private[wip] object UglyTypeDefs {
//   type Code = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
// }
