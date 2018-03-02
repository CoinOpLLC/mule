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
import cats.kernel.{ Monoid, Order }
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
   - minimal to no overhead (value classes on [N: Fractional])
    - can summon implicit Monetary[Currency] typeclass instance given a currency instance
   - use `enumeratum` lib for denominations (supported currencies)
   - exploit java Currency support
   - abstract over currencies for single implicit `cats.Monoid` function
   - monetary enum as factory pattern - can use to make currency
    (Turns out, this runs into limits to abstraction.)
   - no dependencies other than `typelevel algebra`

   todo: treatment of what's expected to be imported implicitly

  */
private[wip] sealed trait MonetaryLike extends EnumEntry { monetary =>

  // type CurrencyType <: Currency
  // def apply[N: Fractional](n: N): Money[N, CurrencyType]
  final private lazy val jc = java.util.Currency getInstance monetary.entryName

  final def currencyCode: String       = jc.getCurrencyCode
  final def numericCode: Int           = jc.getNumericCode
  final def defaultFractionDigits: Int = jc.getDefaultFractionDigits
  final def displayName: String        = jc.getDisplayName
  final def symbol: String             = jc.getSymbol

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

    sealed trait Currency

    final case class Money[N, C <: Currency] private[Money] (val amount: N) extends AnyVal {

      type MNY = Money[N, C]

      def +(rhs: MNY)(implicit N: Fractional[N]): MNY = Money(N plus (amount, rhs.amount))
      def -(rhs: MNY)(implicit N: Fractional[N]): MNY = Money(N minus (amount, rhs.amount))
      def *(scale: N)(implicit N: Fractional[N]): MNY = Money(N times (scale, amount))
      def /(rhs: MNY)(implicit N: Fractional[N]): N   = N div (amount, rhs.amount)
      // def /(rhs: N)(implicit N: Fractional[N]): MNY   = Money(N div (amount, rhs))
      def unary_-(implicit N: Fractional[N]): MNY = Money(N negate amount)
    }

    /**
      * - `Pricing` can come from a variety of sources.
      * - "Orderly market" invariant: `ask` < `bid`.
      * - must model disorderly markets: not everything that comes at you down the wire makes sense.
      */
    abstract class Pricing[N, CA <: Currency, CB <: Currency](implicit N: Fractional[N]) {

      def ask: N
      def bid: N

      import N._

      final val half = one / (one + one) // FIXME: must be a better way

      @inline final def spread: N = ask - bid
      @inline final def mid: N    = bid + half * spread

    }

    import cats.kernel.{ Monoid, Order }

    implicit def orderN[N: Fractional]: Order[N]                         = Order.fromOrdering[N]
    implicit def orderMoney[N: Order, C <: Currency]: Order[Money[N, C]] = Order by (_.amount)

    implicit def monoid[N: Fractional, C <: Currency] = new Monoid[Money[N, C]] {
      type MNY = Money[N, C]
      override def combine(a: MNY, b: MNY): MNY = a + b
      override def empty: MNY                   = Money(implicitly[Fractional[N]].zero)
    }

    implicit def showMoney[C <: Currency: Monetary, N: Fractional]: Show[Money[N, C]] =
      Show show (Format apply _)

    object Format {
      val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative
      def apply[N: Fractional, C <: Currency: Monetary](m: Money[N, C]): String = {
        val MC   = Monetary[C]
        val fmt  = s"%${flags}.${MC.defaultFractionDigits}f"
        val sfmt = if (implicitly[Fractional[N]].signum(m.amount) < 0) fmt else s" $fmt "
        s"${MC.currencyCode} ${m.amount formatted sfmt}"
      }
    }

    sealed trait Context[C <: Currency] {
      def default: Monetary[C]
    }

    object Context {
      def apply[C <: Currency: Monetary]: Context[C] = new Context[C] { def default = implicitly }
    }

    /**
      * Domain consideration: `Currency` _exchange depends on _pricing_ of some kind.
      * One or more Market(s) determine this price.
      * - A design that doesnt' abstract over `Pricing`, **including live data**, is useless.
      * - otoh dead simple immutable for testing / demo also required
      * Three letter codes: 26 ^ 3 = 17576
      * over two hundred assigned; several "dead" currencies (not reused)
      * Market convention: `ABC/XYZ`: buy or sell units of `ABC`, priced in `XYZ`.
      */
    final case class Cross[CA <: Currency, CB <: Currency, N]()(implicit
                                                                MA: Monetary[CA],
                                                                MB: Monetary[CB],
                                                                N: Fractional[N],
                                                                P: Pricing[N, CA, CB]) {
      import N._, P._

      def buy(ma: Money[N, CA]): Money[N, CB]   = MB(ma.amount * ask)
      def sell(ma: Money[N, CA]): Money[N, CB]  = MB(ma.amount * bid)
      def apply(ma: Money[N, CA]): Money[N, CB] = MB(ma.amount * mid)

      def quote: (Money[N, CB], Money[N, CB])    = (buy(MA(one)), sell(MA(one)))
      def cross: Option[Monetary[_ <: Currency]] = ???

      def description: String = s"""
          |Quoter buys  ${MA} and sells ${MB} at ${bid}
          |Quoter sells ${MA} and buys  ${MB} at ${ask}""".stripMargin
    }

    final case class SimplePricing[N: Fractional, CA <: Currency, CB <: Currency](val ask: N, val bid: N) extends Pricing[N, CA, CB]

    final case class LiveMarketPricing[N: Fractional, CA <: Currency, CB <: Currency](cfg: String) extends Pricing[N, CA, CB] {
      def bid: N = ???
      def ask: N = ???
    }

    // final case class FakeMarketPricing[N, CA <: Currency, CB <: Currency](
    //     quotes: (N, N)*
    // )(
    //     implicit MA: Monetary[CA],
    //     MB: Monetary[CB],
    //     N: Fractional[N]
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

    sealed trait Monetary[C <: Currency] extends MonetaryLike {

      // final type CurrencyType = C
      def apply[N: Fractional](n: N) = Money[N, C](n)

      /**
        * implicit context to provide pricing
        */
      def /[CB <: Currency, N](mb: Monetary[CB])(implicit MA: Monetary[C], N: Fractional[N], P: Pricing[N, C, CB]): Cross[C, CB, N] = {
        implicit val MB = mb
        Cross[C, CB, N]
      }
    }

    def apply[C <: Currency: Monetary]: Monetary[C] = implicitly

    // standard major currency order - can we find another reference?
    // "EUR", "GBP", "AUD", "NZD", "USD", "CAD", "CHF", "NOK", "SEK", "JPY"

    val values = findValues

    sealed trait EUR extends Currency
    case object EUR  extends Monetary[EUR]
    implicit val eur: Monetary[EUR] = EUR

    sealed trait USD extends Currency
    case object USD  extends Monetary[USD]
    implicit val usd: Monetary[USD] = USD

    implicit def context = Context[USD] // FIXME: type annotation problems...
  }
}

// design 2:
// make each Monetary a separate case class
// gets hung up on `import Monetary._`, which imports all the verboten `apply` instances
// without the necessary `Fractional` context bound on `N`. And so overloading can't resolve.
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

    implicit class CurrencyOps[C[?] <: Currency[?]: Monetary, N: Fractional](lhs: C[N]) {
      def N: Fractional[N]   = implicitly
      def MC: Monetary[C]    = implicitly
      def +(rhs: C[N]): C[N] = MC apply (N plus (lhs.amount, rhs.amount))
      def -(rhs: C[N]): C[N] = MC apply (N minus (lhs.amount, rhs.amount))
      def /(rhs: C[N]): N    = N div (lhs.amount, rhs.amount)
      def *(scale: N): C[N]  = MC apply (N times (lhs.amount, scale))
      def unary_- : C[N]     = MC apply (N negate lhs.amount)
    }
    implicit class CurrencyScaleOps[N: Fractional](scale: N) {
      def N: Fractional[N] = implicitly
      def *[C[?] <: Currency[?]: Monetary](rhs: C[N]): C[N] =
        Monetary[C] apply (N times (scale, rhs.amount))
    }

    trait Cross[Cn[?] <: Currency[?], Cd[?] <: Currency[?]]

    sealed trait Monetary[C[?] <: Currency[?]] extends MonetaryLike { self =>

      def apply[N: Fractional](n: N): C[N]

      final def /[C2[?] <: Currency[?]](base: Monetary[C2]): Cross[C, C2] =
        new Cross[C, C2] {}
    }

    implicit def showMoney[C[?] <: Currency[?]: Monetary, N: Fractional]: Show[C[N]] =
      Show show (Format apply _)

    implicit def orderN[N: Fractional] = Order.fromOrdering[N]
    implicit def orderMoney[N: Order, C[?] <: Currency[?]: Monetary]: Order[C[N]] =
      Order by (_.amount)

    implicit def monoid[N: Fractional, C[?] <: Currency[?]: Monetary] = new Monoid[C[N]] {
      val N: Fractional[N]                         = implicitly
      val MC: Monetary[C]                          = implicitly
      override def combine(a: C[N], b: C[N]): C[N] = MC apply (N plus (a.amount, b.amount))
      override lazy val empty: C[N]                = MC apply (N.zero)
    }
    object Format {

      val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative

      def apply[N: Fractional, C[?] <: Currency[?]: Monetary](m: C[N]): String = {
        val MC   = Monetary[C]
        val fmt  = s"%${flags}.${MC.defaultFractionDigits}f"
        val sfmt = if (implicitly[Fractional[N]].signum(m.amount) < 0) fmt else s" $fmt "
        s"${MC.currencyCode} ${m.amount formatted sfmt}"
      }
    }
    def values = findValues

    def apply[C[?] <: Currency[?]: Monetary]: Monetary[C] = implicitly

    final class USD[N] private (val amount: N) extends AnyVal with Currency[N]
    case object USD extends Monetary[USD] {
      def apply[N: Fractional](n: N) = new USD(n)
    }
    implicit val usd: Monetary[USD] = USD

    final class EUR[N] private (val amount: N) extends AnyVal with Currency[N]
    case object EUR extends Monetary[EUR] {
      def apply[N: Fractional](n: N) = new EUR(n)
    }
    implicit val eur: Monetary[EUR] = EUR

  }
}

// private[wip] object UglyTypeDefs {
//   type Code = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
// }
