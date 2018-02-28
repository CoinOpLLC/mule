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
   - take inspiration from `squants.market`
   - separate types for each currency (no addition across currencies! domain modeling dictates!)
   - minimal to no overhead (value classes on [N: Numeric])
    - can summon implicit Monetary[Currency] typeclass instance given a currency instance
   - `enumeratum` lib for denominations (supported currencies)
   - exploit java Currency support
   - abstract over currencies for single implicit `Monoid` function
   - monetary enum as factory pattern - can use to make currency
    (Turns out, this runs into limits to abstraction.)
   - no dependencies other than algebra

   todo: treatment of what's expected to be imported implicitly

  */
private[wip] sealed trait MonetaryLike extends EnumEntry { monetary =>

  // type CurrencyType <: Currency
  // def apply[N: Fractional](n: N): Money[N, CurrencyType]
  final private lazy val jc = java.util.Currency getInstance monetary.entryName

  final def code: String        = jc.getCurrencyCode
  final def numericCode: Int    = jc.getNumericCode
  final def fractionDigits: Int = jc.getDefaultFractionDigits
  final def displayName: String = jc.getDisplayName
  final def symbol: String      = jc.getSymbol

}

/**
  * - `Pricing` behaves well as a mixin
  * - "Orderly market" invariant: `ask` < `bid`.
  * - modelling disorderly markets: not everything that comes at you down the wire makes sense.
  */
trait Pricing[N] {
  def ask: N
  def bid: N
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

    import cats.kernel.{ Monoid, Order }

    implicit def orderN[N: Fractional]: Order[N]                         = Order.fromOrdering[N]
    implicit def orderMoney[N: Order, C <: Currency]: Order[Money[N, C]] = Order by (_.amount)

    implicit def monoid[N: Fractional, C <: Currency] = new Monoid[Money[N, C]] {
      type MNY = Money[N, C]
      override def combine(a: MNY, b: MNY): MNY = a + b
      override lazy val empty: MNY              = Money(implicitly[Fractional[N]].zero)
    }

    implicit def showMoney[C <: Currency: Monetary, N: Fractional]: Show[Money[N, C]] =
      Show show (Format apply _)

    object Format {
      val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative
      def apply[N: Fractional, C <: Currency: Monetary](m: Money[N, C]): String = {
        val DC   = Monetary[C]
        val fmt  = s"%${flags}.${DC.fractionDigits}f"
        val sfmt = if (implicitly[Fractional[N]].signum(m.amount) < 0) fmt else s" $fmt "
        s"${DC.code} ${m.amount formatted sfmt}"
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
      * - A design that doesnt' abstract over `Pricing`, including live data, is useless.
      * - otoh dead simple immutable for testing / demo also required
      * Three letter codes: 26 ^ 3 = 17576
      * over two hundred assigned; several "dead" currencies (not reused)
      * Market convention: `ABC/XYZ`: buy or sell units of `ABC`, priced in `XYZ`.
      */
    class Cross[CA <: Currency, CB <: Currency, N: Fractional] { _: Pricing[N] =>
      val N = implicitly[Fractional[N]]
      import N.mkNumericOps
      final def buy(ma: Money[N, CA])  = Money[N, CB](ma.amount * ask)
      final def sell(ma: Money[N, CA]) = Money[N, CB](ma.amount * bid)
    }

    class SimplePricing[N: Fractional](val bid: N, val ask: N) extends Pricing[N]

    final case class SimplePricedCross[CA <: Currency, CB <: Currency, N: Fractional](
        val ask: N,
        val bid: N
    ) extends Cross[CA, CB, N]
        with Pricing[N]

    sealed trait Monetary[C <: Currency] extends MonetaryLike {

      // final type CurrencyType = C
      def apply[N: Fractional](n: N) = Money[N, C](n)

      /**
        * implicit context to provide pricing
        */
      def /[CB <: Currency, N: Fractional](base: Monetary[CB]): Cross[C, CB, N] = {
        val N = implicitly[Fractional[N]]
        import N._
        SimplePricedCross[C, CB, N](one + one + one, one + one) // FIXME!
      }
    }

    def apply[C <: Currency: Monetary]: Monetary[C] = implicitly

    val values = findValues

    sealed trait USD extends Currency
    case object USD  extends Monetary[USD]
    implicit val usd: Monetary[USD] = USD

    sealed trait EUR extends Currency
    case object EUR  extends Monetary[EUR]
    implicit val eur: Monetary[EUR] = EUR

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

      final def code: String = { // principle of least ghastliness
        val cs = getClass.toString
        cs substring (cs lastIndexOf '$') + 1
      }
      final override def toString: String = s"$code(${amount.toString})"
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
        val DC   = Monetary[C]
        val fmt  = s"%${flags}.${DC.fractionDigits}f"
        val sfmt = if (implicitly[Fractional[N]].signum(m.amount) < 0) fmt else s" $fmt "
        s"${DC.code} ${m.amount formatted sfmt}"
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
