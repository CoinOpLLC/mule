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

import cats.Show
// import refined.collection._
// import refined.numeric._
// import refined.auto._

/**

   requirements for currency / money:
   - take inspiration from `squants.market`
   - separate types for each currency (no addition across currencies! domain modeling dictates!)
   - minimal to no overhead (value classes on [N: Numeric])
    - can summon implicit Moneta[Currency] typeclass instance given a currency instance
   - `enumeratum` lib for denominations (supported currencies)
   - exploit java Currency support
   - abstract over currencies for single implicit `Monoid` function
   - moneta enum as factory pattern - can use to make currency
    (Turns out, this runs into limits to abstraction.)
   - no dependencies other than algebra

   todo: treatment of what's expected to be imported implicitly

  */
object PhantomTypePerCurrency {
  // design 1: phantom types
  // problem: toString (only one java class serves as a box - the restriction is entirely a compile time scala typesystem thing.
  // Which is fine, as far as it goes, which isn't far enough to interoperate with other sw that expects toString to give distinct output for different currencies.)
  // But we can implement Show[Money[N, C]] for all the N and C we care about.

  final case class Money[N, C <: Currency] private[Money] (val amount: N) extends AnyVal {

    type MNY = Money[N, C]

    def +(rhs: MNY)(implicit N: Fractional[N]): MNY = Money(N plus (amount, rhs.amount))
    def unary_-(implicit N: Fractional[N]): MNY     = Money(N negate amount)
    // intentionally omitted
    // def -(rhs: MNY)(implicit N: Fractional[N]): MNY

    def *(scale: N)(implicit N: Fractional[N]): MNY = Money(N times (scale, amount))
    def /(rhs: MNY)(implicit N: Fractional[N]): N   = N div (amount, rhs.amount)
  }

  object Money {

    import cats.kernel.{ Monoid, Order }

    implicit def orderN[N: Fractional]: Order[N]                         = Order.fromOrdering[N]
    implicit def orderMoney[N: Order, C <: Currency]: Order[Money[N, C]] = Order by (_.amount)

    implicit def monoid[N: Fractional, C <: Currency] = new Monoid[Money[N, C]] {
      type MNY = Money[N, C]
      override def combine(a: MNY, b: MNY): MNY = a + b
      override lazy val empty: MNY              = Money(implicitly[Fractional[N]].zero)
    }

    implicit def showMoney[C <: Currency: Moneta, N: Fractional]: Show[Money[N, C]] =
      Show show (Format apply _)

    object Format {
      val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative
      def apply[N: Fractional, C <: Currency: Moneta](m: Money[N, C]): String = {
        val DC   = Moneta[C]
        val fmt  = s"%${flags}.${DC.fractionDigits}f"
        val sfmt = if (implicitly[Fractional[N]].signum(m.amount) < 0) fmt else s" $fmt "
        s"${DC.code} ${m.amount formatted sfmt}"
      }
    }
  }

  sealed trait Currency
  sealed trait Cross[CA <: Currency, CB <: Currency]

  private[wip] sealed trait BaseMoneta extends EnumEntry { moneta =>

    // type CurrencyType <: Currency
    // def apply[N: Fractional](n: N): Money[N, CurrencyType]
    final lazy val jc = java.util.Currency getInstance moneta.entryName

    final def code: String        = jc.getCurrencyCode
    final def numericCode: Int    = jc.getNumericCode
    final def fractionDigits: Int = jc.getDefaultFractionDigits
    final def displayName: String = jc.getDisplayName
    final def symbol: String      = jc.getSymbol

  }

  sealed trait Moneta[C <: Currency] extends BaseMoneta {

    // final type CurrencyType = C
    def apply[N: Fractional](n: N)                        = Money[N, C](n)
    def /[CB <: Currency](base: Moneta[CB]): Cross[C, CB] = new Cross[C, CB] {}
  }

  object Moneta extends Enum[BaseMoneta] {

    def apply[C <: Currency: Moneta]: Moneta[C] = implicitly

    val values = findValues

    sealed trait USD extends Currency
    case object USD  extends Currency with Moneta[USD]

    sealed trait EUR extends Currency
    case object EUR  extends Moneta[EUR]

  }

  import Moneta._

  implicit val denominationUsd: Moneta[USD] = USD
  implicit val denominationEur: Moneta[EUR] = EUR
}

object ClassPerCurrency {
  // design 2:
  // make each Moneta a separate case class
  // gets hung up on `import Moneta._`, which imports all the verboten `apply` instances
  // without the necessary `Fractional` context bound on `N`. And so overloading can't resolve.
  // -> and so devolves back to the same need for a Show[Currency] class as phantom type
  // (design 1)

  import scala.language.higherKinds

  sealed trait Currency[N] extends Any {

    def amount: N

    final def code: String = { // principle of least ghastliness
      val cs = getClass.toString
      cs substring (cs lastIndexOf '$') + 1
    }
    final override def toString: String = s"$code(${amount.toString})"
  }

  object Currency {
    implicit class CurrencyOps[C[?] <: Currency[?]: Moneta, N: Fractional](lhs: C[N]) {
      def N: Fractional[N]     = implicitly
      def DC: Moneta[C]        = implicitly
      def +(rhs: C[N]): C[N]   = DC apply (N plus (lhs.amount, rhs.amount))
      def /(rhs: C[N]): Double = (N toDouble lhs.amount) / (N toDouble rhs.amount)
      def *(scale: N): C[N]    = DC apply (N times (scale, lhs.amount))
      def unary_- : C[N]       = DC apply (N negate lhs.amount)
    }
  }

  private[wip] sealed trait BaseMoneta extends EnumEntry { moneta =>

    final lazy val jc = java.util.Currency getInstance moneta.entryName

    final def code: String        = jc.getCurrencyCode
    final def numericCode: Int    = jc.getNumericCode
    final def fractionDigits: Int = jc.getDefaultFractionDigits
    final def displayName: String = jc.getDisplayName
    final def symbol: String      = jc.getSymbol

  }

  sealed trait Moneta[C[?] <: Currency[?]] extends BaseMoneta {

    def apply[N: Fractional](n: N): C[N]

    final def /[C2[?] <: Currency[?]](base: Moneta[C2]): Moneta.Cross[C, C2] =
      new Moneta.Cross[C, C2] {}
  }

  object Moneta extends Enum[BaseMoneta] {
    import cats.kernel.{ Monoid, Order }

    def apply[C[?] <: Currency[?]: Moneta]: Moneta[C] = implicitly

    def values = findValues

    trait Cross[Cn[?] <: Currency[?], Cd[?] <: Currency[?]]

    final class USD[N] private (val amount: N) extends AnyVal with Currency[N]
    case object USD extends Moneta[USD] {
      def apply[N: Fractional](n: N) = new USD(n)
    }

    final class EUR[N] private (val amount: N) extends AnyVal with Currency[N]
    case object EUR extends Moneta[EUR] {
      def apply[N: Fractional](n: N) = new EUR(n)
    }

    implicit def order[N: Fractional] = Order.fromOrdering[N]
    implicit def orderMoney[N: Order, C[?] <: Currency[?]: Moneta]: Order[C[N]] =
      Order by (_.amount)

    implicit def monoid[N: Fractional, C[?] <: Currency[?]: Moneta] = new Monoid[C[N]] {
      val N: Fractional[N]                         = implicitly
      val DC: Moneta[C]                            = implicitly
      override def combine(a: C[N], b: C[N]): C[N] = DC apply (N plus (a.amount, b.amount))
      override lazy val empty: C[N]                = DC apply (N.zero)
    }
  }
  implicit def showMoney[C[?] <: Currency[?]: Moneta, N: Fractional]: Show[C[N]] =
    Show show (Format apply _)

  object Format {
    val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative
    def apply[N: Fractional, C[?] <: Currency[?]: Moneta](m: C[N]): String = {
      val DC   = Moneta[C]
      val fmt  = s"%${flags}.${DC.fractionDigits}f"
      val sfmt = if (implicitly[Fractional[N]].signum(m.amount) < 0) fmt else s" $fmt "
      s"${DC.code} ${m.amount formatted sfmt}"
    }

  }

  import Moneta._

  implicit val denominationUsd: Moneta[USD] = USD
  implicit val denominationEur: Moneta[EUR] = EUR

}

// private[wip] object UglyTypeDefs {
//   type Code = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
// }
