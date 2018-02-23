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
import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
// import refined.numeric._
// import refined.auto._

/**

   requirements for currency / money:
   - separate types for each currency (no addition across currencies! domain modeling dictates!)
   - minimal (value classes on [N: Numeric])
   - summon an implicit Denomination object given a currency amount instance
   - `enumeratum` lib for denominations (supported currencies)
   - exploit java Currency support
   - abstract over currencies for single implicit `Monoid` function
   - denomination enum as factory pattern - can use to make currency

   design 1: phantom types
   problem: toString (only one java class serves as a box - the restriction is entirely a compile time scala typesystem thing. Which is fine, as far as it goes, which isn't far enough to interoperate with other sw that expects toString to give distinct output for different currencies.)
   - denomination factory pattern falls apart because EnumEntry subclasses used as parameters for the `Enum` type constructor cannot themselves be type constructors.

   design 2:
   make each Denomination a separate case class
   gets hung up on `import Denomination._`, which imports all the verboten `apply` instances
   without the necessary `Numeric` context bound on `N`. And so overloading can't resolve.
   -> and so devolves back to the same need for a Show[Currency] class as phantom type
   (design 1)

  */
final case class Money[N, C <: Currency] private[Money] (val amount: N) extends AnyVal {

  type MNY = Money[N, C]

  def +(rhs: MNY)(implicit N: Numeric[N]): MNY = Money(N plus (amount, rhs.amount))
  def unary_-(implicit N: Numeric[N]): MNY     = Money(N negate amount)
  // intentionally omitted
  // def -(rhs: MNY)(implicit N: Numeric[N]): MNY

  def *(scale: N)(implicit N: Numeric[N]): MNY    = Money(N times (scale, amount))
  def /(rhs: MNY)(implicit N: Numeric[N]): Double = (N toDouble amount) / (N toDouble rhs.amount)
}

object Money {

  import cats.kernel.{ Monoid, Order }

  implicit def orderN[N: Numeric]: Order[N]                            = Order.fromOrdering[N]
  implicit def orderMoney[N: Order, C <: Currency]: Order[Money[N, C]] = Order by (_.amount)

  implicit def monoid[N: Numeric, C <: Currency] = new Monoid[Money[N, C]] {
    type MNY = Money[N, C]
    override def combine(a: MNY, b: MNY): MNY = a + b
    override lazy val empty: MNY              = Money(implicitly[Numeric[N]].zero)
  }
}

sealed trait Currency
sealed trait Cross[C <: Currency, CN <: Currency]

sealed trait DenominationBase extends EnumEntry { denomination =>

  private val jc = java.util.Currency getInstance denomination.entryName

  def code: String        = jc.getCurrencyCode
  def numericCode: Int    = jc.getNumericCode
  def fractionDigits: Int = jc.getDefaultFractionDigits
  def displayName: String = jc.getDisplayName
  def symbol: String      = jc.getSymbol
}

sealed trait Denomination[C <: Currency] extends DenominationBase {
  type CurrencyType = C
  def apply[N: Numeric](n: N)                                 = Money[N, C](n)
  def /[CB <: Currency](base: Denomination[CB]): Cross[C, CB] = new Cross[C, CB] {}
}

object Denomination extends Enum[DenominationBase] {

  val values = findValues

  final class USD extends Currency
  case object USD extends Denomination[USD]

  final class EUR extends Currency
  case object EUR extends Denomination[EUR]

}

object AltVersion {

  import scala.language.higherKinds

  sealed trait Currency[N] extends Any { def amount: N }
  object Currency {
    implicit class CurrencyOps[C[?] <: Currency[?]: Denomination, N: Numeric](lhs: C[N]) {
      def N: Numeric[N]        = implicitly
      def DC: Denomination[C]  = implicitly
      def +(rhs: C[N]): C[N]   = DC apply (N plus (lhs.amount, rhs.amount))
      def /(rhs: C[N]): Double = (N toDouble lhs.amount) / (N toDouble rhs.amount)
      def *(scale: N): C[N]    = DC apply (N times (scale, lhs.amount))
      def unary_- : C[N]       = DC apply (N negate lhs.amount)
    }
  }

  sealed trait DenominationBase extends EnumEntry { denomination =>

    final lazy val jc = java.util.Currency getInstance denomination.entryName

    final def code: String        = jc.getCurrencyCode
    final def numericCode: Int    = jc.getNumericCode
    final def fractionDigits: Int = jc.getDefaultFractionDigits
    final def displayName: String = jc.getDisplayName
    final def symbol: String      = jc.getSymbol
  }

  sealed trait Denomination[C[?] <: Currency[?]] extends DenominationBase {
    def apply[N: Numeric](n: N): C[N]
    final def /[C2[?] <: Currency[?]](base: Denomination[C2]): Denomination.Cross =
      new Denomination.Cross {}
  }

  object Denomination extends Enum[DenominationBase] {
    import cats.kernel.{ Monoid, Order }

    def values = findValues

    trait Cross

    final class USD[N] private[Denomination] (val amount: N) extends AnyVal with Currency[N]
    object USD extends Denomination[USD] {
      def apply[N: Numeric](n: N) = new USD(n)
    }

    final class EUR[N] private[Denomination] (val amount: N) extends AnyVal with Currency[N]
    object EUR extends Denomination[EUR] {
      def apply[N: Numeric](n: N) = new EUR(n)
    }

    implicit def order[N: Numeric] = Order.fromOrdering[N]

    implicit def monoid[N: Numeric, C[?] <: Currency[?]: Denomination] = new Monoid[C[N]] {
      val N: Numeric[N]                            = implicitly
      val DC: Denomination[C]                      = implicitly
      override def combine(a: C[N], b: C[N]): C[N] = DC apply (N plus (a.amount, b.amount))
      override lazy val empty: C[N]                = DC apply (N.zero)
    }

  }

}

object Examples {

  import wip.Denomination._

  val eur = EUR // Denomination withName "EUR"
  // val eurF  = (d: Double) => eur(d)
  val eurF  = eur[Double] _
  val eur20 = 20.0 |> eurF

  val d20 = USD(20)
  val d21 = USD(21)

  val e20 = EUR(20)

  def funge[C <: Currency](den: Denomination[C]): Money[Double, C] = den(19.47)
  // def funge[C[?] <: Currency[?]](den: Denomination[C]): C[Double] = den(19.47)

}

private[wip] object UglyTypeDefs {
  type Code = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
}
