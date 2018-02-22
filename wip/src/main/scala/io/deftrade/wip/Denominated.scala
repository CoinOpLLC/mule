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

// import scala.language.implicitConversions

// import scala.util.Try

import enumeratum._
// import enumeratum.values._

//
import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
// import refined.numeric._
// import refined.auto._

// /*
//  * This is an old pattern actually; Effective Java maybe? Enums as factories...
//  * and another oldie: phantom types for the value class,
//  * to allow it to carry the currency "masslessly"
//  */
// final case class Money[N, C <: Currency] private[model] (val amount: N) extends AnyVal {
//   def denomination(implicit d: Denomination[C]): Denomination[C] = d
//   // override def toString                                          = s"""${denomination.code}($amount)"""
// }
//
// object Money {
//
//   implicit def eq[N: Numeric, C <: Currency] = Eq.fromUniversalEquals[Money[N, C]]
//
//   implicit def monoid[N: Numeric, C <: Currency] = new Monoid[Money[N, C]] {
//     type DN = Money[N, C]
//     val N                                  = implicitly[Numeric[N]]
//     override def combine(a: DN, b: DN): DN = Money[N, C](N.plus(a.amount, b.amount))
//     override lazy val empty: DN            = Money[N, C](N.zero)
//   }
// }
//
// sealed trait Currency
// sealed trait Cross[C <: Currency, CN <: Currency] {}
//
// sealed trait DenominationBase extends EnumEntry
// sealed trait Denomination[C <: Currency] extends DenominationBase { denomination =>
//
//   private val jc = java.util.Currency getInstance denomination.entryName
//
//   def code: String        = jc.getCurrencyCode
//   def numericCode: Int    = jc.getNumericCode
//   def fractionDigits: Int = jc.getDefaultFractionDigits
//   def displayName: String = jc.getDisplayName
//   def symbol: String      = jc.getSymbol
//
//   // factory for denominated - only way to create.
//   def apply[N: Numeric](n: N) = Money[N, C](n)
//
//   def /[CB <: Currency](base: Denomination[CB]): Cross[C, CB] = ???
// }
//
// object Denomination extends Enum[DenominationBase] {
//
//   val values = findValues
//
//   trait USD       extends Currency
//   case object USD extends Denomination[USD]
//
//   trait EUR       extends Currency
//   case object EUR extends Denomination[EUR]
//
//   implicit def eqN[N: Numeric]: Eq[N]     = Eq.fromUniversalEquals[N]
//   implicit def eqD[D <: DenominationBase] = Eq.fromUniversalEquals[D]
//
// }
import scala.language.higherKinds

/**

  requirements for currency / money:
  - minimal (value classes over any Numeric)
  - enumeratum for denominations
  - separate types for each currency (no addition across currencies! domain modeling dictates!)
  - native Ordered and toString (distinct java classes for the Boxes)
  - exploit java Currency support
  - abstract over currencies for single Monoid definition
  - enum as factory pattern (minize names and object
  - summon an implicit Denomination object given a currency amount instance

  design 1: phantom types
  problem: toString (only one java class serves as a box - the restriction is entirely a compile time scala typesystem thing. Which is fine, as far as it goes, which isn't far enough to interoperate with other sw that expects toString to give distinct output for different currencies.)

  some implicit magic is suggested, but perhaps this is best applied on the Currency contrstruction side

  desgin 2:


  */
sealed trait Currency[N] extends Any // { def amount: N }
object Currency {
  implicit class CurrencyOps[C[_] <: Currency[N]: Denomination, N: Numeric](cnl: C[N]) {
    val N                    = implicitly[Numeric[N]]
    val D                    = Denomination[C]
    def +(cnr: C[N]): C[N]   = D apply (N plus (D amount cnl, D amount cnr))
    def /(cnr: C[N]): Double = (N toDouble (D amount cnl)) / (N toDouble (D amount cnr))
    def *(nr: N): C[N]       = D apply (N times (D amount cnl, nr))
    def unary_- : C[N]       = D apply (N negate (D amount cnl))
  }
}

sealed trait DenominationBase extends EnumEntry { denomination =>

  private lazy val jc = java.util.Currency getInstance denomination.entryName

  def code: String        = jc.getCurrencyCode
  def numericCode: Int    = jc.getNumericCode
  def fractionDigits: Int = jc.getDefaultFractionDigits
  def displayName: String = jc.getDisplayName
  def symbol: String      = jc.getSymbol
}

sealed trait Denomination[C[_] <: Currency[_]] extends DenominationBase {
  def apply[N: Numeric](n: N): C[N]
  // def /[C2[_]](base: Denomination[C2]): Denomination.Cross = ???
  // def unapply[N: Numeric](c: C[N]): Option[N]
  def amount[N: Numeric](c: C[N]): N // = c.amount // FIXME - do I want [?] here
}

object Denomination extends Enum[DenominationBase] {
  import cats.kernel.{ Monoid, Order }

  def apply[C[_] <: Currency[_]: Denomination]: Denomination[C] = implicitly

  def values = findValues

  trait Cross

  final class USD[N] private[Denomination] (val amount: N) extends AnyVal with Currency[N]
  object USD extends Denomination[USD] {
    def apply[N: Numeric](n: N)          = new USD(n)
    def amount[N: Numeric](c: USD[N]): N = c.amount
  }

  final class EUR[N] private[Denomination] (val amount: N) extends AnyVal with Currency[N]
  object EUR extends Denomination[EUR] {
    def apply[N: Numeric](n: N)          = new EUR(n)
    def amount[N: Numeric](c: EUR[N]): N = c.amount
  }

  implicit def order[N: Numeric] = Order.fromOrdering[N]

  implicit def monoid[N: Numeric, C[_] <: Currency[_]: Denomination] = new Monoid[C[N]] {
    val N                                        = implicitly[Numeric[N]]
    val D                                        = Denomination[C]
    override def combine(a: C[N], b: C[N]): C[N] = D apply (N plus (D amount a, D amount b))
    override lazy val empty: C[N]                = D apply N.zero
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

  // def funge[C <: Currency](den: Denomination[C]): Money[Double, C] = den(19.47)
  def funge[C[_] <: Currency[_]](den: Denomination[C]): C[Double] = den(19.47)

}

private[wip] object UglyTypeDefs {
  type Code = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
}
