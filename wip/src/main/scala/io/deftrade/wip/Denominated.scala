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
package model

// import scala.language.implicitConversions

// import scala.util.Try

import cats.{ Eq, Monoid }
import cats.syntax.all._
import cats.{ instances => ci }
import ci.int._    // , ci.string._, ci.double._, ci.boolean._, ci.bigDecimal._, ci.long._
import ci.option._ //, ci.tuple._, ci.list._, ci.set._, ci.map._
// import ci.eq._

import enumeratum._
// import enumeratum.values._

//
import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
// import refined.numeric._
import refined.auto._

/*
 * This is an old pattern actually; Effective Java maybe? Enums as factories...
 * and another oldie: phantom types for the value class, to allow it to carry the currency "masslessly"
 */
final case class D9d[N, +D <: Denomination] private[model] (val value: N) extends AnyVal // with Ordered[N] FIXME
object D9d {
  implicit def eq[N: Numeric, D <: Denomination]                  = Eq.fromUniversalEquals[D9d[N, D]]
  def apply[D <: Denomination, N: Numeric](d: D)(n: N): D9d[N, D] = d d9d n
  // def apply[D <: Denomination, N: Numeric](d: D): N => D9d[N, d.type] = n => D9d(n)
}

sealed trait Denomination extends EnumEntry { denomination =>

  private val jc = java.util.Currency getInstance denomination.entryName

  def code: String        = jc.getCurrencyCode
  def numericCode: Int    = jc.getNumericCode
  def fractionDigits: Int = jc.getDefaultFractionDigits
  def displayName: String = jc.getDisplayName
  def symbol: String      = jc.getSymbol

  type ExchangeRate <: { def apply(bid: Double, ask: Double): Nothing }
  def /(base: Denomination): ExchangeRate = ???

  final case class Denominated[N](val amount: N) /* extends AnyVal */ {
    type AmountType = N
    def currency: Denomination = denomination
    override def toString      = s"""${currency.code}($amount)"""
  }
  def apply[N: Numeric](n: N) = Denominated(n)

  import Denomination.{ EUR, USD }
  def d9d[N: Numeric](n: N) = D9d[N, denomination.type](n)

  implicit def eq[N: Numeric]: Eq[Denominated[N]] = Eq.fromUniversalEquals[Denominated[N]]

  implicit def monoid[N: Numeric] = new Monoid[Denominated[N]] {
    type DN = Denominated[N]
    val N                                  = implicitly[Numeric[N]]
    override def combine(a: DN, b: DN): DN = Denominated[N](N.plus(a.amount, b.amount))
    override lazy val empty: DN            = Denominated[N](N.zero)
  }
}

object Denomination extends Enum[Denomination] {

  val values = findValues

  case object USD extends Denomination
  case object EUR extends Denomination
  // case object XYZ extends Denomination // FIXME doesn't find problem b/c lazy init

  implicit def eqN[N: Numeric]: Eq[N] = Eq.fromUniversalEquals[N]
  implicit def eqD[D <: Denomination] = Eq.fromUniversalEquals[D]

}

object Denominated {
  // def apply[N: Numeric](d: Denomination): N => d.Denominated[N]  = n => d(n)
  def apply[N: Numeric](d: Denomination)(n: N): d.Denominated[N] = d.Denominated(n)
}

object Examples {
  import Denomination._

  val d     = Denomination withName "EUR"
  val eurF  = Denominated[Int](d) _
  val eur20 = 20 |> eurF

  val buxxf: Int => USD.Denominated[Int] = Denominated[Int](USD)
  val buxx                               = 20 |> buxxf
  val bx2                                = 20 |> buxxf
  buxx === bx2 |> assert

  val d20 = D9d(USD)(20)
  val d21 = D9d(USD)(21)

  val e20 = D9d(EUR)(20)

  val d2: Denomination = USD
  val usdF             = d2.d9d[Int] _

  val usd100 = 100 |> usdF
}

private[model] object UglyTypeDefs {
  type Code = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
}
// FROM branch vcdenom

// package wut
// package model
//
// // import scala.language.implicitConversions
//
// // import scala.util.Try
//
// import cats.{ Eq, Monoid }
// import cats.syntax.all._
// import cats.{ instances => ci }
// import ci.int._, ci.string._, ci.double._, ci.boolean._, ci.bigDecimal._, ci.long._
// import ci.option._, ci.tuple._, ci.list._, ci.set._, ci.map._
// import ci.eq._
//
// // import enumeratum._
// // import enumeratum.values._
//
// //
// import eu.timepit.refined
// import refined.api.Refined
// import refined.W
// // import refined.collection._
// // import refined.numeric._
// import refined.auto._
//
// // TODO: try a typeclass for currency,
// // use an implicit def to turn the type bounds into a set of instances...
//
// // Idea: `Denomination[C <: JvmCurrency]` typeclass. Construct money from their (implicit) instances.
//
// /*
//  * This is an old pattern actually; Enums as factories...
//  * and another oldie: phantom types for the value class, to allow it to carry the currency "masslessly"
//  */
// /** This wants to be a value class someday (it told me so). */
// import java.util.Currency
// import scala.language.higherKinds
//
// sealed trait Denomination {
//
//   protected def jc: Currency
//
//   type DnType[µ] <: Denominated[µ]
//
//   final def code: String        = jc.getCurrencyCode
//   final def numericCode: Int    = jc.getNumericCode
//   final def fractionDigits: Int = jc.getDefaultFractionDigits
//   final def displayName: String = jc.getDisplayName
//   final def symbol: String      = jc.getSymbol
//
// }
//
// object Denomination {
//   implicit def eqN[N: Numeric]: Eq[N] = Eq.fromUniversalEquals[N] // where should this go?
// }
//
// sealed trait Denominated[N] extends Any {
//
//   type AmountType = N
//   def amount: N
//   type D <: Denomination
//   def denomination: D
//
//   final override def toString: String = s"${denomination.code}($amount)"
//
// }
//
// object USD extends Denomination {
//
//   type DnType[µ] = USD[µ]
//   override protected lazy val jc = Currency getInstance "USD"
//
//   def apply[N: Numeric](amount: N): USD[N]        = new USD[N](amount)
//   def unapply[N: Numeric](usd: USD[N]): Option[N] = usd.amount.some
//
//   implicit def eq[N: Numeric]: Eq[USD[N]] = Eq.fromUniversalEquals[USD[N]]
//
//   implicit def monoid[N: Numeric]: Monoid[USD[N]] = new Monoid[USD[N]] {
//     val N                                              = implicitly[Numeric[N]]
//     override def combine(a: USD[N], b: USD[N]): USD[N] = apply(N.plus(a.amount, b.amount))
//     override lazy val empty: USD[N]                    = apply(N.zero)
//   }
// }
//
// final class USD[N] private (val amount: N) extends AnyVal with Denominated[N] {
//   type D = USD.type
//   override def denomination = USD
// }
//
// object EUR extends Denomination {
//
//   type DnType[µ] = EUR[µ]
//
//   override protected lazy val jc = Currency getInstance "EUR"
//
//   def apply[N: Numeric](amount: N): EUR[N]        = new EUR[N](amount)
//   def unapply[N: Numeric](usd: EUR[N]): Option[N] = usd.amount.some
//
//   implicit def eq[N: Numeric]: Eq[EUR[N]] = Eq.fromUniversalEquals[EUR[N]]
//
//   implicit def monoid[N: Numeric]: Monoid[EUR[N]] = new Monoid[EUR[N]] {
//     val N                                              = implicitly[Numeric[N]]
//     override def combine(a: EUR[N], b: EUR[N]): EUR[N] = apply(N.plus(a.amount, b.amount))
//     override lazy val empty: EUR[N]                    = apply(N.zero)
//   }
// }
//
// final class EUR[N] private (val amount: N) extends AnyVal with Denominated[N] {
//   type D = EUR.type
//   override def denomination = EUR
// }
//
// object Denominated { // FIXME this is fuxd: apply() can't return a specific enough return type
//   def apply[N: Numeric, D <: Denomination](n: N, d: D): d.DnType[N] = d match {
//     case USD => USD(n)
//     case EUR => EUR(n)
//   }
//
// }
//
// object Examples {
//   import Denomination._
//   // val buxf: Int => USD[Int] = Denominated[Int, USD[Int]](USD)
//   // val bux20                 = 20 |> buxf
//   // val bux20too              = 20 |> buxf
//   // bux20 === bux20too |> assert
//
//   val benjAhMin = USD(100)
//   val benjOhOne = USD(101)
//   val e20       = EUR(20)
//
//   benjAhMin =!= benjOhOne |> assert
//   e20 === EUR(20)         |> assert
//   val x = Denominated(20, EUR)
//   // x === e20 |> assert FIXME
// }
//
// private[model] object UglyTypeDefs {
//   type CurrencyCode = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
// }
