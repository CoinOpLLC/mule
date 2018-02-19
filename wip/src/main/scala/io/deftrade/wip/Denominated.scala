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
 * and another oldie: phantom types for the value class,
 * to allow it to carry the currency "masslessly"
 */
final case class Money[N, C <: Currency] private[model] (val amount: N) extends AnyVal {
  def denomination(implicit d: Denomination[C]): Denomination[C] = d
  // override def toString                                          = s"""${denomination.code}($amount)"""
}

object Money {

  implicit def eq[N: Numeric, C <: Currency] = Eq.fromUniversalEquals[Money[N, C]]

  implicit def monoid[N: Numeric, C <: Currency] = new Monoid[Money[N, C]] {
    type DN = Money[N, C]
    val N                                  = implicitly[Numeric[N]]
    override def combine(a: DN, b: DN): DN = Money[N, C](N.plus(a.amount, b.amount))
    override lazy val empty: DN            = Money[N, C](N.zero)
  }
}

sealed trait Currency
sealed trait Cross[C <: Currency, CN <: Currency] {}

sealed trait DenominationBase extends EnumEntry
sealed trait Denomination[C <: Currency] extends DenominationBase { denomination =>

  private val jc = java.util.Currency getInstance denomination.entryName

  def code: String        = jc.getCurrencyCode
  def numericCode: Int    = jc.getNumericCode
  def fractionDigits: Int = jc.getDefaultFractionDigits
  def displayName: String = jc.getDisplayName
  def symbol: String      = jc.getSymbol

  // factory for denominated - only way to create.
  def apply[N: Numeric](n: N) = Money[N, C](n)

  def /[CB <: Currency](base: Denomination[CB]): Cross[C, CB] = ???
}

object Denomination extends Enum[DenominationBase] {

  val values = findValues

  trait USD       extends Currency
  case object USD extends Denomination[USD]

  trait EUR       extends Currency
  case object EUR extends Denomination[EUR]

  implicit def eqN[N: Numeric]: Eq[N]     = Eq.fromUniversalEquals[N]
  implicit def eqD[D <: DenominationBase] = Eq.fromUniversalEquals[D]

}

object Examples {

  import Denomination._

  val eur   = EUR // Denomination withName "EUR"
  val eurF  = eur[Double] _
  val eur20 = 20.0 |> eurF

  val d20 = USD(20)
  val d21 = USD(21)

  val e20 = EUR(20)

  def funge[C <: Currency](den: Denomination[C]): Money[Double, C] = den(19.47)

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
