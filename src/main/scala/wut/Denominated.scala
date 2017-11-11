package wut
package model

// import scala.language.implicitConversions

// import scala.util.Try

import cats.{ Eq, Monoid }
import cats.syntax.all._
import cats.{ instances => ci }
import ci.int._, ci.string._, ci.double._, ci.boolean._, ci.bigDecimal._, ci.long._
import ci.option._, ci.tuple._, ci.list._, ci.set._, ci.map._
import ci.eq._

import enumeratum._
import enumeratum.values._

//
import eu.timepit.refined
import refined.api.Refined
import refined.W
import refined.collection._
import refined.numeric._
import refined.auto._

// TODO: try a typeclass for currency,
// use an implicit def to turn the type bounds into a set of instances...

// Idea: `Denomination[C <: JvmCurrency]` typeclass. Construct money from their (implicit) instances.

/** This wants to be a value class someday (it told me so). */
sealed abstract class Denomination extends EnumEntry { denomination =>

  val jc = java.util.Currency getInstance denomination.entryName ensuring (_ != null)

  def code: String        = jc.getCurrencyCode
  def numericCode: Int    = jc.getNumericCode
  def fractionDigits: Int = jc.getDefaultFractionDigits
  def displayName: String = jc.getDisplayName
  def symbol: String      = jc.getSymbol

  /** This wants to be a value class someday (it told me so). */
  final case class Denominated[N](val amount: N) {
    type AmountType = N
    def currency: Denomination = denomination
    override def toString      = s"""${currency.code}($amount)"""
  }
  def apply[N: Numeric](n: N) = Denominated(n)

  implicit def eq[N: Numeric]: Eq[Denominated[N]] = Eq.fromUniversalEquals[Denominated[N]]

  implicit def monoid[N: Numeric] = new Monoid[Denominated[N]] {
    val N = implicitly[Numeric[N]]
    type DN = Denominated[N]
    override def combine(a: DN, b: DN): DN = Denominated[N](N.plus(a.amount, b.amount))
    override lazy val empty: DN            = Denominated[N](N.zero)
  }
}

object Denomination extends Enum[Denomination] {

  val values = findValues

  case object USD extends Denomination
  case object EUR extends Denomination
  case object XYZ extends Denomination

  implicit def eqN[N: Numeric]: Eq[N] = Eq.fromUniversalEquals[N]
  implicit def eqD[D <: Denomination] = Eq.fromUniversalEquals[D]

}

object Denominated {
  def apply[N: Numeric](d: Denomination): N => d.Denominated[N] = n => d(n)
}

object Examples {
  import Denomination._
  val buxxf: Int => USD.Denominated[Int] = Denominated[Int](USD)
  val buxx                               = 20 |> buxxf
  val bx2                                = 20 |> buxxf
  buxx === bx2 |> assert
}

private[model] object UglyTypeDefs {
  type Code = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
}
