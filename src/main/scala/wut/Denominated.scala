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

/*
 * This is an old pattern actually; Enums as factories...
 * and another oldie: phantom types for the value class, to allow it to carry the currency "masslessly"
 */
/** This wants to be a value class someday (it told me so). */
final case class D2[N, D <: Denomination](val value: N) extends AnyVal // with Ordered[N] FIXME
object D2 {
  implicit def eq[N: Numeric, D <: Denomination] = Eq.fromUniversalEquals[D2[N, D]]
}

sealed abstract class Denomination extends EnumEntry { denomination =>

  val jc = java.util.Currency getInstance denomination.entryName

  def code: String        = jc.getCurrencyCode
  def numericCode: Int    = jc.getNumericCode
  def fractionDigits: Int = jc.getDefaultFractionDigits
  def displayName: String = jc.getDisplayName
  def symbol: String      = jc.getSymbol

  /** This wants to be a value class someday (it told me so).
  OK whacky idea: take it outside so it can be a value class; use Refined to tag it with
  the currency type ?!
  `type USD[N] = Denominated[N] Refined USD // or something`
    */
  final case class Denominated[N](val amount: N) /* extends AnyVal */ {
    type AmountType = N
    def currency: Denomination = denomination
    override def toString      = s"""${currency.code}($amount)"""
  }
  def apply[N: Numeric](n: N) = Denominated(n)
  def d2[N: Numeric](n: N)    = D2[N, denomination.type](n)

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
  case object XYZ extends Denomination // FIXME doesn't find problem b/c lazy init

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

  val d20 = USD.d2(20)
  val d21 = USD.d2(21)

  val e20 = EUR.d2(20)
}

private[model] object UglyTypeDefs {
  type Code = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
}
