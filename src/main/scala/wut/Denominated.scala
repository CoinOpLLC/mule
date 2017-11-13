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

// import enumeratum._
// import enumeratum.values._

//
import eu.timepit.refined
import refined.api.Refined
import refined.W
// import refined.collection._
// import refined.numeric._
import refined.auto._

// TODO: try a typeclass for currency,
// use an implicit def to turn the type bounds into a set of instances...

// Idea: `Denomination[C <: JvmCurrency]` typeclass. Construct money from their (implicit) instances.

/*
 * This is an old pattern actually; Enums as factories...
 * and another oldie: phantom types for the value class, to allow it to carry the currency "masslessly"
 */
/** This wants to be a value class someday (it told me so). */
import java.util.Currency
import scala.language.higherKinds

sealed trait Denomination {

  protected def jc: Currency

  type DnType[µ] <: Denominated[µ]

  final def code: String        = jc.getCurrencyCode
  final def numericCode: Int    = jc.getNumericCode
  final def fractionDigits: Int = jc.getDefaultFractionDigits
  final def displayName: String = jc.getDisplayName
  final def symbol: String      = jc.getSymbol

}

object Denomination {
  implicit def eqN[N: Numeric]: Eq[N] = Eq.fromUniversalEquals[N] // where should this go?
}

sealed trait Denominated[N] extends Any {
  type D <: Denomination
  def amount: N
  def denomination: D

  final override def toString: String = s"${denomination.code}($amount)"

}

object USD extends Denomination {

  override protected lazy val jc = Currency getInstance "USD"

  def apply[N: Numeric](amount: N): USD[N]        = new USD[N](amount)
  def unapply[N: Numeric](usd: USD[N]): Option[N] = usd.amount.some

  implicit def eq[N: Numeric]: Eq[USD[N]] = Eq.fromUniversalEquals[USD[N]]

  implicit def monoid[N: Numeric]: Monoid[USD[N]] = new Monoid[USD[N]] {
    val N                                              = implicitly[Numeric[N]]
    override def combine(a: USD[N], b: USD[N]): USD[N] = apply(N.plus(a.amount, b.amount))
    override lazy val empty: USD[N]                    = apply(N.zero)
  }
}

final class USD[N] private (val amount: N) extends AnyVal with Denominated[N] {
  type AmountType = N
  type D          = USD.type
  override def denomination = USD
}

object EUR extends Denomination {

  override protected lazy val jc = Currency getInstance "EUR"

  def apply[N: Numeric](amount: N): EUR[N]        = new EUR[N](amount)
  def unapply[N: Numeric](usd: EUR[N]): Option[N] = usd.amount.some

  implicit def eq[N: Numeric]: Eq[EUR[N]] = Eq.fromUniversalEquals[EUR[N]]

  implicit def monoid[N: Numeric]: Monoid[EUR[N]] = new Monoid[EUR[N]] {
    val N                                              = implicitly[Numeric[N]]
    override def combine(a: EUR[N], b: EUR[N]): EUR[N] = apply(N.plus(a.amount, b.amount))
    override lazy val empty: EUR[N]                    = apply(N.zero)
  }
}

final class EUR[N] private (val amount: N) extends AnyVal with Denominated[N] {
  type AmountType = N
  type D          = EUR.type
  override def denomination = EUR
}

object Denominated { // FIXME this is fuxd: apply() can't return a specific enough return type
  def apply[N: Numeric, D <: Denomination](n: N, d: D): Denominated[N] = d match {
    case USD => USD(n)
  }

}

object Examples {
  import Denomination._
  // val buxf: Int => USD[Int] = Denominated[Int, USD[Int]](USD)
  // val bux20                 = 20 |> buxf
  // val bux20too              = 20 |> buxf
  // bux20 === bux20too |> assert

  val benjAhMin = USD(100)
  val benjOhOne = USD(101)
  val e20       = EUR(20)

  benjAhMin =!= benjOhOne |> assert
  e20 === EUR(20)         |> assert
  val x = Denominated(20, EUR)
  // x === e20 |> assert FIXME
}

private[model] object UglyTypeDefs {
  type CurrencyCode = String Refined refined.string.MatchesRegex[W.`"[A-Z]{3}"`.T]
}
