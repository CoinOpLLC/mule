package wut
package model

import scala.language.implicitConversions

import java.util.{ Currency => JvmCurrency }
// import scala.util.Try

import cats.{ Eq, Monoid }
import cats.syntax.all._
// import cats.{ instances => ci }
// import ci.int._, ci.string._, ci.double._, ci.boolean._, ci.bigDecimal._, ci.long._
// import ci.option._, ci.tuple._, ci.list._, ci.set._, ci.map._

import enumeratum._
// import enumeratum.values._

//
// import eu.timepit.refined
// import refined.api.Refined
// import refined.W
// import refined.collection._
// import refined.numeric._
// import refined.auto._

// TODO: try a typeclass for currency,
// use an implicit def to turn the type bounds into a set of instances...

sealed trait Currency extends EnumEntry {
  type C <: Currency
  def apply[N: Numeric](amount: N): Denominated[N, C]
  final lazy val jvm: JvmCurrency = JvmCurrency getInstance entryName
}

object Currency extends Enum[Currency] {

  val values = findValues // (sic) macro magicke

  case object USD extends Currency {
    def apply[N: Numeric](a: N): Denominated[N, USD] = impl.USD[N](a)
  }
  type USD = USD.type
  case object EUR extends Currency
  case object JPY extends Currency
  case object AUD extends Currency
  case object XAU extends Currency

  implicit val eq = Eq.fromUniversalEquals[Currency]

  // this is the problem right here... we capture a type and there's no way to tie it Back
  // to the correct _dependent type... which is what the methods now produce...
  def of[C <: Currency]: C = ???
}

sealed trait Denominated[N, +C <: Currency] extends Any { self: Product =>
  type AmountType = N
  def amount: N
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  final def currency: C = (Currency withName self.productPrefix).asInstanceOf[C]
}

package impl {
  final case class USD[N](val amount: N) extends AnyVal with Denominated[N, Currency.USD]
  // final case class EUR[N](val amount: N) extends AnyVal with Denominated[N, Currency.EUR.type]
  // final case class JPY[N](val amount: N) extends AnyVal with Denominated[N, Currency.JPY.type]
  // final case class AUD[N](val amount: N) extends AnyVal with Denominated[N, Currency.AUD.type]
  // final case class XAU[N](val amount: N) extends AnyVal with Denominated[N, Currency.XAU.type]
}
object Denominated {
  def apply[N: Numeric, C <: Currency](n: N, c: C): Denominated[N, c.C] = c(n)
  trait Amount[C <: Currency] {
    def apply[N: Numeric]: N => Denominated[N, C]
    def is[N: Numeric](amount: N): Denominated[N, C] = amount |> apply[N]
  }
  def in[C <: Currency](currency: C) = new Amount[currency.C] {
    override def apply[N: Numeric]: N => Denominated[N, currency.C] =
      amount => currency(amount)
  }

  // def [N: Numeric, C <: Currency]
  def unapply[N: Numeric, C <: Currency](dn: Denominated[N, C]): Option[(N, C)] = (dn.amount, dn.currency).some

  private type CaseDenominated[N, C <: Currency] = Denominated[N, C] with Product
  implicit def eq[N: Numeric]: Eq[N] = Eq.fromUniversalEquals[N]
  implicit def eq[N: Numeric, C <: Currency]: Eq[Denominated[N, C]] = Eq.by(unapply(_))

  implicit def monoid[N: Numeric, C <: Currency] = new Monoid[Denominated[N, C]] {
    val N = implicitly[Numeric[N]]
    type CD = Denominated[N, C]
    override def combine(a: CD, b: CD): CD = Denominated in Currency.of[C] is N.plus(a.amount, b.amount)
    override lazy val empty: CD            = Denominated in Currency.of[C] is N.zero
  }

}

object Examples {
  val dollarMonoid = Monoid[BigDecimal Denominated Currency.USD.type]

  val bux = Denominated in Currency.USD is 555.55
  val buxMaker = (Denominated in Currency.USD)[Double]
  val moarBux = 555.55 |> buxMaker
  bux === moarBux |> assert

}
