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

sealed trait Currency extends EnumEntry {
  lazy val jvm: JvmCurrency = JvmCurrency getInstance entryName
}

object Currency extends Enum[Currency] {

  val values = findValues // (sic) macro magicke

  case object USD extends Currency
  case object EUR extends Currency
  case object JPY extends Currency
  case object AUD extends Currency
  case object XAU extends Currency

  implicit val eq = Eq.fromUniversalEquals[Currency]

  def of[C <: Currency]: C = ???
}

sealed trait Denominated[N, +C <: Currency] extends Any { self: Product =>
  type AmountType = N
  def amount: N
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  final def currency: C = (Currency withName self.productPrefix).asInstanceOf[C]
}

private[model] final case class USD[N](override val amount: N) extends AnyVal with Denominated[N, Currency.USD.type]
private[model] final case class EUR[N](override val amount: N) extends AnyVal with Denominated[N, Currency.EUR.type]
private[model] final case class JPY[N](override val amount: N) extends AnyVal with Denominated[N, Currency.JPY.type]
private[model] final case class AUD[N](override val amount: N) extends AnyVal with Denominated[N, Currency.AUD.type]
private[model] final case class XAU[N](override val amount: N) extends AnyVal with Denominated[N, Currency.XAU.type]

object Denominated {
  trait Amount[C <: Currency] {
    def apply[N: Numeric]: N => Denominated[N, C]
    def is[N: Numeric](amount: N): Denominated[N, C] = amount |> apply[N]
  }
  def in[C <: Currency](c: C) = new Amount[C] {
    override def apply[N: Numeric]: N => Denominated[N, C] =
      amount =>
        c match {
          case Currency.USD => USD(amount)
          case Currency.EUR => EUR(amount)
          case Currency.JPY => JPY(amount)
          case Currency.AUD => AUD(amount)
          case Currency.XAU => XAU(amount)
      }
  }

  def unapply[N: Numeric, C <: Currency](dn: Denominated[N, C]): Option[(N, C)] = (dn.amount, dn.currency).some

  private type CaseDenominated[N, C <: Currency] = Denominated[N, C] with Product
  implicit def eq[N: Numeric, C <: Currency, CD <: CaseDenominated[N, C]]: Eq[CD] = Eq.fromUniversalEquals[CD]

  implicit def monoid[N: Numeric, C <: Currency] = new Monoid[Denominated[N, C]] {
    val N = implicitly[Numeric[N]]
    type CD = Denominated[N, C]
    override def combine(a: CD, b: CD): CD = Denominated in Currency.of[C] is N.plus(a.amount, b.amount)
    override lazy val empty: CD            = Denominated in Currency.of[C] is N.zero
  }
}

object Examples {
  val dollarMonoid = Monoid[Denominated[BigDecimal, Currency.USD.type]]

  val bux = Denominated in Currency.USD is 555.55
  // val bux       = 555 |> buckMaker

}
