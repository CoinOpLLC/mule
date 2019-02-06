package io.deftrade
package money

import eu.timepit.refined
import refined.api.Refined

import spire.math.{ Fractional, Integral }

import enumeratum._

import cats.kernel.{ CommutativeGroup, Order }
import cats.{ Invariant, Show }

import BigDecimal.RoundingMode._

/**  */
private[money] sealed trait CurrencyLike extends EnumEntry { monetary =>

  type Type
  def apply[N: Financial](n: N): Money[N, Type]
  final private lazy val jc = java.util.Currency getInstance monetary.entryName // DRY-fu

  final def currencyCode: String = jc.getCurrencyCode
  final def numericCode: Int     = jc.getNumericCode
  final def displayName: String  = jc.getDisplayName
  final def symbol: String       = jc.getSymbol
  final def defaultFractionDigits: Int = jc.getDefaultFractionDigits match {
    case -1 => 4 // typical use case is unit-of-account: no real std, just need a decent default
    case fd => fd
  }

  /**
    * pip: percentage in point https://en.wikipedia.org/wiki/Percentage_in_point
    */
  final def pipScale: Int = defaultFractionDigits + 2

  final def pip: BigDecimal = BigDecimal(0L, scale = pipScale).ulp // ulp := unit of least precision

  def fractionDigits: Int = defaultFractionDigits
  def rounding: RoundingMode = monetary.entryName match {
    case "JPY" => DOWN
    case _     => HALF_UP
  }
}

private[money] object CurrencyLike {
  import cats.instances.string._
  implicit lazy val order: cats.Order[CurrencyLike] = cats.Order by { _.currencyCode }

}

/**
  * Per-real-world-instance of ISO 4217 currencies.
  */
sealed trait Currency[C] extends CurrencyLike { self =>

  type Type = C

  /**
    * Grant of exclusive license to create `Money[N, C]` instances
    * is hearby made to the implicit instance of `Currency[C]`.
    * (This feels regrettably clever.)
    */
  private[this] implicit def C: Currency[C] = self

  /**
    * _fiat bux_
    */
  def apply[N: Financial](n: N) = Money[N, C](n)

  import pricing._

  /**
    * Exchange `Rate` factory. Implicit context provides pricing.
    */
  def /[C2](cb: Currency[C2])(implicit Q: C QuotedIn C2): Rate[C, C2] = {
    implicit val C2 = cb
    Rate[C, C2]
  }

}

object Currency extends Enum[CurrencyLike] {

  /**
    * Three letter codes: 26 ^ 3 = 17576
    * over two hundred assigned; several "dead" currencies (not reused)
    * Market convention: `ABC/XYZ`: buy or sell units of `ABC`, quoted in `XYZ`.
    */
  type CodePattern = refined.W.`"[A-Z]{3}"`.T
  type CodeRx      = refined.string.MatchesRegex[CodePattern]
  type Code        = String Refined CodeRx

  /**
    * Given a currency (phantom) type, get a `Currency` instance.
    */
  def apply[C: Currency]: Currency[C] = implicitly

  /**
    * The Majors are: EUR/USD, USD/JPY, GBP/USD, AUD/USD, USD/CHF, NZD/USD and USD/CAD. (wiki)
    * standard major currency order via objectlabkit
    * TODO: can we find a reference other than objectlabkit?
    */
  final class EUR
  case object EUR extends Currency[EUR]
  implicit def eur: Currency[EUR] = EUR

  final class GBP
  case object GBP extends Currency[GBP]
  implicit def gbp: Currency[GBP] = GBP

  final class AUD
  case object AUD extends Currency[AUD]
  implicit def aud: Currency[AUD] = AUD

  final class NZD
  case object NZD extends Currency[NZD]
  implicit def nzd: Currency[NZD] = NZD

  final class USD
  case object USD extends Currency[USD]
  implicit def usd: Currency[USD] = USD

  final class CAD
  case object CAD extends Currency[CAD]
  implicit def cad: Currency[CAD] = CAD

  final class CHF
  case object CHF extends Currency[CHF]
  implicit def chf: Currency[CHF] = CHF

  final class NOK
  case object NOK extends Currency[NOK]
  implicit def nok: Currency[NOK] = NOK

  final class SEK
  case object SEK extends Currency[SEK]
  implicit def sek: Currency[SEK] = SEK

  final class JPY
  case object JPY extends Currency[JPY]
  implicit def jpy: Currency[JPY] = JPY

  // TODO: MOAR...

  val values = findValues
}
