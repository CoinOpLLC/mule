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
package money

import keyval.CsvEnum

import cats.instances.string._
import cats.Order

import eu.timepit.refined
import refined.api.Refined

import enumeratum.{ Enum, EnumEntry }

import BigDecimal.RoundingMode.{ DOWN, HALF_UP, RoundingMode }

/**
  * Unparameterized [[Currency]] base class.
  *
  * Implemements the bulk of the functionality, and so, is useful in its own right.
  */
sealed trait CurrencyLike extends Numéraire.InCurrency with EnumEntry with Serializable { self =>

  /** instance phantom type representing currency */
  type Type

  /**
    * Grant of exclusive license to create `Money[N, C]` instances
    * is hearby made to the implicit instance of `Currency[C]`.
    *
    * (This feels regrettably clever.)
    */
  protected[this] implicit def C: Currency[Type]

  /**
    * And the Fed said: ''fiat bux''...
    */
  final def fiat[N: Financial](n: N) = Money[N, Type](n)

  /** */
  final def apply[N: Financial](n: N) = fiat(n)

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  final def code: Currency.Code = jc.getCurrencyCode.asInstanceOf[Currency.Code]

  /** */
  final def numericCode: Int = jc.getNumericCode

  /** */
  final def displayName: String = jc.getDisplayName

  /** */
  final def symbol: String = jc.getSymbol

  /** */
  final def defaultFractionDigits: Int = jc.getDefaultFractionDigits match {
    case -1 => 4 // typical use case is unit-of-account: no real std, just need a decent default
    case fd => fd
  }

  /**
    * pip: percentage in point https://en.wikipedia.org/wiki/Percentage_in_point
    */
  final def pip: BigDecimal = BigDecimal(0L, scale = pipScale).ulp // ulp := unit of least precision

  /**
    * Typically, two digits more than typical for a given currency but CHECK THE SOURCE k thx.
    */
  final def pipScale: Int = defaultFractionDigits + 2

  /** */
  def fractionDigits: Int = defaultFractionDigits

  /** */
  def rounding: RoundingMode = self.entryName match {
    case "JPY" => DOWN
    case _     => HALF_UP
  }

  /** */
  final private lazy val jc = java.util.Currency getInstance self.entryName
}

/**  */
object CurrencyLike {

  import refined.cats._

  /** */
  implicit lazy val order: Order[CurrencyLike] = Order by { _.code }
}

/**
  */
sealed trait Currency[C] extends CurrencyLike { self =>

  /** */
  final type Type = C

  override protected[this] implicit def C: Currency[Type] = self
}

/**
  * Some of the more common ISO 4217 currencies.
  */
object Currency extends Enum[CurrencyLike] with CsvEnum[CurrencyLike] { self =>

  /**
    * Three letter codes: 26 ^ 3 = 17576
    * over two hundred assigned; several "dead" currencies (not reused)
    * Market convention: `ABC/XYZ`: buy or sell units of `ABC`, quoted in `XYZ`.
    */
  type CodePattern = refined.W.`"[A-Z]{3}"`.T

  /** */
  type IsCode = refined.string.MatchesRegex[CodePattern]

  /** */
  type Code = String Refined IsCode

  /**
    * Given a currency (phantom) type, get a `Currency` instance.
    */
  def apply[C](implicit C: Currency[C]): Currency[C] = C

  /** */
  def unapply(n: Numéraire): Option[CurrencyLike] = n match {
    case _ if values contains n => ???
    case _                      => None
  }

  /**
    * The Majors are: EUR/USD, USD/JPY, GBP/USD, AUD/USD, USD/CHF, NZD/USD and USD/CAD. (wiki)
    * standard major currency order via objectlabkit
    * TODO: can we find a reference other than objectlabkit?
    * This still smells bonkers. If it flies I want to call it the Wall of Types pattern.
    * nb {{{
    *   import io.deftrade.money.Currency.USD
    * }}}
    * will pull the {{{
    *   implicit def usd: Currency[USD]
    * }}}
    * into scope due to the implicit resolution search path.
    */
  final class EUR private[money] ()
  case object EUR extends Currency[EUR]
  implicit def eur: Currency[EUR] = EUR

  final class GBP private[money] ()
  case object GBP extends Currency[GBP]
  implicit def gbp: Currency[GBP] = GBP

  final class AUD private[money] ()
  case object AUD extends Currency[AUD]
  implicit def aud: Currency[AUD] = AUD

  final class NZD private[money] ()
  case object NZD extends Currency[NZD]
  implicit def nzd: Currency[NZD] = NZD

  final class USD private[money] ()
  case object USD extends Currency[USD]
  implicit def usd: Currency[USD] = USD

  final class CAD private[money] ()
  case object CAD extends Currency[CAD]
  implicit def cad: Currency[CAD] = CAD

  final class CHF private[money] ()
  case object CHF extends Currency[CHF]
  implicit def chf: Currency[CHF] = CHF

  final class NOK private[money] ()
  case object NOK extends Currency[NOK]
  implicit def nok: Currency[NOK] = NOK

  final class SEK private[money] ()
  case object SEK extends Currency[SEK]
  implicit def sek: Currency[SEK] = SEK

  final class JPY private[money] ()
  case object JPY extends Currency[JPY]
  implicit def jpy: Currency[JPY] = JPY

  // TODO: MOAR...

  /** */
  val values = findValues
}
