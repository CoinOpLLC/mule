package io.deftrade
package money

import eu.timepit.refined
import refined.api.Refined

import spire.math.{ Fractional, Integral }

import enumeratum._

import cats.kernel.{ CommutativeGroup, Order }
import cats.{ Invariant, Show }

import BigDecimal.RoundingMode._

/**
  * `Money` is a naked number, with a phantom currency type.
  */
final class Money[N, C] private (val amount: N) extends AnyVal { lhs =>

  type MNY = Money[N, C]

  def +(rhs: MNY)(implicit N: Financial[N]) =
    new Money[N, C](N.fractional plus (amount, rhs.amount))

  def -(rhs: MNY)(implicit N: Financial[N]) =
    new Money[N, C](N.fractional minus (amount, rhs.amount))

  def *[S](scale: S)(implicit N: Financial[N], S: Financial[S], C: Currency[C]) =
    Money[N, C](N.fractional times (S to [N] scale, amount))

  def /(rhs: MNY)(implicit N: Financial[N]): N = N.fractional div (lhs.amount, rhs.amount)

  def unary_-(implicit N: Financial[N], C: Currency[C]): MNY = Money(N.fractional negate amount)

  override def toString: String = amount.toString
}

/**
  * `Money[?, C]: Order: Show: CommutativeGroup`
  */
object Money {

  def apply[N: Financial, C: Currency](amount: N) = new Money[N, C](Financial[N].round[C](amount))

  // implicit def fixmeIKnowIShouldBeAbleToDoThisButHow[N: Financial]: Order[N] = ??? // FIXME

  implicit def orderMoney[N: Financial: Order, C: Currency]: Order[Money[N, C]] =
    // FIXME: should be able to get this from spire::Fractional
    // or failing that stuff `Order` into `Financial` somehow
    Order by (_.amount)

  // toString is limited due to value class implementation
  // But we can implement Show[Money[N, C]] for all the N and C we care about.
  implicit def showMoney[N: Financial, C: Currency]: Show[Money[N, C]] =
    Show show (Format apply _)

  implicit def commutativeGroupMoney[N: Financial, C: Currency]: CommutativeGroup[Money[N, C]] =
    Invariant[CommutativeGroup].imap(Financial[N].commutativeGroup)(Currency[C] apply _)(_.amount)

  object Format {
    private val flags = """#,(""" // decimal-separator, grouping-separators, parens-for-negative
    def apply[N: Financial, C: Currency](m: Money[N, C]): String = {
      val C    = Currency[C]
      val fmt  = s"%${flags}.${C.fractionDigits}f" // TODO: this hack won't extend
      val sfmt = if ((Financial[N].fractional signum m.amount) < 0) fmt else s" $fmt "
      s"${C.currencyCode} ${m.amount formatted sfmt}"
    }
  }

  /** section */
  import refined.api.{ RefType, Validate }
  implicit lazy val refinedRefType: RefType[Money] =
    new RefType[Money] {

      private type F[T, P] = Money[T, P]

      def unsafeWrap[T, P](t: T): F[T, P] = new Money[T, P](t)

      def unwrap[T](tp: F[T, _]): T = tp.amount

      def unsafeRewrap[T, A, B](ta: F[T, A]): F[T, B] = ta |> unwrap |> unsafeWrap
    }

  implicit def refinedValidate[T: Financial, P: Currency]: Validate[T, P] =
    Validate alwaysPassed Currency[P]
}

sealed class Financial[N: Fractional] {

  implicit lazy val fractional                       = Fractional[N]
  implicit def commutativeGroup: CommutativeGroup[N] = fractional.additive

  def fromBigDecimal(bd: BigDecimal): N = fractional.fromBigDecimal(bd)
  def toBigDecimal(n: N): BigDecimal    = fractional.toBigDecimal(n)

  def fromLong(l: Long): N = fractional.fromLong(l)

  /** hack for pattern matching */
  lazy val Zero = fractional.zero
  lazy val One  = fractional.one

  def from[T: Financial](t: T): N = t |> Financial[T].toBigDecimal |> fromBigDecimal

  def to[R: Financial](n: N): R = n |> toBigDecimal |> Financial[R].fromBigDecimal

  /**
    * Someday, maybe, "as if by" will be the operative words.
    * But today, we go with "literally".
    * How do we deal with scale and significant digits?
    * Simple rule: the left operand scale is "sticky" for those operations {+, -, *} that
    * ultimately result in Money.

  # [XBR: Precision, Decimals and Units 1.0](http://www.xbrl.org/WGN/precision-decimals-units/WGN-2017-01-11/precision-decimals-units-WGN-2017-01-11.html)

  > 6.3
  ...Another related issue is the desire to express the exact value of certain ratios that cannot be exactly represented in a decimal representation. This requirement arises from the Tax domain space. Specific examples from the UK Inland Revenue (now HMRC) are marginal relief rates (varying between 9/400 and 1/40 in the late 1990s) and a special tax rate of 22.5/77.5. This implies the need for the fractionItemType in XBRL (where the numerator and denominator are always exact).

  This suggests a Rational type e.g. from spire

  7.4 Representing Exact Currency Amounts
    */
  def round[C](n: N)(implicit C: Currency[C]): N = {
    def round(bd: BigDecimal): BigDecimal = bd setScale (C.fractionDigits, C.rounding)
    n |> toBigDecimal |> round |> fromBigDecimal
  }

  /** Extractors and (any) other tools for dealing with integral quantities. */
  object IntegralIs {

    // TODO:
    def unapply[I](n: N)(implicit I: Integral[N]): Option[N] = ???
  }
}

/**
  */
object Financial {

  def apply[N: Financial]: Financial[N] = implicitly

  implicit object DoubleIsFinancial     extends Financial[Double]
  implicit object BigDecimalIsFinancial extends Financial[BigDecimal]

}

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

/**
  * - `QuotedIn` is a binary typeclass: `A QuotedIn B`. The domain vocabulary usage supports this.
  * - can come from a variety of sources including live market
  * - "Orderly market" invariant: `ask` < `bid`.
  * - must model disorderly markets: not everything that comes at you down the wire makes sense.
  * - note: the types parameters `A` and `C` are effectively *phantom types*
  *   - used summon a pricing instance (e.g. QuotedIn[SomeShadySpeculativeInstrument, CHF]))
  *   - C is intended (but not required by this base type) to represent a currency, and will
  * typically have a Currency[C] typeclass instance
  *
  * Domain consideration: `Currency` _exchange depends on _pricing_ of some kind.
  * One or more Market(s) determine this price.
  * - A design that doesnt' abstract over `QuotedIn`, **including live data**, is useless.
  * - otoh dead simple immutable for testing / demo also required
  */
trait QuotedIn[A, C] extends Any {

  def ask: BigDecimal
  def bid: BigDecimal

  def tick(implicit C: Currency[C]): BigDecimal

  def isDerived: Boolean = false

  type CrossType
  def cross: Option[CrossType] = None

  @inline final def spread = ask - bid
  @inline final def mid    = bid + spread / 2
}
object QuotedIn {
  def apply = ???

  // FIXME: get rid of implicit case class param
  final case class Spread[A, C2](val ask: BigDecimal, val bid: BigDecimal) extends QuotedIn[A, C2] {
    def tick(implicit C2: Currency[C2]): BigDecimal = C2.pip //  / 10 // this is a thing now
  }
  final case class TradeQuote[A, C2: Currency](val trade: BigDecimal) extends /* AnyVal with */ QuotedIn[A, C2] {
    def bid: BigDecimal                             = trade
    def ask: BigDecimal                             = trade
    def tick(implicit C2: Currency[C2]): BigDecimal = C2.pip //  / 10 // this is a thing now
  }
}

// TODO: a case class Quote (immutable value version of `Rate`)

/** TODO: wtf is this about, really? */
final case class Rate[C1, C2]()(implicit
                                C1: Currency[C1],
                                C2: Currency[C2],
                                Q: C1 QuotedIn C2) {
  import Q._
  @inline def buy[N: Financial](m1: Money[N, C1]): Money[N, C2]   = convert(m1, ask)
  @inline def sell[N: Financial](m1: Money[N, C1]): Money[N, C2]  = convert(m1, bid)
  @inline def apply[N: Financial](m1: Money[N, C1]): Money[N, C2] = convert(m1, mid)

  def quote[N: Financial]: (Money[N, C2], Money[N, C2]) = {
    val single = C1(Financial[N].fractional.one)
    (buy(single), sell(single))
  }

  def description: String = s"""
        |Quoter buys  ${C1} and sells ${C2} at ${bid}
        |Quoter sells ${C1} and buys  ${C2} at ${ask}""".stripMargin

  private def convert[N: Financial](m1: Money[N, C1], rate: BigDecimal): Money[N, C2] = {
    val N = Financial[N]
    C2(m1.amount |> N.toBigDecimal |> (_ * rate) |> N.fromBigDecimal)
  }
}
