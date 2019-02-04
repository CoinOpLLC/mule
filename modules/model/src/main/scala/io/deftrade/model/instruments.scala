package io.deftrade
package model

import cats.{ Order }
import cats.instances.string._

import scala.util.matching.Regex

sealed trait UniversalInstrumentIdentifyer extends Serializable with Product {
  def s: String

  // FIXME: TODO: what this really needs is the `Refined` treatment (with `Validation`)
  def rx: Regex = ???
}

object UniversalInstrumentIdentifyer {
  // TODO: each of these needs a regex refinement for the string param
  case class Cusip(s: String) extends UniversalInstrumentIdentifyer
  case class Isin(s: String)  extends UniversalInstrumentIdentifyer
  // case class Ric(s: String)          extends UniversalInstrumentIdentifyer
  // case class Buid(s: String)         extends UniversalInstrumentIdentifyer
  case class IbContractId(s: String) extends UniversalInstrumentIdentifyer
  case class HouseId(s: String)      extends UniversalInstrumentIdentifyer

  // implicit lazy val eq = Eq.fromUniversalEquals[UniversalInstrumentIdentifyer]
  implicit lazy val order: Order[UniversalInstrumentIdentifyer] = Order by (_.s)
}

sealed trait Derivative { self: InstrumentIdentifier =>
  def underlyer: InstrumentIdentifier
}
object Derivative

sealed trait InstrumentIdentifier { def uii: UniversalInstrumentIdentifyer }

object InstrumentIdentifier {

  type UII = UniversalInstrumentIdentifyer

  // A FixedCouponBond or CapitalIndexedBond.
  case class Bond(val uii: UII) extends InstrumentIdentifier
  // A BondFuture.
  case class BondFuture(val uii: UII) extends InstrumentIdentifier
  // A BondFutureOption.
  case class BondFutureOption(val uii: UII) extends InstrumentIdentifier
  // A BulletPayment.
  case class BulletPayment(val uii: UII) extends InstrumentIdentifier
  // A product only used for calibration.
  case class Calibration(val uii: UII) extends InstrumentIdentifier
  // CreditAccount Default Swap (CDS)
  case class Cds(val uii: UII) extends InstrumentIdentifier
  // CDS index
  case class CdsIndex(val uii: UII) extends InstrumentIdentifier
  // Constant Maturity Swap (CMS)
  case class Cms(val uii: UII) extends InstrumentIdentifier
  // A Deliverable Swap Forward
  // https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf
  case class Dsf(val uii: UII) extends InstrumentIdentifier
  // Exchange Traded Derivative - Future (ETD)
  case class EtdFuture(val uii: UII) extends InstrumentIdentifier // FIXME: this conflicts wtih mine...
  // Exchange Traded Derivative - Option (ETD)
  case class EtdOption(val uii: UII) extends InstrumentIdentifier
  // Forward Rate Agreement
  case class Fra(val uii: UII) extends InstrumentIdentifier
  // FX Non-Deliverable Forward
  case class FxNdf(val uii: UII) extends InstrumentIdentifier
  // A FxSingle.
  case class FxSingle(val uii: UII) extends InstrumentIdentifier
  // A FxSingleBarrierOption.
  case class FxSingleBarrierOption(val uii: UII) extends InstrumentIdentifier
  // A FxSwap.
  case class FxSwap(val uii: UII) extends InstrumentIdentifier
  // A FxVanillaOption.
  case class FxVanillaOption(val uii: UII, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative
  // A IborCapFloor.
  case class IborCapFloor(val uii: UII) extends InstrumentIdentifier
  // A IborFuture.
  case class IborFuture(val uii: UII) extends InstrumentIdentifier
  // A IborFutureOption.
  case class IborFutureOption(val uii: UII) extends InstrumentIdentifier
  // // A representation based on sensitivities.
  case class Sensitivities(val uii: UII) extends InstrumentIdentifier
  // A Swap.
  case class Swap(val uii: UII) extends InstrumentIdentifier
  // A Swaption.
  case class Swaption(val uii: UII) extends InstrumentIdentifier
  // A TermDeposit.
  case class TermDeposit(val uii: UII) extends InstrumentIdentifier

  case class AmortizingLoan(val uii: UII)  extends InstrumentIdentifier
  case class ConvertibleLoan(val uii: UII) extends InstrumentIdentifier

  case class CommonStock(val uii: UII)    extends InstrumentIdentifier
  case class PreferredStock(val uii: UII) extends InstrumentIdentifier

  case class StockIndexFutureOption(val uii: UII, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative
  case class StockIndexOption(val uii: UII, val underlyer: InstrumentIdentifier)       extends InstrumentIdentifier with Derivative
  case class StockIndexFuture(val uii: UII, val underlyer: InstrumentIdentifier)       extends InstrumentIdentifier with Derivative
  case class StockOption(val uii: UII, val underlyer: InstrumentIdentifier)            extends InstrumentIdentifier with Derivative

  implicit lazy val order: Order[InstrumentIdentifier] = Order by (_.uii)
}

import io.deftrade.money.Financial

sealed trait Strike[N] extends Any { def value: N }
object Strike {

  final case class SimpleStrike[N] private (val value: N) extends AnyVal with Strike[N]
  object SimpleStrike {
    def apply[N: Financial](value: N): SimpleStrike[N] = new SimpleStrike(value)
  }

  final case class LogMoneynessStrike[N] private (val value: N) extends AnyVal with Strike[N]
  object LogMoneynessStrike {
    def apply[N: Financial](value: N): LogMoneynessStrike[N] = new LogMoneynessStrike(value)
    def apply[N: Financial](strike: N, forward: N): LogMoneynessStrike[N] =
      ??? // apply(ln(strike / forward))
  }
}
/*
object Isin extends App {
  val isins = Seq("US0378331005", "US0373831005", "U50378331005",
    "US03378331005", "AU0000XVGZA3","AU0000VXGZA3", "FR0000988040")

  private def ISINtest(isin: String): Boolean = {
    val isin0 = isin.trim.toUpperCase

    def luhnTestS(number: String): Boolean = {

      def luhnTestN(digits: Seq[Int]): Boolean = {

        def checksum(digits: Seq[Int]): Int = {
          digits.reverse.zipWithIndex
            .foldLeft(0) {
              case (sum, (digit, i)) =>
                if (i % 2 == 0) sum + digit
                else sum + (digit * 2) / 10 + (digit * 2) % 10
            } % 10
        }

        checksum(digits) == 0
      }

      luhnTestN(number.map { c =>
        assert(c.isDigit, s"$number has a non-digit error")
        c.asDigit
      })
    }

    if (!isin0.matches("^[A-Z]{2}[A-Z0-9]{9}\\d$")) false
    else {
      val sb = new StringBuilder
      for (c <- isin0.substring(0, 12)) sb.append(Character.digit(c, 36))
      luhnTestS(sb.toString)
    }
  }

  isins.foreach(isin => println(f"$isin is ${if (ISINtest(isin)) "" else "not"}%s valid"))

}

 */
