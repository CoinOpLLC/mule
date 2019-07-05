package io.deftrade
package model
package keys

import io.deftrade.model.refinements.Isin

import cats.{ Order }
import cats.instances.string._

import scala.util.matching.Regex

sealed trait Derivative { self: InstrumentIdentifier =>
  def underlyer: InstrumentIdentifier
}
object Derivative

sealed trait InstrumentIdentifier { def isin: Isin }

object InstrumentIdentifier {

  // A FixedCouponBond or CapitalIndexedBond.
  case class Bond(val isin: Isin) extends InstrumentIdentifier
  // A BondFuture.
  case class BondFuture(val isin: Isin) extends InstrumentIdentifier
  // A BondFutureOption.
  case class BondFutureOption(val isin: Isin) extends InstrumentIdentifier
  // A BulletPayment.
  case class BulletPayment(val isin: Isin) extends InstrumentIdentifier
  // A product only used for calibration.
  case class Calibration(val isin: Isin) extends InstrumentIdentifier
  // CreditAccount Default Swap (CDS)
  case class Cds(val isin: Isin) extends InstrumentIdentifier
  // CDS index
  case class CdsIndex(val isin: Isin) extends InstrumentIdentifier
  // Constant Maturity Swap (CMS)
  case class Cms(val isin: Isin) extends InstrumentIdentifier
  // A Deliverable Swap Forward
  // https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf
  case class Dsf(val isin: Isin) extends InstrumentIdentifier
  // Exchange Traded Derivative - Future (ETD)
  case class EtdFuture(val isin: Isin) extends InstrumentIdentifier // FIXME: this conflicts wtih mine...
  // Exchange Traded Derivative - Option (ETD)
  case class EtdOption(val isin: Isin) extends InstrumentIdentifier
  // Forward Rate Agreement
  case class Fra(val isin: Isin) extends InstrumentIdentifier
  // FX Non-Deliverable Forward
  case class FxNdf(val isin: Isin) extends InstrumentIdentifier
  // A FxSingle.
  case class FxSingle(val isin: Isin) extends InstrumentIdentifier
  // A FxSingleBarrierOption.
  case class FxSingleBarrierOption(val isin: Isin) extends InstrumentIdentifier
  // A FxSwap.
  case class FxSwap(val isin: Isin) extends InstrumentIdentifier
  // A FxVanillaOption.
  case class FxVanillaOption(val isin: Isin, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative
  // A IborCapFloor.
  case class IborCapFloor(val isin: Isin) extends InstrumentIdentifier
  // A IborFuture.
  case class IborFuture(val isin: Isin) extends InstrumentIdentifier
  // A IborFutureOption.
  case class IborFutureOption(val isin: Isin) extends InstrumentIdentifier
  // // A representation based on sensitivities.
  case class Sensitivities(val isin: Isin) extends InstrumentIdentifier
  // A Swap.
  case class Swap(val isin: Isin) extends InstrumentIdentifier
  // A Swaption.
  case class Swaption(val isin: Isin) extends InstrumentIdentifier
  // A TermDeposit.
  case class TermDeposit(val isin: Isin) extends InstrumentIdentifier

  case class AmortizingLoan(val isin: Isin)  extends InstrumentIdentifier
  case class ConvertibleLoan(val isin: Isin) extends InstrumentIdentifier

  case class CommonStock(val isin: Isin)    extends InstrumentIdentifier
  case class PreferredStock(val isin: Isin) extends InstrumentIdentifier

  case class StockIndexFutureOption(val isin: Isin, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative
  case class StockIndexOption(val isin: Isin, val underlyer: InstrumentIdentifier)       extends InstrumentIdentifier with Derivative
  case class StockIndexFuture(val isin: Isin, val underlyer: InstrumentIdentifier)       extends InstrumentIdentifier with Derivative
  case class StockOption(val isin: Isin, val underlyer: InstrumentIdentifier)            extends InstrumentIdentifier with Derivative

  implicit lazy val order: Order[InstrumentIdentifier] = Order by (_.isin.value)
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
