package io.deftrade
package model
package keys

import io.deftrade.model.refinements.{ Isin, Psin, Usin }

import cats.{ Order }
import cats.instances.string._

sealed trait Derivative { self: InstrumentIdentifier =>
  def underlyer: InstrumentIdentifier
}
object Derivative

sealed trait InstrumentIdentifier { def usin: Usin }

object InstrumentIdentifier {

  // A FixedCouponBond or CapitalIndexedBond.
  case class Bond(val usin: Usin) extends InstrumentIdentifier
  // A BondFuture.
  case class BondFuture(val usin: Usin) extends InstrumentIdentifier
  // A BondFutureOption.
  case class BondFutureOption(val usin: Usin) extends InstrumentIdentifier
  // A BulletPayment.
  case class BulletPayment(val usin: Usin) extends InstrumentIdentifier
  // A product only used for calibration.
  case class Calibration(val usin: Usin) extends InstrumentIdentifier
  // CreditAccount Default Swap (CDS)
  case class Cds(val usin: Usin) extends InstrumentIdentifier
  // CDS index
  case class CdsIndex(val usin: Usin) extends InstrumentIdentifier
  // Constant Maturity Swap (CMS)
  case class Cms(val usin: Usin) extends InstrumentIdentifier
  // A Deliverable Swap Forward
  // https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf
  case class Dsf(val usin: Usin) extends InstrumentIdentifier
  // Exchange Traded Derivative - Future (ETD)
  case class EtdFuture(val usin: Usin) extends InstrumentIdentifier // FIXME: this conflicts wtih mine...
  // Exchange Traded Derivative - Option (ETD)
  case class EtdOption(val usin: Usin) extends InstrumentIdentifier
  // Forward Rate Agreement
  case class Fra(val usin: Usin) extends InstrumentIdentifier
  // FX Non-Deliverable Forward
  case class FxNdf(val usin: Usin) extends InstrumentIdentifier
  // A FxSingle.
  case class FxSingle(val usin: Usin) extends InstrumentIdentifier
  // A FxSingleBarrierOption.
  case class FxSingleBarrierOption(val usin: Usin) extends InstrumentIdentifier
  // A FxSwap.
  case class FxSwap(val usin: Usin) extends InstrumentIdentifier
  // A FxVanillaOption.
  case class FxVanillaOption(val usin: Usin, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative
  // A IborCapFloor.
  case class IborCapFloor(val usin: Usin) extends InstrumentIdentifier
  // A IborFuture.
  case class IborFuture(val usin: Usin) extends InstrumentIdentifier
  // A IborFutureOption.
  case class IborFutureOption(val usin: Usin) extends InstrumentIdentifier
  // // A representation based on sensitivities.
  case class Sensitivities(val usin: Usin) extends InstrumentIdentifier
  // A Swap.
  case class Swap(val usin: Usin) extends InstrumentIdentifier
  // A Swaption.
  case class Swaption(val usin: Usin) extends InstrumentIdentifier
  // A TermDeposit.
  case class TermDeposit(val usin: Usin) extends InstrumentIdentifier

  case class AmortizingLoan(val usin: Usin)  extends InstrumentIdentifier
  case class ConvertibleLoan(val usin: Usin) extends InstrumentIdentifier

  case class CommonStock(val usin: Usin)    extends InstrumentIdentifier
  case class PreferredStock(val usin: Usin) extends InstrumentIdentifier

  case class StockIndexFutureOption(val usin: Usin, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative
  case class StockIndexOption(val usin: Usin, val underlyer: InstrumentIdentifier)       extends InstrumentIdentifier with Derivative
  case class StockIndexFuture(val usin: Usin, val underlyer: InstrumentIdentifier)       extends InstrumentIdentifier with Derivative
  case class StockOption(val usin: Usin, val underlyer: InstrumentIdentifier)            extends InstrumentIdentifier with Derivative

  implicit lazy val order: Order[InstrumentIdentifier] = Order by (_.usin.value)
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
