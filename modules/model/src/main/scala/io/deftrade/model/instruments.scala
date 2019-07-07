package io.deftrade
package model
package keys

import keyval._, model.refinements.{ Isin, Psin, Usin }

import cats.{ Eq, Hash, Order, Show }
import cats.instances.string._

import io.circe.Json

/**
  * `Instrument`s in the house.
  * TODO:
  * - use the XBRL definitions for these, a la OpenGamma
  * - see implementations in `Refine` library
  */
final case class Instrument(displayName: String, meta: Json)

object Instrument {
  type Key = keys.InstrumentIdentifier
  val Key                         = keys.InstrumentIdentifier
  implicit def eq: Eq[Instrument] = Eq by (_.meta)
}

sealed trait InstrumentIdentifier { def usin: Usin }

object InstrumentIdentifier extends Basics with Exotics with Fx with Ibor with Lending

sealed trait Derivative { self: InstrumentIdentifier =>
  def underlyer: InstrumentIdentifier
}
object Derivative

trait CapitalStack {

  import refinements.Ein

  final case class CommonStock(usin: Usin, ein: Ein) extends InstrumentIdentifier

  final case class PreferredStock(val usin: Usin, ein: Ein) extends InstrumentIdentifier

  // A FixedCouponBond or CapitalIndexedBond.
  // Question: What is the EIN of the United States Treasury?
  // We will need to use one to represent that Entity. :|
  final case class Bond(val usin: Usin, ein: Ein) extends InstrumentIdentifier
}

trait Basics {

  case class Index(usin: Usin) extends InstrumentIdentifier
  case class TreasurySecurity(isin: Isin)

  case class EtdFuture(val usin: Usin) extends InstrumentIdentifier // FIXME: this conflicts wtih mine...
  // Exchange Traded Derivative - Option (ETD)
  case class EtdOption(val usin: Usin) extends InstrumentIdentifier
  // I mean, right?
  case class EtdFutureOption(val usin: Usin, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative

  /** FIXME: "Index" anything should reference the index - need a Table of Indexes */
  case class IndexOption(val usin: Usin, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative

  case class StockOption(val usin: Usin, val underlyer: InstrumentIdentifier) extends InstrumentIdentifier with Derivative

  case class BondFuture(val usin: Usin) extends InstrumentIdentifier
  // A BondFutureOption.
  case class BondFutureOption(val usin: Usin) extends InstrumentIdentifier
  // Exchange Traded Derivative - Future (ETD)

  implicit lazy val order: Order[InstrumentIdentifier] = Order by (_.usin.value)
}

trait Exotics {
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
  // Forward Rate Agreement
  case class Fra(val usin: Usin) extends InstrumentIdentifier
  // // A representation based on sensitivities.
  case class Sensitivities(val usin: Usin) extends InstrumentIdentifier
  // A Swap.
  case class Swap(val usin: Usin) extends InstrumentIdentifier
  // A Swaption.
  case class Swaption(val usin: Usin) extends InstrumentIdentifier
}

trait Fx {
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
}

trait Ibor {
  // A IborCapFloor.
  case class IborCapFloor(val usin: Usin) extends InstrumentIdentifier
  // A IborFuture.
  case class IborFuture(val usin: Usin) extends InstrumentIdentifier
  // A IborFutureOption.
  case class IborFutureOption(val usin: Usin) extends InstrumentIdentifier
}

trait Lending {
  // A BulletPayment.
  case class BulletPayment(val usin: Usin) extends InstrumentIdentifier
  // A TermDeposit.
  case class TermDeposit(val usin: Usin) extends InstrumentIdentifier

  case class AmortizingLoan(val usin: Usin) extends InstrumentIdentifier

  case class ConvertibleNote(val usin: Usin) extends InstrumentIdentifier
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
