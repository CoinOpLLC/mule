package io.deftrade
package model
package capital

import money.{ CurrencyLike, Financial }
import time.ZonedDateTime
import model.refinements.{ Isin, Mic, Usin, VarChar }
import keyval._

// import cats.{ Eq, Hash, Order, Show }
import cats.instances.string._

// import shapeless.syntax.singleton._

import io.circe.Json

/**
  * `Instrument`s in the house.
  * TODO:
  * - use the XBRL definitions for these, a la OpenGamma
  * - see implementations in `Refine` library
  */
final case class Instrument(
    symbol: VarChar,
    issuer: LegalEntity.Key,
    meta: Json
)

object Instrument extends WithAdtKey[Usin, Instrument]

trait PrimaryCapital {

  case class CommonStock(
      mic: Mic,
      c: CurrencyLike,
      tclass: Option[VarChar]
  )

  object CommonStock extends WithAdtKey[Usin, CommonStock]

  case class PreferredStock(
      mic: Mic,
      c: CurrencyLike,
      series: VarChar,
  )

  object PreferredStock extends WithAdtKey[Usin, PreferredStock]

  /** */
  case class Bond(matures: ZonedDateTime) extends Maturity

  /** We presume "bonds" (as opposed to loans) are issued by Corporations, not natural persons. */
  object Bond extends WithAdtKey[Isin, Bond]

  case class TreasurySecurity(matures: ZonedDateTime) extends Maturity
}

trait VanillaDerivatives {

  case class Index()

  case class EtdFuture()

  // Exchange Traded Derivative - Option (ETD)
  case class EtdOption()

  // I mean, right?
  case class EtdFutureOption(underlyer: Instrument.Key) extends Derivative

  case class IndexOption(underlyer: Instrument.Key) extends Derivative

  case class StockOption(underlyer: Instrument.Key) extends Derivative

  case class BondFuture()
  // A BondFutureOption.
  case class BondFutureOption()
  // Exchange Traded Derivative - Future (ETD)
}

trait Exotics {
  // A product only used for calibration.
  case class Calibration()
  // Credit Default Swap (CDS)
  case class Cds()
  // CDS index
  case class CdsIndex()
  // Constant Maturity Swap (CMS)
  case class Cms()
  // A Deliverable Swap Forward
  // https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf
  case class Dsf()
  // Forward Rate Agreement
  case class Fra()
  // // A representation based on sensitivities.
  case class Sensitivities()
  // A Swap.
  case class Swap()
  // A Swaption.
  case class Swaption()
}

trait Fx {
  // FX Non-Deliverable Forward
  case class FxNdf()
  // A FxSingle.
  case class FxSingle()
  // A FxSingleBarrierOption.
  case class FxSingleBarrierOption()
  // A FxSwap.
  case class FxSwap()
  // A FxVanillaOption.
  case class FxVanillaOption()
}

trait Ibor {
  // A IborCapFloor.
  case class IborCapFloor()
  // A IborFuture.
  case class IborFuture()
  // A IborFutureOption.
  case class IborFutureOption()
}

trait Lending {
  // A BulletPayment.
  case class BulletPayment()
  // A TermDeposit.
  case class TermDeposit()

  case class AmortizingLoan()

  case class ConvertibleNote()
}

/** FIXME: "Index" anything should reference the index - need a Table of Indexes */
sealed trait Derivative {
  def underlyer: Instrument.Key
}

sealed trait Index {
  def underlyer: Instrument
}

sealed trait Maturity {
  def matures: ZonedDateTime
}

sealed trait Expiry {
  def expires: ZonedDateTime
}

sealed trait Strike[N] {
  def strike: N
}

object Strike {

  sealed abstract case class SimpleStrike[N] private (strike: N) extends Strike[N]
  object SimpleStrike {
    def apply[N: Financial](strike: N): SimpleStrike[N] = new SimpleStrike(strike) {}
  }

  sealed abstract case class LogMoneynessStrike[N] private (strike: N) extends Strike[N]
  object LogMoneynessStrike {
    def apply[N: Financial](strike: N): LogMoneynessStrike[N] = new LogMoneynessStrike(strike) {}
    def apply[N: Financial](strike: N, forward: N): LogMoneynessStrike[N] =
      ??? // apply(ln(strike / forward)) TODO abstract ln over Financial[N] ?!
  }
}

object Instruments extends PrimaryCapital with VanillaDerivatives with Exotics with Fx with Ibor with Lending
