package io.deftrade
package model
package capital

import money.{ CurrencyLike, Financial }
import time.ZonedDateTime
import model.refinements.{ Isin, Mic, Psin, Usin, VarChar }
import keyval._

import cats.{ Eq, Hash, Order, Show }
import cats.instances.string._

import shapeless.syntax.singleton._

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

  final case class CommonStock(
      mic: Mic,
      c: CurrencyLike,
      tclass: Option[VarChar]
  )

  object CommonStock extends WithAdtKey[Usin, CommonStock]

  final case class PreferredStock(
      mic: Mic,
      c: CurrencyLike,
      series: VarChar,
  )

  object PreferredStock extends WithAdtKey[Usin, PreferredStock]

  /** */
  final case class Bond(matures: ZonedDateTime) extends Maturity

  /** We presume "bonds" (as opposed to loans) are issued by Corporations, not natural persons. */
  object Bond extends WithAdtKey[Isin, Bond]

  final case class TreasurySecurity(matures: ZonedDateTime) extends Maturity
}

trait VanillaDerivatives {

  final case class Index()

  final case class EtdFuture()

  // Exchange Traded Derivative - Option (ETD)
  final case class EtdOption()

  // I mean, right?
  final case class EtdFutureOption(underlyer: Instrument.Key) extends Derivative

  final case class IndexOption(underlyer: Instrument.Key) extends Derivative

  final case class StockOption(underlyer: Instrument.Key) extends Derivative

  final case class BondFuture()
  // A BondFutureOption.
  final case class BondFutureOption()
  // Exchange Traded Derivative - Future (ETD)
}

trait Exotics {
  // A product only used for calibration.
  final case class Calibration()
  // Credit Default Swap (CDS)
  final case class Cds()
  // CDS index
  final case class CdsIndex()
  // Constant Maturity Swap (CMS)
  final case class Cms()
  // A Deliverable Swap Forward
  // https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf
  final case class Dsf()
  // Forward Rate Agreement
  final case class Fra()
  // // A representation based on sensitivities.
  final case class Sensitivities()
  // A Swap.
  final case class Swap()
  // A Swaption.
  final case class Swaption()
}

trait Fx {
  // FX Non-Deliverable Forward
  final case class FxNdf()
  // A FxSingle.
  final case class FxSingle()
  // A FxSingleBarrierOption.
  final case class FxSingleBarrierOption()
  // A FxSwap.
  final case class FxSwap()
  // A FxVanillaOption.
  final case class FxVanillaOption()
}

trait Ibor {
  // A IborCapFloor.
  final case class IborCapFloor()
  // A IborFuture.
  final case class IborFuture()
  // A IborFutureOption.
  final case class IborFutureOption()
}

trait Lending {
  // A BulletPayment.
  final case class BulletPayment()
  // A TermDeposit.
  final case class TermDeposit()

  final case class AmortizingLoan()

  final case class ConvertibleNote()
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
