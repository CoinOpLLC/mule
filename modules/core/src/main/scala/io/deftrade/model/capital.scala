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
package model
package capital

import time.ZonedDateTime
import money.{ CurrencyLike, Financial }
import keyval._
import refinements.{ Label }
import model.reference.{ IsIsin, IsUsin, Mic }

import cats.Id
import cats.instances.string._

// import shapeless.syntax.singleton._

import io.circe.Json

import scala.language.higherKinds

/**
  * Models a tradeable thing.
  *
  * TODO:
  *   - use the XBRL definitions for these, a la OpenGamma
  *   - see implementations in `Refine` library
  */
final case class Instrument(
    symbol: Label,
    issuer: LegalEntity.Key,
    meta: Json
)

/** */
object Instrument extends WithRefinedKey[String, IsUsin, Instrument]

/** */
trait PrimaryCapital {

  /** */
  case class CommonStock(
      mic: Mic,
      c: CurrencyLike,
      tclass: Option[Label]
  )

  /** */
  object CommonStock extends WithRefinedKey[String, IsUsin, CommonStock]

  /** */
  case class PreferredStock(
      mic: Mic,
      c: CurrencyLike,
      series: Label,
  )

  /** */
  object PreferredStock extends WithRefinedKey[String, IsUsin, PreferredStock]

  /** */
  case class Bond(matures: ZonedDateTime) extends Maturity

  /** We presume "bonds" (as opposed to loans) are issued by Corporations, not natural persons. */
  object Bond extends WithRefinedKey[String, IsIsin, Bond]

  /** */
  case class TreasurySecurity(matures: ZonedDateTime) extends Maturity
}

/** */
trait VanillaDerivatives {

  /** */
  case class Index(underlyer: List[Instrument.Key]) extends Derivative[List] {
    def components: List[Instrument.Key] = underlyer
  }

  /** */
  object Index extends WithRefinedKey[String, IsIsin, Index]

  /** Exchange Traded Derivative - Future (ETD) */
  case class EtdFuture(underlyer: Instrument.Key) extends Derivative[Id]

  /**Exchange Traded Derivative - Option (ETD)  */
  case class EtdOption(underlyer: Instrument.Key) extends Derivative[Id]

  /** I mean, right? */
  case class EtdFutureOption(underlyer: Instrument.Key) extends Derivative[Id]

  /** */
  case class IndexOption(underlyer: Instrument.Key) extends Derivative[Id]

  /** */
  case class StockOption(underlyer: Instrument.Key) extends Derivative[Id]

  /** */
  case class BondFuture(underlyer: Instrument.Key) extends Derivative[Id]

  /** */
  case class BondFutureOption(index: Index.Key) extends Derivative[Id] {

    /** FIXME need to be able to derive this */
    def underlyer: Instrument.Key = ??? // index
  }
}

/** WIP */
trait Exotics {

  /** A product only used for calibration. FIXME WTF */
  case class Calibration()

  /** Credit Default Swap */
  case class Cds()

  /** */
  case class CdsIndex()

  /** Constant Maturity Swap */
  case class Cms()

  /**
    * [[https://www.cmegroup.com/trading/interest-rates/files/understanding-dsf.pdf Deliverable Swap Forward]]
    */
  case class Dsf()

  /** Forward Rate Agreement */
  case class Fra()

  /** A representation based on sensitivities. FIXME WTF */
  case class Sensitivities()

  /** */
  case class Swap()

  /** */
  case class Swaption()
}

/** WIP */
trait Fx {

  /** FX Non-Deliverable Forward */
  case class FxNdf()

  /** */
  case class FxSingle()

  /** */
  case class FxSingleBarrierOption()

  /** */
  case class FxSwap()

  /** */
  case class FxVanillaOption()
}

/** */
trait Ibor {

  /** */
  case class IborCapFloor()

  /** */
  case class IborFuture()

  /** */
  case class IborFutureOption()
}

/**
  * Bespoke lending products.
  */
trait Lending {

  /** */
  case class BulletPayment()

  /** */
  case class TermDeposit()

  /** */
  case class AmortizingLoan()

  /** */
  case class ConvertibleNote()
}

/** FIXME: "Index" anything should reference the index - need a Table of Indexes */
sealed trait Derivative[F[_]] {

  /** */
  def underlyer: F[Instrument.Key]
}

/** */
sealed trait Index {

  /** */
  def underlyer: Instrument
}

/** */
sealed trait Maturity {

  /** */
  def matures: ZonedDateTime
}

/** */
sealed trait Expiry {

  /** */
  def expires: ZonedDateTime
}

/** */
sealed trait Strike[N] {

  /** */
  def strike: N
}

/** */
object Strike {

  /** */
  sealed abstract case class SimpleStrike[N] private (strike: N) extends Strike[N]

  /** */
  object SimpleStrike {
    def apply[N: Financial](strike: N): SimpleStrike[N] = new SimpleStrike(strike) {}
  }

  /** */
  sealed abstract case class LogMoneynessStrike[N] private (strike: N) extends Strike[N]

  /** */
  object LogMoneynessStrike {

    /** */
    def apply[N: Financial](strike: N): LogMoneynessStrike[N] = new LogMoneynessStrike(strike) {}

    /** */
    def apply[N: Financial](strike: N, forward: N): LogMoneynessStrike[N] =
      ??? // apply(ln(strike / forward)) TODO abstract ln over Financial[N] ?!
  }
}

/** */
object Instruments extends PrimaryCapital with VanillaDerivatives with Exotics with Fx with Ibor with Lending
