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

import cats.instances.string._

// import shapeless.syntax.singleton._

import io.circe.Json

/**
  * Models a tradeable thing.
  *
  * TODO:
  *   - use the XBRL definitions for these, a la OpenGamma
  *   - see implementations in `Refine` library
  *   - `symbol` implies a unified symbology. (These don't just happen.)
  */
final case class Instrument(
    symbol: Label,
    currency: CurrencyLike,
    issuer: LegalEntity.Key,
    meta: Json,
)

/**
  * `Instrument`s evolve over time.
  *
  * E.g. t is not inconcievable that one might want to allow for,
  * and track, a change in currency.
  */
object Instrument extends WithRefinedKey[String, IsUsin, Instrument]

/** */
trait Aspects {

  /** */
  sealed trait Contract { def instrument: Instrument.Key }

  /** */
  sealed trait Tracks extends Contract { def members: Set[Instrument.Key] }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.)*/
  sealed trait Maturity extends Contract { def matures: ZonedDateTime }

  /** */
  sealed trait Expiry extends Contract { def expires: ZonedDateTime }

  /** All derivatives (e.g. Futures) are assumed to expire. */
  sealed trait Derivative extends Expiry { def underlyer: WithKey#Key }

  /** Everyting with a strike is assumed to expire. */
  sealed trait Strike[N] { self: Derivative =>
    def strike: N
  }

  /** */
  object Strike {
    // def apply[N: Financial](strike: N): Strike[N]
    /** */
    object LogMoneynessStrike {
      // def apply[N: Financial](strike: N, forward: N): LogMoneynessStrike[N] =
      //   apply(ln(strike / forward)) TODO abstract ln over Financial[N] ?!
    }
  }
}

/** */
trait PrimaryCapital extends Aspects {

  /** */
  case class CommonStock(
      override val instrument: Instrument.Key,
      mic: Mic,
      tclass: Option[Label]
  ) extends Contract

  /** */
  object CommonStock extends WithRefinedKey[String, IsUsin, CommonStock]

  /** */
  case class PreferredStock(
      override val instrument: Instrument.Key,
      mic: Mic,
      series: Label,
  ) extends Contract

  /** */
  object PreferredStock extends WithRefinedKey[String, IsUsin, PreferredStock]

  /** */
  case class Bond(
      override val instrument: Instrument.Key,
      override val matures: ZonedDateTime
  ) extends Maturity

  /** We presume "bonds" (as opposed to loans) are issued by Corporations, not natural persons. */
  object Bond extends WithRefinedKey[String, IsIsin, Bond]

  /** */
  case class TreasurySecurity(
      override val instrument: Instrument.Key,
      override val matures: ZonedDateTime
  ) extends Maturity
}

/** */
trait VanillaDerivatives extends Aspects {

  sealed trait PutCall
  case object Put  extends PutCall
  case object Call extends PutCall

  /** */
  case class Index(
      override val instrument: Instrument.Key,
      override val members: Set[Instrument.Key]
  ) extends Tracks

  /** */
  object Index extends WithRefinedKey[String, IsIsin, Index]

  /** Exchange Traded Derivative - Future (ETD) */
  case class EtdFuture(
      override val instrument: Instrument.Key,
      override val expires: ZonedDateTime,
      override val underlyer: Instrument.Key
  ) extends Derivative

  object EtdFuture extends WithRefinedKey[String, IsIsin, EtdFuture]

  /**Exchange Traded Derivative - Option (ETD)  */
  case class EtdOption[N: Financial](
      val putCall: PutCall,
      override val instrument: Instrument.Key,
      override val expires: ZonedDateTime,
      override val underlyer: Instrument.Key,
      override val strike: N,
  ) extends Derivative
      with Strike[N]

  /** I mean, right? */
  case class EtdFutureOption[N: Financial](
      val putCall: PutCall,
      override val instrument: Instrument.Key,
      override val expires: ZonedDateTime,
      override val underlyer: EtdFuture.Key,
      override val strike: N,
  ) extends Derivative
      with Strike[N]

  /** */
  case class EtdIndexOption[N: Financial](
      val putCall: PutCall,
      override val instrument: Instrument.Key,
      override val expires: ZonedDateTime,
      override val underlyer: Index.Key,
      override val strike: N,
  ) extends Derivative
      with Strike[N]
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

/** WIP */
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
  *
  * TODO: do these next
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

/** */
object Instruments extends PrimaryCapital with VanillaDerivatives with Exotics with Fx with Ibor with Lending
