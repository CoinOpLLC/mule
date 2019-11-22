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

import time.{ ZonedDateTime }
import time.market.Frequency
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
    market: Mic,
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
object columns {

  /** */
  sealed trait Tracker { def members: Set[Instrument.Key] }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.)*/
  sealed trait Maturity { def matures: ZonedDateTime }

  /** */
  sealed trait Expiry { def expires: ZonedDateTime }

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
object layers {

  import columns._

  /** */
  trait PrimaryCapital {

    /** */
    case class CommonStock(
        tclass: Option[Label]
    )

    /** */
    object CommonStock extends WithRefinedKey[String, IsUsin, CommonStock]

    /** */
    case class PreferredStock(
        series: Label,
    )

    /** */
    object PreferredStock extends WithRefinedKey[String, IsUsin, PreferredStock]

    /** */
    case class Bond(
        override val matures: ZonedDateTime
    ) extends Maturity

    /**
      * `Bonds` (as opposed to loans) are always issued by corporations, never by natural persons.
      */
    object Bond extends WithRefinedKey[String, IsIsin, Bond]

    /** */
    case class TreasurySecurity(
        override val matures: ZonedDateTime
    ) extends Maturity
  }

  /**
    * And by "vanilla" we mean an exchange traded derivative (ETD).
    */
  trait VanillaDerivatives {

    sealed trait PutCall
    object PutCall {
      case object Put  extends PutCall
      case object Call extends PutCall
    }

    /** */
    case class Index(
        override val members: Set[Instrument.Key]
    ) extends Tracker

    /** */
    object Index extends WithRefinedKey[String, IsIsin, Index]

    /** Exchange Traded Derivative - Future (ETD) */
    case class EtdFuture(
        override val expires: ZonedDateTime,
        override val underlyer: Instrument.Key
    ) extends Derivative

    object EtdFuture extends WithRefinedKey[String, IsIsin, EtdFuture]

    /**Exchange Traded Derivative - Option (ETD)  */
    case class EtdOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlyer: Instrument.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]

    /** I mean, right? */
    case class EtdFutureOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlyer: EtdFuture.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]

    /** */
    case class EtdIndexOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlyer: Index.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]
  }

  /**
    * WIP - for otc derivative market participants (like hedge funds).
    */
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

  /**
    * WIP - for fx derivative market participants (like banks).
    */
  trait Fx extends {

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

  /**
    * WIP - for otc derivative market participants (like banks).
    */
  trait Ibor extends {

    /** */
    case class IborCapFloor()

    /** */
    case class IborFuture()

    /** */
    case class IborFutureOption()
  }

  /**
    * Private lending instruments.
    *
    * TODO: do these next
    */
  trait Lending {

    /** */
    case class BulletPayment(
        instrument: Instrument.Key,
        matures: ZonedDateTime
    ) extends Maturity

    /** */
    case class CreditLine(
        instrument: Instrument.Key,
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1Q
    ) extends Maturity

    /** */
    case class AmortizingLoan(
        instrument: Instrument.Key,
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1M
    ) extends Maturity

    /** */
    case class ConvertibleNote()
  }
}
