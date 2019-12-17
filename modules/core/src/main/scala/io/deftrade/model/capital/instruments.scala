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

import time.{ LocalDate, Period, ZonedDateTime }
import time.market.Frequency
import money.{ Currency, CurrencyLike, Financial }
import keyval._
import refinements.{ Label }
import model.reference.{ IsIsin, IsUsin }

// import cats.implicits._
import cats.syntax.eq._
import cats.instances.string._

// import shapeless.syntax.singleton._

import enumeratum.EnumEntry

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
    issuer: LegalEntity.Key,
    currency: CurrencyLike,
    meta: Json,
) {

  /** And by fiat we mean convention... */
  def isLegalTender: Boolean =
    symbol.value === currency.code.value
}

/**
  * `Instrument`s evolve over time.
  *
  * E.g. one might want to allow for, and track, a change in currency.
  * FIXME find some other way, this is nuts - when are new `ISIN`s issued?
  */
object Instrument extends WithRefinedKey[String, IsUsin, Instrument]

/**
  * WIP that follows Composing Contracts from Simon Peyton Jones et al.
  * TODO: finish

data Contract =
 Zero
| One  Currency
| Give Contract
| And  Contract Contract
| Or   Contract Contract
| Cond    (Obs Bool)   Contract Contract
| Scale   (Obs Double) Contract
| When    (Obs Bool)   Contract
| Anytime (Obs Bool)   Contract
| Until   (Obs Bool)   Contract

  */
sealed trait Contract

/** */
object Contract {

  case object Zero                                                                  extends Contract
  sealed abstract case class One private[Contract] (k: CurrencyLike)                extends Contract
  sealed abstract case class Give private[Contract] (c: Contract)                   extends Contract
  sealed abstract case class Scale[N: Financial] private[Contract] (v: Obs[N])      extends Contract
  sealed abstract case class And private[Contract] (c1: Contract, c2: Contract)     extends Contract
  sealed abstract case class Or private[Contract] (c1: Contract, c2: Contract)      extends Contract
  sealed abstract case class When private[Contract] (v: Obs[Boolean], c: Contract)  extends Contract
  sealed abstract case class Until private[Contract] (v: Obs[Boolean], c: Contract) extends Contract
  sealed abstract case class Anytime private[Contract] (
      v: Obs[Boolean],
      c: Contract
  ) extends Contract
  sealed abstract case class Cond private[Contract] (
      v: Obs[Boolean], // if
      c1: Contract, // then
      c2: Contract // else
  ) extends Contract

  /** */
  implicit class Ops(val c: Contract) /* extends AnyVal */ {

    /** */
    final def and(c2: Contract): Contract = ???

    /** */
    final def or(c2: Contract): Contract = ???

    /** truncate the horizon of a contract */
    final def truncate(zdt: ZonedDateTime): Contract = ???

    /** acquire underlying contract at the specficied date */
    final def at(zdt: ZonedDateTime): Contract = ???

    /** */
    final def elseThen(c2: Contract): Contract = ???

    /** */
    final def scale[N: Financial](q: N): Contract = ???
  }

  /**  */
  def zeroCouponInstrument[N: Financial, C: Currency](t: ZonedDateTime, x: N) =
    Contract.one[C] scale x truncate t

  /** */
  def zero: Contract = Zero

  /** */
  def one[C: Currency]: Contract = new One(Currency[C]) {}

  /** */
  def give(c: Contract): Contract = new Give(c) {}

  /** */
  def when(v: Obs[Boolean], c: Contract): Contract = new When(v, c) {}

  /** */
  def anytime(v: Obs[Boolean], c: Contract): Contract = new Anytime(v, c) {}

  /**  */
  trait Obs[A]

  /**  */
  object Obs {
    def konst[A](a: A): Obs[A]                     = ???
    def time(zdt: ZonedDateTime): Obs[Period]      = ???
    def wsjPrimeRate(date: LocalDate): Obs[Period] = ???
  }
}

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

    sealed trait PutCall extends EnumEntry
    object PutCall extends DtEnum[PutCall] {
      case object Put  extends PutCall
      case object Call extends PutCall
      lazy val values = findValues
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
