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

import time._, market._, money._, contracts._, keyval._, refinements._

import cats.implicits._

import enumeratum.EnumEntry

import eu.timepit.refined

import keys.{ IsIsin, IsUsin }

/**
  * Models a tradeable thing.
  *
  * TODO:
  *   - investigate FpML ingestion
  *   - `symbol` implies a unified symbology
  *       - specify how to specify it
  *   - "columns": rename as "forms" ?!
  *   - factories for custom `Instruments` (e.g. `SAFEnote`)
  *
  *   FIXME: model the remainder: everything is a Contract for money except:
  *   - Contracts for equity shares (e.g common stock)
  *   - Contracts for physical delivery of commodities
  *   - Contracts for Real Estate(?!)
  */
final case class Instrument(
    symbol: Label,
    issuer: Party.Key,
    currency: CurrencyLike,
    cols: columns.Columns,
    meta: Meta.Id,
) extends Numéraire.InKind {

  /**
    *  TODO: revisit this
    */
  def isLegalTender: Boolean =
    symbol.value === currency.code.value

  /**  */
  def display: Label = cols.display

  /**  */
  def contract: Contract = cols.contract
}

/**
  * Although `Instrument`s '''do not''' evolve over time, the [[keyval.WithKey]] companion takes
  * advantage of the natural (`ISIN` derived) "business key" as a `String`.
  *
  * TODO: The only lifecyle event allowed for an ISIN should be to '''delete''' it.  How to enforce?
  *
  * TODO: an (immutable) instrument in a [[model.layers.Ledger.Position]] can hide an extensive
  * history: What if in 1990 you had 3 separate investments in DEC, COMPAQ, and HP stock...
  * and then this happens:
  * {{{
  *         HP ->  HPQ
  *                 ^
  *                 ^
  *  DEC -> COMPAQ -+
  * }}}
  * you end up with one investment in HPQ!
  *
  * You will need to be able to walk the graph back in time.
  *
  * TODO: `Instrument` evolution as `Contract` `Novation`. (events connecting `ISIN`s?S)
  */
object Instrument extends WithRefinedKey[String, IsUsin, Instrument]

/** */
object columns {

  /** */
  sealed trait Columns extends Product with Serializable {
    final def display: Label = {
      val name: String = productPrefix
      val Right(label) = refined.refineV[IsLabel](name)
      label
    }

    /** */
    def contract: Contract
  }

  /** */
  sealed trait Tracks { self: Columns =>
    def members: Set[Instrument.Key]
  }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.)*/
  sealed trait Maturity extends Columns { def matures: ZonedDateTime }

  /** */
  sealed trait Expiry extends Columns { def expires: ZonedDateTime }

  /** All derivatives (e.g. Futures) are assumed to expire. */
  sealed trait Derivative extends Expiry with Columns { def underlier: WithKey#Key }

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
    ) extends Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      * `Bonds` (as opposed to loans) are always issued by entities, never by natural persons.
      */
    object Bond extends WithRefinedKey[String, IsIsin, Bond]

    /**
      * `Bills` are always issued by entities, never by natural persons.
      */
    object Bill extends WithRefinedKey[String, IsIsin, Bill]

    /** */
    case class Bill(
        override val matures: ZonedDateTime
    ) extends Maturity {

      import std.zeroCouponBond

      /** */
      def contract: Contract =
        zeroCouponBond(maturity = matures.toInstant, face = Currency.USD(1000.0))
    }
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
    ) extends Tracks
        with Columns {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    object Index extends WithRefinedKey[String, IsIsin, Index]

    /** Exchange Traded Derivative - Future (ETD) */
    case class EtdFuture(
        override val expires: ZonedDateTime,
        override val underlier: Instrument.Key
    ) extends Derivative
        with Columns {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    object EtdFuture extends WithRefinedKey[String, IsIsin, EtdFuture]

    /**Exchange Traded Derivative - Option (ETD)  */
    case class EtdOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Instrument.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]
        with Columns {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** I mean, right? */
    case class EtdFutureOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: EtdFuture.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]
        with Columns {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class EtdIndexOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Index.Key,
        override val strike: N,
    ) extends Derivative
        with Strike[N]
        with Columns {

      /** FIXME: implement */
      def contract: Contract = ???
    }
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
        matures: ZonedDateTime
    ) extends Maturity
        with Columns {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class CreditLine(
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1Q
    ) extends Maturity
        with Columns {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class AmortizingLoan(
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1M
    ) extends Maturity
        with Columns {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class ConvertibleNote()
  }
}
