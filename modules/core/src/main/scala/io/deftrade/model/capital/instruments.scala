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

import cats.{ Eq, Show }
import cats.implicits._
import cats.derived.{ auto, semi }

import enumeratum.EnumEntry

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined }
import refined.string.{ Url }
import refined.numeric.{ Positive }

import keys.{ IsIsin, IsUsin }

/** Necessary annotations for data loaded from external sources. */
trait Provenance {
  def loadedAt: Instant
  def loadedFrom: String Refined Url
}

/**
  * Models a tradeable thing.
  *
  * Everything is a `Contract` for (mostly future) money ([[InCoin]])
  * ''except'' for the following categories of "pure stuff" ([[InKind]]):
  *   - `Contract`s for equity (e.g shares of common stock)
  *   - `Contract`s for physical delivery of commodities (e.g. tanks of propane)
  *   - `Contract`s for real property (e.g. CRE assets)
  *
  * TODO:
  *   - specify the unified symbology behind `symbol`
  *   - factories for custom `Instruments` (e.g. `SAFEnote`)
  *   - `FpML` ingestion
  */
final case class Instrument(
    symbol: Label,
    form: Form.Id,
    loadedAt: Instant,
    loadedFrom: String Refined Url
) extends Numéraire.InKind
    with Provenance

/**
  * An `Instrument` ''evolves'' over time as the `form.Contract` state is updated.
  * The [[keyval.WithKey]] companion takes
  * advantage of the natural (`ISIN` derived) "business key" to define a `String` refinement.
  */
object Instrument extends WithRefinedKey[String, IsAscii24, Instrument] {

  /** */
  implicit def instrumentEq: Eq[Instrument] =
    // import auto._; semi.eq
    Eq.fromUniversalEquals[Instrument]

  /** */
  // implicit def instrumentShow: Show[Instrument] = { import auto._; semi.show }
}

/**
  * Pure satellite table of metadata about `Instrument`.
  */
final case class Issued(
    instrument: Instrument.Key,
    by: LegalEntity.Key,
    in: CurrencyLike
)

/** */
object Issued extends WithId[Issued]

/**
  * Abstract Data Type (ADT) representing [[Contract]] parameters and state.
  *
  * Embeds `Contract`s within `Instrument`s.
  */
sealed abstract class Form extends Product with Serializable {

  /**
    * The `Contract` embedded within this `Form`.
    */
  def contract: Contract

  /**
    * The display name is the final case class name.
    */
  final def display: Label = {
    val name: String = productPrefix
    val Right(label) = refineV[IsLabel](name)
    label
  }
}

/** */
object Form extends WithKey.Aux[Instrument.Key, Form] { lazy val Key = Instrument.Key }

/**
  * Parameters common to multiple `Form`s.
  */
object columns {

  /**
    * Denotes a single [[Instrument.Key]] which tracks a (non-empty) set of `Instrument.Key`s
    *
    * Enumerating the components of an [[Index]] such as the DJIA is the typical use case.
    */
  sealed trait Tracks { self: Form =>
    def members: Set[Instrument.Key]
  }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.)*/
  sealed trait Maturity { self: Form =>
    def matures: ZonedDateTime
  }

  /** `Expiry` only applies to `Derivative`s. */
  sealed trait Expiry { self: Derivative =>
    def expires: ZonedDateTime
  }

  /** `Strike`s only apply to `Derivative`s. */
  sealed trait Strike { self: Derivative =>
    def strike: Double
  }

  /** All derivative contracts (e.g. Futures) are assumed to be struck at a certain price,
    * and expire on a certain day.
    */
  sealed trait Derivative extends Expiry with Strike { def underlier: WithKey#Key }

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

/**
  * Groups of related `Form`s.
  */
object layers {

  import columns._

  /** */
  trait PrimaryCapital {

    /** */
    case class CommonStock(
        tclass: Option[Label]
    ) extends Form {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class PreferredStock(
        series: Label,
        preference: Double Refined Positive,
        participating: Boolean,
        dividend: Double Refined IsUnitInterval.`[0,1)` // ]
    ) extends Form {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    object PreferredStock extends WithRefinedKey[String, IsUsin, PreferredStock]

    /**
      * Assume semiannual, Treasury-style coupons.
      * FIXME: additional params:
      * State:
      * under most circumstances the list is never inspected (forced)
      */
    case class Bond(
        coupon: Double, // per 100 face
        issued: Instant,
        matures: ZonedDateTime,
        paidCoupons: LazyList[Instant], // most recent first
        unpaidCoupons: LazyList[ZonedDateTime] // soonest due first
    ) extends Form
        with Maturity {

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
    ) extends Form
        with Maturity {

      import std.zeroCouponBond

      /** A `Bill` is a kind of zero coupon bond. */
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
    ) extends Form
        with Tracks {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    object Index extends WithRefinedKey[String, IsIsin, Index]

    /** Exchange Traded Derivative - Future (ETD) */
    case class XtFuture(
        override val expires: ZonedDateTime,
        override val underlier: Instrument.Key,
        override val strike: Double,
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    object XtFuture extends WithRefinedKey[String, IsIsin, XtFuture]

    /**Exchange Traded Derivative - Option (ETD)  */
    case class XtOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Instrument.Key,
        override val strike: Double,
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** I mean, right? */
    case class XtFutureOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: XtFuture.Key,
        override val strike: Double,
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def strikeAmount: N = ???

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class XtIndexOption[N: Financial](
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Index.Key,
        override val strike: Double,
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }
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
    ) extends Form
        with Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class CreditLine(
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1Q
    ) extends Form
        with Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class AmortizingLoan(
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1M
    ) extends Form
        with Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** */
    case class ConvertibleNote(
        matures: ZonedDateTime,
        discount: Double Refined IsUnitInterval.`[0,1)`,
        cap: Option[Double Refined Positive]
    ) extends Form
        with Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }
  }
}

/**
  * Links which model `Instrument` lifecycle transformation acts
  * (such as M&A actions) as events connecting `Instrument.Key`s.
  *
  * Motivation:
  *
  * An (immutable) instrument in a [[model.layers.Ledger.Position]] can hide an extensive
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
  * - You will need to be able to walk the graph back in time.
  * - `Novation`s are the link relations that connect [[Instrument]]s in that graph.
  */
final case class Novation(
    was: Instrument.Key,
    becomes: Instrument.Key,
    loadedAt: Instant,
    loadedFrom: String Refined Url
) extends Provenance

/**
  * A `Novation.Id` makes an effective M&A ''receipt''.
  *
  * There can be more than one leg in an M&A transaction:
  * - store a `List[Novation]`
  * - all `List` elements (legs) will get the same `Id`.
  * - thus the ''receipt'' is common to all the legs that make up the transaction.
  */
object Novation extends WithId[Novation]

/**
  * Memorializes the progression of a single `Instrument`'s `Form`
  * (and thus `Contract`) through its lifecycle.
  */
final case class Evolution(
    forKey: Instrument.Key,
    was: Form.Id,
    becomes: Form.Id,
    at: Instant
)

/** `Evolution`s are pure value objects. */
object Evolution extends WithId[Evolution]
