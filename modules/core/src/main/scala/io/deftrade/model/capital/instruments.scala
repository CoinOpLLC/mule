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
import cats.{ Eq, Order, Show }
import cats.derived.{ auto, semi }

import enumeratum.EnumEntry

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined }
import refined.string.{ Url }
import refined.numeric.{ Positive }
import refined.cats._

import keys.{ IsIsin, IsUsin }

/**
  * Models a tradeable thing.
  *
  * Everything is a `Contract` for (mostly future) ([[contracts.Numéraire.InCoin money]])
  * ''except'' for the following categories of([[contracts.Numéraire.InKind pure stuff]]):
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
    issuedBy: LegalEntity.Key,
    issuedIn: Option[CurrencyLike]
) extends Numéraire.InKind

/**
  * An `Instrument` ''evolves'' over time as the `form.Contract` state is updated.
  * The [[keyval.WithKey]] companion takes
  * advantage of the natural (`ISIN` derived) "business key" to define a `String` refinement.
  */
object Instrument extends WithRefinedKey[String, IsAscii24, Instrument] {

  /**
    */
  implicit def instrumentEq: Eq[Instrument] =
    // import auto._; semi.eq
    Eq.fromUniversalEquals[Instrument]

  // /** */
  // implicit def instrumentShow: Show[Instrument] = { import auto._; semi.show }
}

/**
  * Abstract Data Type (ADT) representing [[contracts.Contract]] parameters and state.
  *
  * Embeds `Contract`s within `Instrument`s according to a uniform paramerter scheme.
  *
  * TODO: `Preamble`? `Exhibit`s? Other kinds of (linked or embedded) metadata?
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

/**
  * Instances of `Form` memorialize the state of a `Contract` at a given point in time.
  * (E.g.: the principle and interest balances in a term loan amortization schedule.)
  * Therefore, a `Form` instance, associated with a given `Instrument`, will evolve over time.
  * The key value store captures that evolution.
  *
  * How many evolutions per Instrument key is it reasonable to expect?
  * Dozens (e.g. monthly) not billions (e.g. every millisecond).
  * As an extreme example, the "trace" of bespoke structured product, quoted every
  * fifteen minutes for ten years, would run to about sixty thosand entries. This might be fine.
  *
  * Other examples:
  * - interest or dividend payment
  * - optionality exercise
  * - price fixings
  */
object Form extends WithKey.Aux[Instrument.Key, Form] { lazy val Key = Instrument.Key }

/**
  * Parameters common to multiple `Form`s.
  */
object columns {

  /**
    * Denotes a single [[Instrument.Key]] which tracks a (non-empty) set of `Instrument.Key`s
    *
    * Enumerating the components of an [[forms.Index]] such as the DJIA is the typical use case.
    */
  sealed trait Tracks { self: Form =>
    def members: Set[Instrument.Key]
  }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.) */
  sealed trait Maturity { self: Form =>
    def matures: ZonedDateTime
  }

  /** `Expiry` only applies to `Derivative`s. */
  sealed trait Expiry { self: Derivative =>
    def expires: ZonedDateTime
  }

  /**
    * `Strike`s only apply to `Derivative`s.
    *
    * TODO: {{{def logMoneynessStrike(strike: Double, forward: Double): Strike }}}
    */
  sealed trait Strike { self: Derivative =>
    def strike: Double
  }

  /**
    * All derivative contracts (such as `Future`s) are assumed to be struck at a certain price,
    * and expire on a certain day.
    */
  sealed trait Derivative extends Expiry with Strike { def underlier: WithKey#Key }
}

/**
  * Groups of related `Form`s.
  */
object layers {

  import columns._

  /**
    */
  trait PrimaryCapital {

    /**
      */
    case class CommonStock(
        tclass: Option[Label]
    ) extends Form {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object CommonStock extends WithRefinedKey[String, IsUsin, CommonStock]

    /**
      */
    case class PreferredStock(
        series: Label,
        preference: Double Refined Positive,
        participating: Boolean,
        dividend: Double Refined IsUnitInterval.`[0,1)` // ]
    ) extends Form {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object PreferredStock extends WithRefinedKey[String, IsUsin, PreferredStock]

    /**
      * Assume semiannual, Treasury-style coupons.
      *
      * FIXME:
      * - state is experimental
      * - under most circumstances the list is never inspected (forced)
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
      */
    case class Bill(
        override val matures: ZonedDateTime
    ) extends Form
        with Maturity {

      import std.zeroCouponBond

      /** A `Bill` is a kind of zero coupon bond. */
      def contract: Contract =
        zeroCouponBond(maturity = matures.toInstant, face = Currency.USD(1000.0))
    }

    /**
      * `Bills` are always issued by entities, never by natural persons.
      */
    object Bill extends WithRefinedKey[String, IsIsin, Bill]
  }

  /**
    * And by "vanilla" we mean an exchange traded derivative (ETD).
    */
  trait VanillaDerivatives {

    /**
      */
    sealed trait PutCall extends EnumEntry

    /**
      */
    object PutCall extends DtEnum[PutCall] {
      case object Put  extends PutCall
      case object Call extends PutCall
      lazy val values = findValues
    }

    /**
      */
    case class Index(
        members: Set[Instrument.Key]
    ) extends Form
        with Tracks {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object Index extends WithRefinedKey[String, IsIsin, Index]

    /** Exchange Traded Derivative - Future (ETD) */
    case class XtFuture(
        expires: ZonedDateTime,
        underlier: Instrument.Key,
        strike: Double
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object XtFuture extends WithRefinedKey[String, IsIsin, XtFuture]

    /** Exchange Traded Derivative - Option (ETD) */
    case class XtOption(
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Instrument.Key,
        override val strike: Double
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** TODO: recheck that `Isin` thing... */
    object XtOption extends WithRefinedKey[String, IsIsin, XtOption]

    /**
      */
    case class XtFutureOption(
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: XtFuture.Key,
        override val strike: Double
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def strikeAmount: Double = ???

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object XtFutureOption extends WithRefinedKey[String, IsIsin, XtFutureOption]

    /**
      */
    case class XtIndexOption(
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Index.Key,
        override val strike: Double
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object XtIndexOption extends WithRefinedKey[String, IsIsin, XtIndexOption]
  }

  /**
    * Private lending instruments.
    *
    * TODO: do these next
    */
  trait Lending {

    /**
      */
    case class BulletPayment(
        matures: ZonedDateTime
    ) extends Form
        with Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    case class CreditLine(
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1Q
    ) extends Form
        with Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    case class AmortizingLoan(
        matures: ZonedDateTime,
        frequency: Frequency // = Frequency.F1M
    ) extends Form
        with Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
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

import layers._

/**
  * Groups of related `Form`s.
  */
object forms
    extends PrimaryCapital  // nececssary
    with VanillaDerivatives // fun
    with Lending // as one does
// with Fx                 // WIP
// with Exotics            // primarily for hedge funds
// with Ibor               // primariy for banks

// /** Necessary annotations for data loaded from external sources. */
// private[deftrade] trait Provenance {
//   def sourcedAt: Instant
//   def sourcedFrom: String Refined Url
// }

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
  *
  * TODO: `Provenance` factorization
  *
  * TODO: `Novation`s can represent `Contract`:
  * - Issuance
  * - Assignment
  * - Termination
  */
final case class Novation(
    ante: Option[Instrument.Key],
    post: Option[Instrument.Key],
    date: LocalDate,
    meta: Meta.Id,
    sourcedAt: Instant,
    sourcedFrom: String Refined Url
) // extends Provenance

/**
  * A `Novation.Id` makes an effective M&A ''receipt''.
  *
  * There can be more than one leg in an M&A transaction:
  * - store a `List[Novation]` (or ideally a `NonEmptySet`)
  * - all `List` elements (legs) will get the same `Id`.
  * - thus the ''receipt'' is common to all the legs that make up the transaction.
  */
object Novation extends WithId.Aux[Novation] {

  implicit def novationOrder: Order[Novation] = { import auto.order._; semi.order }
  implicit def novationShow: Show[Novation]   = { import auto.show._; semi.show }
}
