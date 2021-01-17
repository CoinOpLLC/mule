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

import keys.{ ISIN, USIN }

/** Models a tradeable thing.
  *
  * Policy: Only legal entities (and not natural persons) may issue `Instruments`.
  */
final case class Instrument(
    symbol: Label,
    issuedBy: LegalEntities.Key,
    issuedIn: Option[CurrencyLike]
) extends Numéraire.InKind

/**
  */
object Instrument {

  /**
    */
  implicit def instrumentEq: Eq[Instrument] = { import auto._; semi.eq }
  // Eq.fromUniversalEquals[Instrument]

  /**
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  implicit def instrumentShow: Show[Instrument] = { import auto._; semi.show }
}

/** Indexed by CUSIPs and other formats.
  * An `Instrument` ''evolves'' over time as the `form.Contract` state is updated.
  */
object Instruments extends KeyValueStores.KV[USIN, Instrument]

/**
  */
object ExchangeTradedInstruments extends KeyValueStores.KV[ISIN, Instrument]

/** Represents [[contracts.Contract]] parameters and state.
  *
  * Embeds `Contract`s within `Instrument`s according to a uniform paramerter scheme.
  *
  * TODO: `Preamble`? `Exhibit`s? Other kinds of (linked or embedded) metadata?
  */
sealed abstract class Form extends Product with Serializable {

  /** The `Contract` embedded within this `Form`.
    */
  def contract: Contract

  /** The display name is the final case class name.
    */
  final def display: Label = {
    val name: String = productPrefix
    val Right(label) = refineV[IsLabel](name)
    label
  }
}

/** Instances of `Form` memorialize the state of a `Contract` at a given point in time.
  */
object Form

// object Forms extends KeyValueStores.KV[Instruments.Key, Form] {
//   // lazy val Key: KeyCompanion[Instrument] = ??? // Instruments.Key
// }

/** Parameters common to multiple `Form`s.
  */
object columns {

  /** Denotes a single [[Instruments.Key]] which tracks a (non-empty) set of `Instruments.Key`s
    *
    * Enumerating the components of an [[forms.Index]] such as the DJIA is the typical use case.
    */
  sealed trait Tracks { self: Form =>
    def members: Set[Instruments.Key]
  }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.) */
  sealed trait Maturity { self: Form =>
    def matures: ZonedDateTime
  }

  /** `Expiry` only applies to `Derivative`s. */
  sealed trait Expiry { self: Derivative =>
    def expires: ZonedDateTime
  }

  /** `Strike`s only apply to `Derivative`s.
    *
    * TODO: {{{def logMoneynessStrike(strike: Double, forward: Double): Strike }}}
    */
  sealed trait Strike { self: Derivative =>
    def strike: Double
  }

  /** All derivative contracts (such as `Future`s) are assumed to be struck at a certain price,
    * and expire on a certain day.
    */
  sealed trait Derivative extends Expiry with Strike { def underlier: Instruments.Key }
}

/** Groups of related `Form`s.
  */
object layers {

  import columns._

  /**
    */
  trait PrimaryCapital {

    /**
      */
    final case class CommonStock(
        tclass: Option[Label]
    ) extends Form {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object CommonStocks extends KeyValueStores.KV[Instruments.Key, CommonStock]

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
    object PreferredStocks extends KeyValueStores.KV[Instruments.Key, PreferredStock]

    /** Assume semiannual, Treasury-style coupons.
      */
    final case class Bond(
        coupon: Double, // per 100 face
        issued: Instant,
        matures: ZonedDateTime,
        unpaidCoupons: List[ZonedDateTime], // soonest due first
        paidCoupons: List[Instant] // most recent first
    ) extends Form
        with Maturity {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** `Bonds` (as opposed to loans) are always issued by entities, never by natural persons.
      */
    object Bonds extends KeyValueStores.KV[ExchangeTradedInstruments.Key, Bond]

    /**
      */
    final case class Bill(
        override val matures: ZonedDateTime
    ) extends Form
        with Maturity {

      import std.zeroCouponBond

      /** A `Bill` is a kind of zero coupon bond. */
      def contract: Contract =
        zeroCouponBond(maturity = matures.toInstant, face = Currency.USD(1000.0))
    }

    /** `Bills` are always issued by entities, never by natural persons.
      */
    object Bills extends KeyValueStores.KV[ExchangeTradedInstruments.Key, Bill]
  }

  /** And by "vanilla" we mean an exchange traded derivative (ETD).
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
    final case class Index(
        members: Set[Instruments.Key]
    ) extends Form
        with Tracks {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object Indexes extends KeyValueStores.KV[ExchangeTradedInstruments.Key, Index]

    /** Exchange Traded Derivative - Future (ETD) */
    final case class XtFuture(
        expires: ZonedDateTime,
        underlier: Instruments.Key,
        strike: Double
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object XtFutures extends KeyValueStores.KV[ExchangeTradedInstruments.Key, XtFuture]

    /** Exchange Traded Derivative - Option (ETD) */
    final case class XtOption(
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Instruments.Key,
        override val strike: Double
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /** TODO: recheck that `Isin` thing... */
    object XtOptions extends KeyValueStores.KV[ExchangeTradedInstruments.Key, XtOption]

    protected object FIXME {

      import refined.auto._

      def x: ISIN = ???
      def y: USIN = x
    }

    /**
      */
    final case class XtFutureOption(
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        // override val underlier: XtFutures.Key,
        // override val underlier: ExchangeTradedInstruments.Key,
        override val underlier: Instruments.Key,
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
    object XtFutureOptions extends KeyValueStores.KV[XtFutures.Key, XtFutureOption]

    /**
      */
    case class XtIndexOption(
        val putCall: PutCall,
        override val expires: ZonedDateTime,
        override val underlier: Instruments.Key,
        override val strike: Double
    ) extends Form
        with Derivative {

      /** FIXME: implement */
      def contract: Contract = ???
    }

    /**
      */
    object XtIndexOptions extends KeyValueStores.KV[Indexes.Key, XtIndexOption]
  }

  /** Private lending instruments.
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

/** Groups of related `Form`s.
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

/** Links which model `Instrument` lifecycle transformation acts
  * (such as M&A actions) as events connecting `Instruments.Key`s.
  */
final case class Novation(
    ante: Option[Instruments.Key],
    post: Option[Instruments.Key],
    date: LocalDate,
    // meta: Metas.Id,
    sourcedAt: Instant,
    sourcedFrom: String Refined Url
) // extends Provenance

/** A `Novation.Id` makes an effective M&A ''receipt''.
  */
object Novation {

  implicit def novationOrder: Order[Novation] = { import auto.order._; semi.order }
  implicit def novationShow: Show[Novation]   = { import auto.show._; semi.show }
}

/**
  */
object Novations extends ValueStores.V[Novation]
