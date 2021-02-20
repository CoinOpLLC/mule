package io.deftrade
package model.layers

import time._, money._, contracts._, keyval._, refinements._
import model.pillars.keys.{ ISIN, USIN }
import model.pillars.{ std, Metas }

import cats.implicits._
import cats.{ Eq, Order, Show }
import cats.derived.{ auto, semiauto }

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined }
import refined.string.{ Url }
import refined.numeric.Positive
import refined.cats._

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

trait Paper { module: ModuleTypes with Person =>

  final case class Papers(
      instruments: Instruments.KeyValueStore[IO],
      forms: Forms.ValueStore[IO],
      instrumentsForms: InstrumentsForms.KeyValueStore[IO],
      novations: Novations.ValueStore[IO]
  )

  val papers: Papers

  /** Models a tradeable thing.
    *
    * Policy: Only legal entities (and not natural persons) may issue `Instruments`.
    */
  sealed abstract case class Instrument private (
      final val symbol: Label,
      final val issuedBy: LegalEntities.Key,
      final val issuedIn: Option[CurrencyLike]
  ) extends Numéraire.InKind

  /**
    */
  object Instrument {

    def apply(
        symbol: Label,
        issuedBy: LegalEntities.Key,
        issuedIn: Option[CurrencyLike]
    ): Instrument =
      new Instrument(symbol, issuedBy, issuedIn) {}

    /**
      */
    implicit def instrumentEq: Eq[Instrument] = { import auto._; semiauto.eq }

    /**
      */
    implicit def instrumentShow: Show[Instrument] = { import auto._; semiauto.show }
  }

  /** Indexed by CUSIPs and other formats.
    * An `Instrument` ''evolves'' over time as the `form.Contract` state is updated.
    */
  case object Instruments extends KeyValueStores.KV[USIN, Instrument]

  /**
    */
  case object ExchangeTradedInstruments extends KeyValueStores.KV[ISIN, Instrument]

  /** Represents [[contracts.Contract]] parameters and state.
    *
    * Instances of `Form` memorialize the state of a `Contract` at a given point in time.
    *
    * Embeds `Contract`s within `Instrument`s according to a uniform paramerter scheme.
    *
    * TODO: `Preamble`? `Exhibit`s? Other kinds of (linked or embedded) metadata?
    */
  final type Form = Phorm

  /**
    */
  final lazy val Form = Phorm

  /**
    */
  case object Forms extends ValueStores.SADT[Form] {

    sealed abstract case class Link private (form: Id)

    object Link {

      def apply(link: Id): Link =
        new Link(link) {}

      implicit lazy val linkEq: Eq[Link]     = { import auto.eq._; semiauto.eq }
      implicit lazy val linkShow: Show[Link] = { import auto.show._; semiauto.show }
    }
  }

  /** Parameters common to multiple `Form`s.
    */
  sealed trait columns {

    /** Tracks a (non-empty) set of `Instruments.Key`s
      *
      * Enumerating the components of an [[forms.Index]] such as the DJIA is the typical use case.
      */
    sealed trait Tracker { self: Form =>
      def members: Set[Instruments.Key]
    }

    /** Bonds (primary capital) `mature` (as opposed to `expire`.) */
    sealed trait Maturity { self: Form =>
      def matures: ZonedDateTime
    }

    /** All derivative contracts (such as `Future`s) are assumed to be struck at a certain price,
      * and expire on a certain day.
      */
    sealed trait Derivative extends Expiry with Strike {
      def underlier: Instruments.Key
    }

    /** `Expiry` only applies to `Derivative`s.
      */
    sealed trait Expiry { self: Derivative =>
      def expires: ZonedDateTime
    }

    /** `Strike`s only apply to `Derivative`s.
      *
      * TODO: {{{def logMoneynessStrike(strike: Quamtity, forward: Quamtity): Strike }}}
      */
    sealed trait Strike { self: Derivative =>
      def strike: Quantity
    }
  }

  /** Groups of related `Form`s.
    */
  sealed trait PrimaryCapital extends columns {

    /**
      */
    sealed abstract case class CommonStock private (final val tclass: Option[Label]) extends Form

    object CommonStock {

      def apply(tclass: Option[Label]): CommonStock =
        new CommonStock(tclass) {
          final def contract: Contract =
            contracts.zero
        }

      implicit lazy val cStkEq: Eq[CommonStock]     = { import auto.eq._; semiauto.eq }
      implicit lazy val cStkShow: Show[CommonStock] = { import auto.show._; semiauto.show }

      implicit lazy val cStkEncoder: Encoder[CommonStock] = {
        import io.circe.refined._; deriveEncoder
      }
      implicit lazy val cStkDecoder: Decoder[CommonStock] = {
        import io.circe.refined._; deriveDecoder
      }
    }

    /**
      */
    case object CommonStocks extends KeyValueStores.KV[Instruments.Key, CommonStock]

    // /**
    //   */
    // final case class PreferredStock(
    //     series: Label,
    //     preference: Quantity Refined Positive,
    //     participating: Boolean,
    //     dividend: Quantity Refined IsUnitInterval.`[0,1)` // ]
    // ) extends Form {
    //
    //   /** FIXME: implement */
    //   def contract: Contract = ???
    // }
    //
    // /**
    //   */
    // case object PreferredStocks extends KeyValueStores.KV[Instruments.Key, PreferredStock]
    //
    // /** Assume semiannual, Treasury-style coupons.
    //   */
    // final case class Bond(
    //     coupon: Quantity, // per 100 face
    //     issued: Instant,
    //     matures: ZonedDateTime,
    //     unpaidCoupons: List[ZonedDateTime], // soonest due first
    //     paidCoupons: List[Instant] // most recent first
    // ) extends Form
    //     with Maturity {
    //
    //   /** FIXME: implement */
    //   def contract: Contract = ???
    // }
    //
    // /** `Bonds` (as opposed to loans) are always issued by entities, never by natural persons.
    //   */
    // case object Bonds extends KeyValueStores.KV[ExchangeTradedInstruments.Key, Bond]
    //
    // /**
    //   */
    // final case class Bill(
    //     override val matures: ZonedDateTime
    // ) extends Form
    //     with Maturity {
    //
    //   import std.zeroCouponBond
    //
    //   /** A `Bill` is a kind of zero coupon bond. */
    //   def contract: Contract =
    //     zeroCouponBond(maturity = matures.toInstant, face = Currency.USD(1000.0))
    // }
    //
    // /** `Bills` are always issued by entities, never by natural persons.
    //   */
    // case object Bills extends KeyValueStores.KV[ExchangeTradedInstruments.Key, Bill]
  }

  object layers extends PrimaryCapital

  /**
    */
  case object InstrumentsForms extends KeyValueStores.KV[Instruments.Key, Forms.Link]

  /** Links which model `Instrument` lifecycle transformation acts
    * (such as M&A actions) as events connecting `Instruments.Key`s.
    */
  sealed abstract case class Novation private (
      asOf: LocalDate,
      ante: Option[Instruments.Key],
      post: Option[Instruments.Key],
      meta: Metas.Id,
      sourcedAt: Instant,
      sourcedFrom: String Refined Url
  ) // extends Provenance

  /** A `Novation.Id` makes an effective M&A ''receipt''.
    */
  object Novation {

    def apply(
        asOf: LocalDate,
        ante: Option[Instruments.Key],
        post: Option[Instruments.Key],
        meta: Metas.Id,
        sourcedAt: Instant,
        sourcedFrom: String Refined Url
    ): Novation =
      new Novation(asOf, ante, post, meta, sourcedAt, sourcedFrom) {}

    implicit def novationOrder: Order[Novation] = { import auto.order._; semiauto.order }
    implicit def novationShow: Show[Novation]   = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object Novations extends ValueStores.VS[Novation]
}

import refined.auto._
sealed trait Phorm extends Product {

  def contract: Contract
  //
  final def display: Label = {
    val name: String = s"""$productPrefix::${contract.show}"""
    val Right(label) = refineV[IsLabel](name)
    label
  }
}

object Phorm {

  implicit lazy val phormEq: Eq[Phorm]     = ??? // { import auto.eq._; semiauto.eq }
  implicit lazy val phormShow: Show[Phorm] = ??? // { import auto.show._; semiauto.show }

  implicit lazy val phormEncoder
    : Encoder[Phorm] = ??? // { import io.circe.refined._; deriveEncoder }
  implicit lazy val phormDecoder
    : Decoder[Phorm] = ??? // { import io.circe.refined._; deriveDecoder }
}

/**
  */
case object Phorms extends ValueStores.SADT[Phorm] {

  sealed abstract case class Link private (form: Id)

  object Link {

    def apply(link: Id): Link = new Link(link) {}

    implicit lazy val linkEq: Eq[Link]     = { import auto.eq._; semiauto.eq }
    implicit lazy val linkShow: Show[Link] = { import auto.show._; semiauto.show }
  }
}

// /** And by "vanilla" we mean an exchange traded derivative (ETD).
//   */
// object VanillaDerivatives {
//
//   /**
//     */
//   sealed trait PutCall extends EnumEntry
//
//   /**
//     */
//   object PutCall extends DtEnum[PutCall] {
//     case object Put  extends PutCall
//     case object Call extends PutCall
//     lazy val values = findValues
//   }
//
//   /**
//     */
//   final case class Index(
//       members: Set[Instruments.Key]
//   ) extends Form
//       with columns.Tracker {
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
//
//   /**
//     */
//   case object Indexes extends KeyValueStores.KV[ExchangeTradedInstruments.Key, Index]
//
//   /** Exchange Traded Derivative - Future (ETD) */
//   final case class XtFuture(
//       expires: ZonedDateTime,
//       underlier: Instruments.Key,
//       strike: Double
//   ) extends Form
//       with columns.Derivative {
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
//
//   /**
//     */
//   case object XtFutures extends KeyValueStores.KV[ExchangeTradedInstruments.Key, XtFuture]
//
//   /** Exchange Traded Derivative - Option (ETD) */
//   final case class XtOption(
//       val putCall: PutCall,
//       override val expires: ZonedDateTime,
//       override val underlier: Instruments.Key,
//       override val strike: Double
//   ) extends Form
//       with columns.Derivative {
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
//
//   /** TODO: recheck that `Isin` thing... */
//   case object XtOptions extends KeyValueStores.KV[ExchangeTradedInstruments.Key, XtOption]
//
//   /**
//     */
//   final case class XtFutureOption(
//       val putCall: PutCall,
//       override val expires: ZonedDateTime,
//       // override val underlier: XtFutures.Key,
//       // override val underlier: ExchangeTradedInstruments.Key,
//       override val underlier: Instruments.Key,
//       override val strike: Double
//   ) extends Form
//       with columns.Derivative {
//
//     /** FIXME: implement */
//     def strikeAmount: Double = ???
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
//
//   /**
//     */
//   case object XtFutureOptions extends KeyValueStores.KV[XtFutures.Key, XtFutureOption]
//
//   /**
//     */
//   final case class XtIndexOption(
//       val putCall: PutCall,
//       override val expires: ZonedDateTime,
//       override val underlier: Instruments.Key,
//       override val strike: Double
//   ) extends Form
//       with columns.Derivative {
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
//
//   /**
//     */
//   case object XtIndexOptions extends KeyValueStores.KV[Indexes.Key, XtIndexOption]
// }
//
// /** Private lending instruments.
//   *
//   * TODO: do these next
//   */
// object Lending {
//
//   /**
//     */
//   final case class BulletPayment(
//       matures: ZonedDateTime
//   ) extends Form
//       with columns.Maturity {
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
//
//   /**
//     */
//   final case class CreditLine(
//       matures: ZonedDateTime,
//       frequency: Frequency // = Frequency.F1Q
//   ) extends Form
//       with columns.Maturity {
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
//
//   /**
//     */
//   final case class AmortizingLoan(
//       matures: ZonedDateTime,
//       frequency: Frequency // = Frequency.F1M
//   ) extends Form
//       with columns.Maturity {
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
//
//   /**
//     */
//   final case class ConvertibleNote(
//       matures: ZonedDateTime,
//       discount: Double Refined IsUnitInterval.`[0,1)`,
//       cap: Option[Double Refined Positive]
//   ) extends Form
//       with columns.Maturity {
//
//     /** FIXME: implement */
//     def contract: Contract = ???
//   }
// }

// /** Groups of related `Form`s.
//   */
// object forms
//     extends PrimaryCapital  // nececssary
//     with VanillaDerivatives // fun
//     with Lending // as one does
// with Fx                 // WIP
// with Exotics            // primarily for hedge funds
// with Ibor               // primariy for banks

// /** Necessary annotations for data loaded from external sources. */
// private[deftrade] trait Provenance {
//   def sourcedAt: Instant
//   def sourcedFrom: String Refined Url
// }
