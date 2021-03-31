package io.deftrade
package model.slices

import keyval._
import time._
import contracts.{ Contract, Numéraire }
import money.Currency
import model.slices.keys.{ USIN }
import refinements.{ Label }

import spire.math.Fractional

import cats.implicits._
import cats.{ Contravariant, Defer, Eq, Monad, Order, Show }
import cats.derived.{ auto, semiauto }

import eu.timepit.refined
import refined.api.Refined
import refined.numeric.Positive
import refined.cats._

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import io.circe.refined._

/**
  */
trait ContractKey extends Numéraire.InKind {

  val usin: USIN
}

/**
  */
object ContractKey {

  implicit def contractKeyOrder: Order[ContractKey] =
    Order by (_.usin)

  implicit def contractKeyShow: Show[ContractKey] =
    Contravariant[Show].contramap(Show[USIN])(_.usin)
  // implicit def contractKeyOrder: Order[ContractKey] = { import auto.order._; semiauto.order }
  // implicit def contractKeyShow: Show[ContractKey]   = { import auto.show._; semiauto.show }
}

sealed trait Form extends Product

object Form {

  implicit lazy val formEq: Eq[Form]     = { import auto.eq._; semiauto.eq }
  implicit lazy val formShow: Show[Form] = { import auto.show._; semiauto.show }

  implicit lazy val formEncoder: Encoder[Form] = { deriveEncoder }
  implicit lazy val formDecoder: Decoder[Form] = { deriveDecoder }
}

/** Parameters common to multiple `Form`s.
  */
object columns {

  final type Quantity = Double

  /** ShareClass class of the shares represented by this `Instrument`.
    */
  sealed trait ShareClass { self: Form =>
    def shareClass: Option[Label]
  }

  /** Tracks a (non-empty) set of `Instruments.Key`s
    *
    * Enumerating the components of an [[forms.Index]] such as the DJIA is the typical use case.
    */
  sealed trait Tracker { self: Form =>
    def members: Set[ContractKey]
  }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.) */
  sealed trait Maturity { self: Form =>
    def matures: ZonedDateTime
  }

  /** All derivative contracts (such as `Future`s) are assumed to be struck at a certain price,
    * and expire on a certain day.
    */
  sealed trait Derivative extends Expiry with Strike {
    def underlier: ContractKey
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

/**
  */
object PrimaryCapital {

  import columns._

  /**
    */
  sealed abstract case class CommonStock private (final val shareClass: Option[Label])
      extends Form
      with ShareClass

  object CommonStock {

    private[deftrade] def apply(shareClass: Option[Label]): CommonStock =
      new CommonStock(shareClass) {}

    implicit lazy val cStkEq: Eq[CommonStock]     = { import auto.eq._; semiauto.eq }
    implicit lazy val cStkShow: Show[CommonStock] = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object CommonStocks extends KeyValueStores.KV[ContractKey, CommonStock]

  sealed abstract case class PreferredStock private (
      final val shareClass: Option[Label],
      final val preference: Quantity Refined Positive,
      final val participating: Boolean,
  ) extends Form
      with ShareClass

  object PreferredStock {

    def apply(
        shareClass: Label,
        preference: Quantity Refined Positive,
        participating: Boolean,
    ): PreferredStock =
      apply(shareClass.some, preference, participating)

    private[deftrade] def apply(
        shareClass: Option[Label],
        preference: Quantity Refined Positive,
        participating: Boolean,
    ): PreferredStock =
      new PreferredStock(shareClass, preference, participating) {}

    implicit lazy val pfStkEq: Eq[PreferredStock]     = { import auto.eq._; semiauto.eq }
    implicit lazy val pfStkShow: Show[PreferredStock] = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object PreferredStocks extends KeyValueStores.KV[ContractKey, PreferredStock]

  /** Assume semiannual, Treasury-style coupons.
    *
    * TODO: Note that the state is cursor-like; evolve this further.
    */
  sealed abstract case class Bond private (
      final val coupon: Quantity, // per 100 face
      final val issued: Instant,
      final val matures: ZonedDateTime,
      final val unpaidCoupons: List[ZonedDateTime], // soonest due first
      final val paidCoupons: List[Instant] // most recent first
  ) extends Form
      with Maturity

  object Bond {

    def apply(
        coupon: Quantity, // per 100 face
        issued: Instant,
        matures: ZonedDateTime,
        unpaidCoupons: List[ZonedDateTime], // soonest due first
        paidCoupons: List[Instant] // most recent first
    ): Bond =
      new Bond(coupon, issued, matures, unpaidCoupons, paidCoupons) {}

    implicit lazy val bondEq: Eq[Bond]     = { import auto.eq._; semiauto.eq }
    implicit lazy val bondShow: Show[Bond] = { import auto.show._; semiauto.show }
  }

  /** `Bonds` (as opposed to loans) are always issued by entities, never by natural persons.
    */
  case object Bonds extends KeyValueStores.KV[ContractKey, Bond]

  // /**
  //   */
  // sealed abstract case class Bill(
  //     override val matures: ZonedDateTime
  // ) extends Form
  //     with Maturity {
  //
  //   import std.zeroCouponBond
  //
  //   // /** A `Bill` is a kind of zero coupon bond. */
  //   // def contract[F[_]: Monad]: F[Contract] =
  //   //   zeroCouponBond(maturity = matures.toInstant, face = Currency.USD(1000.0)).pure[F]
  // }
  //
  // object Bill
  //
  // /** `Bills` are always issued by entities, never by natural persons.
  //   */
  // case object Bills extends KeyValueStores.KV[ContractKey, Bill]
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
//     * TODO:
//     * `Oracle` events:
//     * - Equity Financing
//     * - Liquidity
//     * - Dissolution
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
