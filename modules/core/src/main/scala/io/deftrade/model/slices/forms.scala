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
import cats.{ Defer, Eq, Monad, Order, Show }
import cats.derived.{ auto, semiauto }

import eu.timepit.refined
import refined.api.Refined
import refined.numeric.Positive
import refined.cats._

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import io.circe.refined._

sealed abstract case class ContractKey private (final val usin: USIN) extends Numéraire.InKind
object ContractKey {

  /** FIXME implement
    */
  private[deftrade] def apply(usin: USIN): ContractKey =
    new ContractKey(usin) {

      final def contract[F[_]: Monad: Defer]: F[Contract] =
        ???
    }
  implicit def contractKeyOrder: Order[ContractKey] = { import auto.order._; semiauto.order }
  implicit def contractKeyShow: Show[ContractKey]   = { import auto.show._; semiauto.show }
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

/** TODO:
  * `Oracle` events:
  * - Equity Financing
  * - Liquidity
  * - Dissolution
  */
object ConvertibleNote
