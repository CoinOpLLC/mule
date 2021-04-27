package io.deftrade
package model.slices

import keyval._
import time._
import money.Currency
import model.slices.keys._
import refinements.{ Label }

import spire.math.Fractional
// import spire.compat.numeric

import enumeratum.EnumEntry

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
trait ContractKey[IsP] {

  val key: String Refined IsP
}

/**
  */
object ContractKey {

  // def contract[F[_]: Monad: Defer](ck: ContractKey): F[Contract]

  implicit def contractKeyOrder[IsP]: Order[ContractKey[IsP]] =
    Order by (_.key)

  implicit def contractKeyShow[IsP]: Show[ContractKey[IsP]] =
    Contravariant[Show].contramap(Show[String Refined IsP])(_.key)

  implicit def contractKeyEncoder[IsP]: Encoder[ContractKey[IsP]] = ???
  implicit def contractKeyDecoder[IsP]: Decoder[ContractKey[IsP]] = ???
}

sealed trait Form extends Product

object Form {

  implicit lazy val formEq: Eq[Form]     = { import auto.eq._; semiauto.eq }
  implicit lazy val formShow: Show[Form] = { import auto.show._; semiauto.show }

  implicit lazy val formEncoder: Encoder[Form] = { deriveEncoder }
  implicit lazy val formDecoder: Decoder[Form] = { deriveDecoder }
}

sealed trait Columns[N] {

  /**
    */
  final type Quantity = N

  /** ShareClass class of the shares represented by this `Instrument`.
    */
  sealed trait ShareClass { self: Form =>
    def shareClass: Option[Label]
  }

  /** Bonds (primary capital) `mature` (as opposed to `expire`.) */
  sealed trait Maturity { self: Form =>
    def issued: Instant
    def matures: ZonedDateTime
  }

  // /** Tracks a (non-empty) set of `Instruments.Key`s
  //   *
  //   * Enumerating the components of an [[forms.Index]] such as the DJIA is the typical use case.
  //   */
  // sealed trait Tracker { self: Form =>
  //   def members: Trackers.Key
  // }
  // case object Trackers extends KeyValueStores.KV[USIN, USIN]

  /** `Expiry` only applies to `Derivative`s.
    */
  sealed trait Expiry { self: Form with Strike =>
    def expires: ZonedDateTime
  }

  /** `Strike`s only apply to `Derivative`s.
    *
    * TODO: {{{def logMoneynessStrike(strike: Quamtity, forward: Quamtity): Strike }}}
    */
  sealed trait Strike { self: Form with Expiry =>
    def strike: Quantity
  }

  /** All derivative contracts (such as `Future`s) are assumed to be struck at a certain price,
    * and expire on a certain day.
    */
  sealed trait Derivative[IsP] extends Expiry with Strike { self: Form =>
    def underlier: ContractKey[IsP]
  }
}

/** The batteries we included.
  *
  * TODO: modularize - the SADT derivation is too fragile rn
  */
object CapitalStack extends Columns[Double] {

  /**
    */
  final case class CommonStock(shareClass: Option[Label]) extends Form with ShareClass

  case object CommonStock {

    implicit lazy val cStkEq: Eq[CommonStock]     = { import auto.eq._; semiauto.eq }
    implicit lazy val cStkShow: Show[CommonStock] = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object CommonStocks extends KeyValueStores.KV[ContractKey[IsCUSIP], CommonStock]

  final case class PreferredStock(shareClass: Option[Label],
                                  preference: Quantity Refined Positive,
                                  participating: Boolean,
  ) extends Form
      with ShareClass

  object PreferredStock {

    implicit lazy val pfStkEq: Eq[PreferredStock]     = { import auto.eq._; semiauto.eq }
    implicit lazy val pfStkShow: Show[PreferredStock] = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object PreferredStocks extends KeyValueStores.KV[ContractKey[IsCUSIP], PreferredStock]

  /** Assume semiannual, Treasury-style coupons.
    *
    * TODO: Note that the state is cursor-like; evolve this further.
    */
  final case class Bond(face: Quantity,
                        coupon: Quantity, // per 100 face
                        issued: Instant,
                        matures: ZonedDateTime,
                        defaulted: Option[Instant])
      extends Form
      with Maturity

  object Bond {

    implicit lazy val bondEq: Eq[Bond]     = { import auto.eq._; semiauto.eq }
    implicit lazy val bondShow: Show[Bond] = { import auto.show._; semiauto.show }
  }

  /** `Bonds` (as opposed to loans) are always issued by entities, never by natural persons.
    */
  case object Bonds extends KeyValueStores.KV[ContractKey[IsISIN], Bond]

  /**
    */
  final case class Bill(
      face: Quantity,
      issued: Instant,
      matures: ZonedDateTime
  ) extends Form
      with Maturity

  object Bill {

    implicit lazy val bondEq: Eq[Bill]     = { import auto.eq._; semiauto.eq }
    implicit lazy val bondShow: Show[Bill] = { import auto.show._; semiauto.show }
  }

  /** `Bills` are always issued by entities, never by natural persons.
    */
  case object Bills extends KeyValueStores.KV[ContractKey[IsISIN], Bill]

// }
//
// /** And by "vanilla" we mean an exchange traded derivative (ETD), no over-the-counter (OTC)
//   */
// sealed abstract class VanillaDerivatives[N: Fractional: Show] extends PrimaryCapital[N] {

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

  // /**
  //   */
  // final case class Index(members: Set[ContractKey]) extends Form with Tracker
  //
  // object Index {
  //
  //   implicit lazy val indexEq: Eq[Index]     = { import auto.eq._; semiauto.eq }
  //   implicit lazy val indexShow: Show[Index] = { import auto.show._; semiauto.show }
  // }
  //
  // /**
  //   */
  // case object Indexes extends KeyValueStores.KV[ContractKey, Index]

  /** Exchange Traded Derivative - Future (ETD) */
  final case class XtFuture(expires: ZonedDateTime,
                            underlier: ContractKey[IsISIN],
                            strike: Quantity)
      extends Form
      with Derivative[IsISIN]

  object XtFuture {

    implicit lazy val xtFutureEq: Eq[XtFuture]     = { import auto.eq._; semiauto.eq }
    implicit lazy val xtFutureShow: Show[XtFuture] = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object XtFutures extends KeyValueStores.KV[ContractKey[IsISIN], XtFuture]
  //
  // /** Exchange Traded Derivative - Option (ETD) */
  // final case class XtOption(putCall: PutCall,
  //                     expires: ZonedDateTime,
  //                     underlier: CommonStocks.Key,
  //                     strike: Quantity)
  //     extends Form
  //     with Derivative
  //
  // /** TODO: recheck that `Isin` thing... */
  // case object XtOptions extends KeyValueStores.KV[ContractKey, XtOption]
  //
  // /**
  //   */
  // final case class XtFutureOption(putCall: PutCall,
  //                           expires: ZonedDateTime,
  //                           underlier: XtFutures.Key,
  //                           strike: Quantity)
  //     extends Form
  //     with Derivative
  //
  // /**
  //   */
  // case object XtFutureOptions extends KeyValueStores.KV[ContractKey, XtFutureOption]
  //
  // /**
  //   */
  // final case class XtIndexOption(putCall: PutCall,
  //                          expires: ZonedDateTime,
  //                          underlier: Indexes.Key,
  //                          strike: Quantity)
  //     extends Form
  //     with Derivative
  //
  // /**
  //   */
  // case object XtIndexOptions extends KeyValueStores.KV[ContractKey, XtIndexOption]
}

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
