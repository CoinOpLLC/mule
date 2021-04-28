package io.deftrade
package model.layers

import time._, money._, contracts._, keyval._, refinements._
import model.slices.keys._
import model.slices.{ CapitalStack, Metas }

import cats.implicits._
import cats.{ Contravariant, Defer, Eq, Monad, Order, Show }
import cats.data.NonEmptyList
import cats.derived.{ auto, semiauto }

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined }
import refined.string.{ Url }
import refined.cats._

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

trait Paper { module: ModuleTypes with Person =>

  sealed abstract case class ContractKey[IsP] private (
      final val key: String Refined IsP
  ) extends model.slices.ContractKey[IsP]
      with Numéraire.InKind

  object ContractKey {

    import CapitalStack._

    def apply(key: String Refined IsUSIN): ContractKey[IsUSIN] =
      new ContractKey(key) { self =>
        final def contract[F[_]: Monad: Defer]: F[Contract] = {

          // Note: papers.instrumentsForms has everything you need to define the contract
          // (and it's in scope!)

          val x = for {
            links <- papers.instrumentsForms getAll self
            ls: List[Forms.Link] = links match {
              case Some(NonEmptyList(h, t)) => h :: t
              case None                     => Nil
            }
            form <- (ls map (link => papers.forms get link.form)).sequence
          } yield form

          //  map {
          //   case Some(Bond(face, coupon, issued, matures, defaulted)) => ???
          // }

          // import std.zeroCouponBond

          // /** A `Bill` is a kind of zero coupon bond. */
          // def contract[F[_]: Monad]: F[Contract] =
          //   zeroCouponBond(maturity = matures.toInstant, face = Currency.USD(1000.0)).pure[F]

          def res: Contract = ???
          res.pure[F]
        }
      }

    implicit def contractKeyOrder[IsP]: Order[ContractKey[IsP]] =
      Order by (_.key)

    implicit def contractKeyShow[IsP]: Show[ContractKey[IsP]] =
      Contravariant[Show].contramap(Show[String Refined IsP])(_.key)
  }

  import model.slices.Form
  // trait Form extends model.slices.Form

  /** `Store`s related to `Contract`s.
    */
  case class Papers(
      instruments: Instruments.KeyValueStore[IO],
      forms: Forms.ValueStore[IO],
      instrumentsForms: InstrumentsForms.KeyValueStore[IO],
      novations: Novations.ValueStore[IO]
  )

  /** `Store`s related to `Contract`s. TODO: something besides cake pattern please.
    */
  val papers: Papers

  /** Models a tradeable thing.
    *
    * Policy: Only legal entities (and not natural persons) may issue `Instruments`.
    */
  sealed abstract case class Instrument private (
      final val symbol: Label,
      final val issuedBy: LegalEntities.Key,
      final val issuedIn: Option[CurrencyLike]
  )

  /**
    */
  object Instrument {

    def apply(
        symbol: Label,
        issuedBy: LegalEntities.Key,
        issuedIn: Option[CurrencyLike],
    ): Instrument =
      new Instrument(symbol, issuedBy, issuedIn) {}

    implicit def instrumentEq: Eq[Instrument]     = { import auto.eq._; semiauto.eq }
    implicit def instrumentShow: Show[Instrument] = { import auto.show._; semiauto.show }
  }

  /** Indexed by CUSIPs and other formats.
    * An `Instrument` ''evolves'' over time as the `form.Contract` state is updated.
    */
  case object Instruments extends KeyValueStores.KV[ContractKey[IsUSIN], Instrument]

  /**
    */
  case object ExchangeTradedInstruments extends KeyValueStores.KV[ContractKey[IsISIN], Instrument]

  /**
    */
  case object Forms extends ValueStores.SADT[Form] {

    sealed abstract case class Link private (form: Id)

    object Link {

      def apply(form: Id): Link =
        new Link(form) {}

      implicit lazy val linkEq: Eq[Link]     = { import auto.eq._; semiauto.eq }
      implicit lazy val linkShow: Show[Link] = { import auto.show._; semiauto.show }
    }
  }

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
