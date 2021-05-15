package io.deftrade
package model.layers

import time._, money._, contracts._, keyval._, refinements._
import model.slices.keys._
import model.slices.{ CapitalStack, Metas }

import cats.implicits._
import cats.{ Contravariant, Defer, Eq, Eval, Monad, Order, Semigroup, Show }
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

  import model.slices.Form

  final def contractGoverning(instrument: Instruments.Key)(
      implicit
      sg: Semigroup[Form]
  ): IO[Result[Contract]] =
    for {
      links <- papers.instrumentsForms getAll instrument
      forms <- links.fold(???)(_ map (papers.forms get _.form)).sequence
    } yield Result safe forms.sequence.fold(contracts.zero)(_.reduce.contract)

  /**
    * placeholder
    */
  final lazy val instrumentContracts: Map[Instruments.Key, IO[Result[Contract]]] =
    Map.empty

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

    // /**
    //   */
    // sealed abstract case class Contract private (
    //     final val contract: contracts.Contract
    // ) extends Numéraire.InKind
    //
    // /**
    //   */
    // object Contract {
    //   def apply(contract: => contracts.Contract): Contract =
    //     new Contract(contract) {}
    // }
    //
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
  case object Instruments extends KeyValueStores.KV[USIN, Instrument]

  /**
    */
  case object ExchangeTradedInstruments extends KeyValueStores.KV[ISIN, Instrument]

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
