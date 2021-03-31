package io.deftrade
package model.layers

import time._, money._, contracts._, keyval._, refinements._
import model.slices.keys.{ ISIN, USIN }
import model.slices.{ ContractKey => CK, Metas }

import cats.implicits._
import cats.{ Contravariant, Defer, Eq, Monad, Order, Show }
import cats.derived.{ auto, semiauto }

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined }
import refined.string.{ Url }
import refined.cats._

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

trait Paper { module: ModuleTypes with Person =>

  sealed abstract case class ContractKey private (
      final val usin: USIN
  ) extends CK

  object ContractKey {

    def apply(usin: USIN): ContractKey =
      new ContractKey(usin) { self =>
        final def contract[F[_]: Monad: Defer]: F[Contract] = {

          // Note: papers.instrumentsForms has everything you need to define the contract
          // (and it's in scope!)
          // papers.instrumentForms get self flatMap { _ match {
          //  case Foo(b, c1, c2) =>
          // } }
          def res: Contract = ???
          res.pure[F]
        }
      }

    implicit def contractKeyOrder: Order[ContractKey] =
      Order by (_.usin)

    implicit def contractKeyShow: Show[ContractKey] =
      Contravariant[Show].contramap(Show[USIN])(_.usin)
  }

  final type Form = model.slices.Form

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
  case object Instruments extends KeyValueStores.KV[ContractKey, Instrument]

  /**
    */
  case object ExchangeTradedInstruments extends KeyValueStores.KV[ISIN, Instrument]

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
