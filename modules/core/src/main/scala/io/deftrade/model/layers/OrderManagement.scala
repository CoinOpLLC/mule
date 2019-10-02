package io.deftrade
package model
package layers

import keyval._, time._, money._

import cats.implicits._
import cats.{ Foldable, Monad, SemigroupK }
import cats.data.{ EitherT, Kleisli, NonEmptySet }

import eu.timepit.refined
//import refined.cats._
import refined.auto._

import scala.collection.immutable.SortedSet
import scala.language.higherKinds

/** */
trait OrderManagement {
  self: MarketData with Ledger with ModuleTypes =>

  /**
    * What the client wants [[Execution]] of.
    *
    * Note: a denominating currency is always required by the [[MarketData.Exchange]]s,
    * in order to fully specify the trade, even if there is no limit amount attached.
    *
    * TODO: revisit parent/child orders
    */
  sealed abstract case class Order(
      ts: Instant,
      market: Market.Key,
      trade: Trade,
      currency: CurrencyLike,
      limit: Option[MonetaryAmount]
  )

  /** */
  object Order extends WithOpaqueKey[Long, Order] {

    /**
      * Note that currency is required even for market orders.
      */
    def market[C: Currency](market: Market.Key, trade: Trade): Order =
      new Order(instant, market, trade, Currency[C], none) {}

    /** */
    def limit[C: Currency](market: Market.Key, trade: Trade, limit: Mny[C]): Order =
      new Order(instant, market, trade, Currency[C], limit.amount.some) {}
  }

  /**
    * What actually happened to the [[Order]].
    */
  sealed abstract case class Execution(
      ts: Instant,
      oms: OMS.Key,
      orderKey: Order.Key,
      tx: Transaction
  )

  /** */
  object Execution extends WithOpaqueKey[Long, Execution] {}

  /**
    *`OMS` := Order Management System. Ubiquitous domain acronym.
    *
    * The methods on `OMS` return [[cats.data.Kleisli]] arrows, which are intended to be chained with
    * `andThen`.
    *
    * Reference:
    * [[https://livebook.manning.com/#!/book/functional-and-reactive-domain-modeling/chapter-4/270 Functional and Reactive Domain Modelling, section 4.4]]
    *
    * TODO: Flesh this out - currently just a sketch
    *   - `SemigroupK` for `Kleisli` is restrictive.
    *   - OTOH it *is* a pipeline and so the Kleisli modeling has fidelity.
    *   - If it is possible for the arrows to be sequenced in a semantically incorrect way per the
    * domain model, use phantom types to ensure proper sequencing.
    *
    */
  sealed abstract case class OMS[F[_]: Monad: SemigroupK: Foldable] private (
      entity: LegalEntity.Key,
      contra: Account.Key,
      markets: NonEmptySet[Market.Key]
  ) {

    /**
      * `Folio` is a `Map` of `Map`s, which, normalized and written out as a list,
      * has rows of type: {{{
      *   (Folio.Key, Instrument.Key, Quantity)
      * }}}
      */
    type FolioTable = Map[Folio.Key, Folio.Value]

    /** */
    type ResultF[R] = EitherT[F, Fail, R]

    /** */
    type Phase[T, R] = Kleisli[ResultF, T, R]

    /** */
    final def process[C: Currency, A](p: Account.Key, a: Allocation)(
        folios: FolioTable
    )(
        block: => A
    ): Phase[A, FolioTable] =
      riskCheck[C, A](p)(block) andThen trade(p) andThen allocate(a) andThen settle(folios)(p)

    /** */
    def riskCheck[C: Currency, A](p: Account.Key)(a: A): Phase[A, Order] =
      ???

    /** */
    def trade(p: Account.Key): Phase[Order, Execution] =
      ???

    /** */
    def allocate(a: Allocation): Phase[Execution, Execution] =
      ???

    /** */
    final def settle(
        folios: Map[Folio.Key, Folio.Value]
    )(
        p: Account.Key
    ): Phase[Execution, FolioTable] = ???
  }

  /** */
  type Allocation = UnitPartition[Account.Key, Quantity]

  /** Namespace placeholder */
  object Allocation

  /**
    * FIXME: augment/evolve creation pattern.
    */
  object OMS extends WithOpaqueKey[Long, OMS[cats.Id]] {

    /**
      * Each OMS must maintain a contra [[Ledger.Account]].
      * The creation of this account (and its [[Ledger.Account.Key]]) must occur
      * before the OMS is created.
      *
      */
    def apply[F[_]: Monad: SemigroupK: Foldable](
        key: LegalEntity.Key,
        contraAccount: Account.Key,
        market: Market.Key,
        ms: Market.Key*
    ): OMS[F] =
      new OMS[F](key, contraAccount, NonEmptySet(market, SortedSet(ms: _*))) {}
  }
}
