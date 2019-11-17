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
package layers

import keyval._, time._, money._

import cats.implicits._
import cats.{ Monad }
import cats.data.{ Kleisli, NonEmptySet }

import eu.timepit.refined
import refined.cats.refTypeOrder
import refined.auto._

import scala.collection.immutable.SortedSet
import scala.language.higherKinds

/** */
trait OrderManagement { self: MarketData with Ledger with ModuleTypes =>

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
      trade: Trade.Id,
      currency: CurrencyLike,
      limit: Option[MonetaryAmount]
  )

  /**
    * Once placed, an [[Order]] may be modified or canceled,
    * and so is modeled as an entity which can evolve.
    */
  object Order extends WithOpaqueKey[Long, Order] {

    /**
      * Note that currency is required even for market orders.
      */
    def market[C: Currency](market: Market.Key, trade: Trade.Id): Order =
      new Order(instant, market, trade, Currency[C], none) {}

    /** */
    def limit[C: Currency](market: Market.Key, trade: Trade.Id, limit: Mny[C]): Order =
      new Order(instant, market, trade, Currency[C], limit.amount.some) {}
  }

  /**
    * What actually happened to the [[Order]] at the [[Market]].
    *
    * TODO: describe in more detail how partial executions end up as multiple [[Transaction]]s.
    */
  sealed abstract case class Execution(
      ts: Instant,
      order: Order.Key,
      oms: OMS.Key,
      tx: Transaction.Id
  )

  /**
    * `Execution`s are pure values (do not evolve.)
    *
    * Note: this implies that so-called "broken trades" require explicit modelling.
    */
  object Execution extends WithId[Execution]

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
  sealed abstract case class OMS[F[_]: Monad] private (
      entity: LegalEntity.Key,
      contra: Folio.Key,
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
    type Phase[T, R] = Kleisli[ResultT[F, *], T, R]

    /** */
    final def process[C: Currency, A](
        p: Folio.Key,
        a: Allocation
    )(
        folios: FolioTable
    )(
        block: => A
    ): Phase[A, FolioTable] =
      riskCheck[C, A](p)(block) andThen
        trade(p) andThen
        allocate(a) andThen
        settle(folios)(p)

    /** */
    def riskCheck[C: Currency, A](p: Folio.Key)(a: A): Phase[A, Order]

    /** */
    def trade(p: Folio.Key): Phase[Order, Execution]

    /** */
    def allocate(a: Allocation): Phase[Execution, Execution]

    /** */
    final def settle(
        folios: Map[Folio.Key, Folio.Value]
    )(
        p: Folio.Key
    ): Phase[Execution, FolioTable] = ???
  }

  /** */
  type Allocation = UnitPartition[Folio.Key, Quantity]

  /** Namespace placeholder */
  object Allocation

  /**
    * FIXME: augment/evolve creation pattern.
    */
  object OMS extends WithOpaqueKey[Long, OMS[cats.Id]] {

    /**
      * Each OMS must maintain a contra [[Ledger.Folio.Key]].
      * The creation of this account (and its [[Ledger.Folio.Key]]) must occur
      * before the OMS is created.
      *
      */
    def apply[F[_]: Monad](
        key: LegalEntity.Key,
        contraAccount: Folio.Key,
        market: Market.Key,
        ms: Market.Key*
    ): OMS[F] =
      new OMS[F](key, contraAccount, NonEmptySet(market, SortedSet(ms: _*))) {

        /** */
        def riskCheck[C: Currency, A](p: Folio.Key)(a: A): Phase[A, Order] = ???

        /** */
        def trade(p: Folio.Key): Phase[Order, Execution] = ???

        /** */
        def allocate(a: Allocation): Phase[Execution, Execution] = ???

      }
  }
}
