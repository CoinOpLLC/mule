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

import keyval._, time._, money._, refinements.{ IsLabel }

import cats.implicits._
// import cats.{ Sync }
import cats.data.{ Kleisli, NonEmptySet }
import cats.effect.{ Sync }
import eu.timepit.refined
import refined.cats.refTypeOrder
import refined.auto._

import fs2.Stream

import scala.collection.immutable.SortedSet

/** */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait OrderManagement { self: MarketData with Ledger with ModuleTypes =>

  /** */
  type Allocation = UnitPartition[Folio.Key, Quantity]

  /** Namespace placeholder */
  object Allocation

  /** `link` table */
  case class OMSmarkets(market: Market.Key)

  /** */
  object OMSmarkets extends WithId[OMSmarkets]

  /** */
  type OMSrecord = (LegalEntity.Key, Folio.Key, Folio.Key, OMSmarkets.Id)

  /**
    *`OMS` := Order Management System. Ubiquitous domain acronym.
    *
    * The methods on `OMS` return [[cats.data.Kleisli]] arrows, which are intended to be chained with
    * `andThen`.
    *
    * Reference:
    * [[https://livebook.manning.com/#!/book/functional-and-reactive-domain-modeling/chapter-4/270
    Functional and Reactive Domain Modelling, section 4.4]]
    *
    * TODO: Revisit [[cats.data.Kleisli]] usage
    *   - Why not [[fs2.Pipe]]?
    *   - Order processing *is* a natural pipeline, and so the Kleisli modeling has fidelity.
    */
  sealed abstract case class OMS[F[_]: Sync] private (
      host: LegalEntity.Key,
      entry: Folio.Key,
      contra: Folio.Key,
      markets: NonEmptySet[Market.Key],
  ) {

    /** */
    type ToStreamOf[T, R] = Kleisli[ResultT[Stream[F, *], *], T, R]

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
      * and so is modeled as an `entity` which can evolve over time.
      */
    object Order extends WithOpaqueKey[Long, Order] {

      /**
        * Note that currency is required even for market orders.
        */
      def market[C: Currency](market: Market.Key, trade: Trade.Id): Order =
        new Order(instant, market, trade, Currency[C], none) {}

      /** */
      def limit[C: Currency](market: Market.Key, trade: Trade.Id, limit: Money[C]): Order =
        new Order(instant, market, trade, Currency[C], limit.amount.some) {}
    }

    /**
      * What actually happened to the [[Order]] at the [[Market]].
      *
      * Multiple partial executions reference the same `Order`
      * and end up as multiple [[Transaction]]s.
      */
    sealed abstract case class Execution(
        at: Instant,
        order: Order.Key,
        transaction: Transaction.Id
    )

    /**
      * `Execution`s are pure values (do not evolve.)
      *
      * Note: this implies that so-called "broken trades" require explicit modelling.
      */
    object Execution extends WithId[Execution]

    /**
      * FIXME: address the issue of scheduling T+2 etc.
      */
    final def process[C: Currency, A](
        allocation: Allocation
    )(
        block: => A
    ): A ToStreamOf Unit =
      riskCheck[C, A](block) andThen
        trade andThen
        allocate(allocation) andThen
        settle

    /** */
    def riskCheck[C: Currency, A](a: A): A ToStreamOf Order

    /**  */
    def trade: Order ToStreamOf Execution

    /** Moves the traded [[capital.Instrument]]s to their final destination [[Folio]]. */
    def allocate(a: Allocation): Execution ToStreamOf Execution

    /**
      * Updates the actual [[Folio]]s, with [[Account]] specific (and this `Folio` specific)
      * cash account [[capital.Instrument]]s substituted for the raw [[money.Currency]]
      * pseudo `Instrument` specified in the [[Order]] and enumerated within the [[Leg]]s
      * of the [[Trade]] specified in the [[Transaction]].
      */
    def settle: Execution ToStreamOf Unit
  }

  /**
    * FIXME: augment/evolve creation pattern.
    */
  object OMS extends WithRefinedKey[String, IsLabel, OMSrecord] {

    /**
      * Each OMS must maintain a contra [[Ledger.Folio.Key]].
      * The creation of this account (and its [[Ledger.Folio.Key]]) must occur
      * before the OMS is created.
      *
      */
    def apply[F[_]: Sync](
        host: LegalEntity.Key,
        entry: Folio.Key,
        contra: Folio.Key,
        market: Market.Key,
        ms: Market.Key*
    ): OMS[F] =
      new OMS[F](host, entry, contra, NonEmptySet(market, SortedSet(ms: _*))) {

        /** */
        def riskCheck[C: Currency, A](a: A): A ToStreamOf Order = ???

        /** This phase is implemented by the brokerage api integration code. */
        def trade: Order ToStreamOf Execution = ???

        /** */
        def allocate(a: Allocation): Execution ToStreamOf Execution = ???

        /** */
        def settle: Execution ToStreamOf Unit = ???
      }
  }
}
