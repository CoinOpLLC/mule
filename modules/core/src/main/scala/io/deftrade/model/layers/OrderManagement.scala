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

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait OrderManagement { self: MarketData with Ledger with ModuleTypes =>

  /**
    */
  type Allocation = UnitPartition[Folio.Key, Quantity]

  /** Namespace placeholder */
  object Allocation

  /**
    */
  type OMSrecord = (LegalEntity.Key, Folio.Key, Folio.Key, Market.Key)

  /**
    * `OMS` := Order Management System. Ubiquitous domain acronym.
    *
    * The methods on `OMS` return [[cats.data.Kleisli]] arrows, which are intended to be chained with
    * `andThen`.
    *
    * TODO: Revisit [[cats.data.Kleisli]] usage
    *   - Why not [[fs2.Pipe]]?
    *   - Order processing *is* a natural pipeline, and so the Kleisli modeling has fidelity.
    */
  sealed abstract case class OMS[F[_]](
      host: LegalEntity.Key,
      entry: Folio.Key,
      contra: Folio.Key,
      markets: NonEmptySet[Market.Key]
  ) {

    /**
      */
    type ToStreamOf[T, R] = Kleisli[ResultT[Stream[F, *], *], T, R]

    /**
      * What it is that the client wants [[Execution]] of.
      */
    sealed abstract case class Order private (
        at: Instant,
        market: Market.Key,
        trade: Trade.Id,
        currency: CurrencyLike,
        limit: Option[MonetaryAmount],
        goodTill: Option[Instant]
    )

    /**
      */
    object Order extends WithOpaqueKey[Long, Order] {

      /**
        * Note that currency is required even for market orders.
        */
      def market[C: Currency](market: Market.Key, trade: Trade.Id): Order =
        new Order(instant, market, trade, Currency[C], none, none) {}

      /**
        */
      def limit[C: Currency](market: Market.Key, trade: Trade.Id, limit: Money[C]): Order =
        new Order(instant, market, trade, Currency[C], limit.amount.some, none) {}
    }

    /**
      * What actually happened to the [[Order]] at the [[Market]].
      *
      * FIXME: canceled order signals are TBD.
      */
    sealed abstract case class Execution(
        at: Instant,
        order: Order.Key,
        transaction: Transaction.Id
    )

    /**
      * `Execution`s are pure values (do not evolve.)
      *
      * Implication for domain modellers: so-called "broken trades" require explicit modelling.
      */
    object Execution extends WithId.Aux[Execution] {}

    def riskCheck: Order ToStreamOf Order

    def trade: Order ToStreamOf Execution

    /** FIXME: change api to
      * bind allocations at the beginning of processing. The order processing ppipe need know
      * nothing about the details of the account and the sub accounts.
      *
      * FIXME: implement (it's a system affordance)
      */
    final def allocate(a: Allocation): Execution ToStreamOf Execution = ???

    /**
      * Settlement updates the actual [[Folio]]s,
      * with [[Account]] specific (and this `Folio` specific)
      * cash account [[capital.Instrument]]s substituted for the raw [[money.Currency]]
      * pseudo `Instrument` specified in the [[Order]] and enumerated within the [[Leg]]s
      * of the [[Trade]] specified in the [[Transaction]].
      *
      * FIXME: implement (it's a system affortance)
      */
    final val settle: Execution ToStreamOf Confirmation = ???

    /**
      * Order processing is all just kleisli arrows? Always has been.
      */
    final def process(allocation: Allocation): Order ToStreamOf Confirmation =
      riskCheck andThen trade andThen allocate(allocation) andThen settle
  }

  /**
    */
  object OMS extends WithRefinedKey[String, IsLabel, OMSrecord] {

    /**
      * TODO: evole
      */
    def mk[F[_]: Sync](
        host: LegalEntity.Key,
        entry: Folio.Key,
        contra: Folio.Key,
        market: Market.Key,
        ms: Market.Key*
    ): OMS[F] =
      new OMS[F](host, entry, contra, NonEmptySet(market, SortedSet(ms: _*))) {

        /**
          */
        def riskCheck: Order ToStreamOf Order = ???

        /** This phase is implemented by the brokerage api integration code. */
        def trade: Order ToStreamOf Execution = ???
      }
  }
}
