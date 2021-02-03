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
import cats.{ Eq, Order, Show }
import cats.derived.{ auto, semiauto }
import cats.data.{ Kleisli }

import eu.timepit.refined
import refined.cats.refTypeOrder
import refined.auto._
import refined.cats._

import fs2.Stream

/**
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait OrderManagement { self: MarketData with Ledger with ModuleTypes =>

  /**
    */
  type Allocation = UnitPartition[Folios.Key, Quantity]

  /** Namespace placeholder */
  object Allocation

  /**
    */
  type OMSrecord = (LegalEntities.Key, Folios.Key, Folios.Key, Exchanges.Key)

  /** `OMS` := Order Management System. Ubiquitous domain acronym.
    *
    * The methods on `OMS` return [[cats.data.Kleisli]] arrows, which are intended to be chained with
    * `andThen`.
    *
    * TODO: Revisit [[cats.data.Kleisli]] usage
    *   - Why not [[fs2.Pipe]]?
    *   - Order processing *is* a natural pipeline, and so the Kleisli modeling has fidelity.
    */
  sealed abstract case class OMS private (
      final val host: LegalEntities.Key,
      final val markets: ExchangeSets.Key,
      final val meta: Metas.Id,
  ) {

    type F[_]

    /**
      */
    type ToStreamOf[T, R] = Kleisli[ResultT[Stream[F, *], *], T, R]

    /** What it is that the client wants [[Execution]] of.
      */
    sealed abstract case class Order private (
        final val at: Instant,
        final val market: Exchanges.Key,
        final val trade: Trades.Id,
        final val currency: CurrencyLike,
        final val limit: Option[MonetaryAmount],
        final val goodTill: Option[Instant]
    )

    /**
      */
    object Order {

      /** Note that currency is required even for market orders.
        */
      def market[C: Currency](market: Exchanges.Key, trade: Trades.Id): Order =
        Order(instant, market, trade, Currency[C], none, none)

      /**
        */
      def limit[C: Currency](market: Exchanges.Key, trade: Trades.Id, limit: Money[C]): Order =
        Order(instant, market, trade, Currency[C], limit.amount.some, none)

      private[OMS] def apply(
          at: Instant,
          market: Exchanges.Key,
          trade: Trades.Id,
          currency: CurrencyLike,
          limit: Option[MonetaryAmount],
          goodTill: Option[Instant]
      ): Order =
        new Order(at, market, trade, currency, limit, goodTill) {}

      implicit lazy val cpEq: Eq[Order]     = { import auto.eq._; semiauto.eq }
      implicit lazy val cpShow: Show[Order] = { import auto.show._; semiauto.show }
    }

    /**
      */
    case object Orders extends KeyValueStores.KV[Long OpaqueKey Order, Order]

    /** What actually happened to the [[Order]] at the [[Market]].
      *
      * FIXME: canceled order signals are TBD.
      */
    sealed abstract case class Execution private (
        final val at: Instant,
        final val order: Orders.Key,
        final val transaction: Transactions.Id
    )

    /** `Execution`s are pure values (do not evolve.)
      *
      * Implication for domain modellers: so-called "broken trades" require explicit modelling.
      */
    object Execution {

      private[OMS] def apply(
          at: Instant,
          order: Orders.Key,
          transaction: Transactions.Id
      ): Execution =
        new Execution(at, order, transaction) {}

      implicit lazy val cpEq: Eq[Execution]     = { import auto.eq._; semiauto.eq }
      implicit lazy val cpShow: Show[Execution] = { import auto.show._; semiauto.show }
    }

    case object Executions extends ValueStores.VS[Execution]

    def riskCheck: Order ToStreamOf Order

    def trade: Order ToStreamOf Execution

    /** FIXME: change api to
      * bind allocations at the beginning of processing. The order processing ppipe need know
      * nothing about the details of the account and the sub accounts.
      *
      * FIXME: implement (it's a system affordance)
      */
    final def allocate(a: Allocation): Execution ToStreamOf Execution = ???

    /** Settlement updates the actual [[Folio]]s,
      * with [[Account]] specific (and this `Folio` specific)
      * cash account [[capital.Instrument]]s substituted for the raw [[money.Currency]]
      * pseudo `Instrument` specified in the [[Order]] and enumerated within the [[Leg]]s
      * of the [[Trade]] specified in the [[Transaction]].
      *
      * FIXME: implement (it's a system affordance)
      */
    final def settle: Execution ToStreamOf Confirmation = ???

    /** Order processing is all just kleisli arrows? Always has been.
      */
    final def process(allocation: Allocation): Order ToStreamOf Confirmation =
      riskCheck andThen trade andThen allocate(allocation) andThen settle
  }

  /**
    */
  object OMS {

    // /** TODO: evole
    //   */
    private[deftrade] def apply(
        host: LegalEntities.Key,
        markets: ExchangeSets.Key,
        meta: Metas.Id,
    ): OMS =
      new OMS(
        host,
        markets,
        meta
      ) {

        /**
          */
        def riskCheck: Order ToStreamOf Order = ???

        /** This phase is implemented by the brokerage api integration code. */
        def trade: Order ToStreamOf Execution = ???
      }

    implicit lazy val omsOrder: Order[OMS] = { import auto.order._; semiauto.order }

    implicit lazy val omsShow: Show[OMS] = { import auto.show._; semiauto.show }
  }
}
