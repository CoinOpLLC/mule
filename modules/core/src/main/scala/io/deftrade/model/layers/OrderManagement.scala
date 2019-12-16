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

import keyval._, time._, money._, capital._

import cats.implicits._
import cats.{ Monad }
import cats.data.{ Kleisli, NonEmptySet }
import cats.effect.IO

import eu.timepit.refined
import refined.cats.refTypeOrder
import refined.auto._

import fs2.Stream

import scala.collection.immutable.SortedSet
import scala.language.higherKinds

/** */
trait OrderManagement { self: MarketData with Ledger with ModuleTypes =>

  /** FIXME placeholder */
  type FolioTable = Map[Folio.Key, Folio.Value]

  /**
    *`OMS` := Order Management System. Ubiquitous domain acronym.
    *
    * The methods on `OMS` return [[cats.data.Kleisli]] arrows, which are intended to be chained with
    * `andThen`.
    *
    * Reference:
    * [[https://livebook.manning.com/#!/book/functional-and-reactive-domain-modeling/chapter-4/270 Functional and Reactive Domain Modelling, section 4.4]]
    *
    * TODO: Revisit [[cats.data.Kleisli]] usage
    *   - Why not [[fs2.Pipe]]?
    *   - Order processing *is* a natural pipeline, and so the Kleisli modeling has fidelity.
    */
  sealed abstract case class OMS[F[_]: Monad] private (
      entity: LegalEntity.Key,
      entry: Folio.Key,
      contra: Folio.Key,
      markets: NonEmptySet[Market.Key],
      folios: FolioTable
  ) {

    /**
      * `Folio` is a `Map` of `Map`s, which, normalized and written out as a list,
      * has rows of type: {{{
      *   (Folio.Key, Instrument.Key, Quantity)
      * }}}
      */
    /** */
    type EffectStream[A] = Stream[F, A]

    /** */
    type Phase[T, R] = Kleisli[ResultT[EffectStream, *], T, R]

    /** WIP Composing Contracts from Peyton Jones et al */
    sealed trait Contract {

      /** */
      def trade: Trade.Id = ???

      /** */
      def and(c: Contract): Contract = ???

      /** */
      def or(c: Contract): Contract = ???

      /** */
      def truncate(zdt: ZonedDateTime): Contract = ???

      /** */
      def elseThen(c: Contract): Contract = ???

      /** */
      def scale(q: Quantity): Contract = ???
    }

    /** */
    object Contract {

      def apply(leg: Leg) = leg match {
        case (k, x) => Contract one k scale x
      }

      /**  */
      def zeroCouponInstrument(t: ZonedDateTime, x: Quantity, k: Instrument.Key) =
        Contract(k -> x) truncate t

      /** */
      def zero: Contract = ???

      /** */
      def one(k: Instrument.Key): Contract = ??? // Contract(instrument)

      /** */
      def give(c: Contract): Contract = ??? // flip sign of Quantity

      /** */
      def get(c: Contract): Contract = ???

      /** */
      def anytime(c: Contract): Contract = ???

      /** FIXME: Obs != Stream in subtle ways */
      type Obs[A] = Stream[IO, A]

      object Obs {
        def konst[A](a: A): Obs[A] = Stream eval (IO delay a)
      }

      def time(zdt: ZonedDateTime): Obs[Period] = ???

      def wsjPrimeRate(date: LocalDate): Obs[Period] = ???
    }

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

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def process[C: Currency, A](
        allocation: Allocation
    )(
        block: => A
    ): Phase[A, Unit] =
      riskCheck[C, A](block) andThen
        trade andThen
        allocate(allocation) andThen
        settle(folios)(entry)

    /** */
    def riskCheck[C: Currency, A](a: A): Phase[A, Order]

    /**  */
    def trade: Phase[Order, Execution]

    /** */
    def allocate(a: Allocation): Phase[Execution, Execution]

    /** */
    def settle(
        folios: FolioTable
    )(
        p: Folio.Key
    ): Phase[Execution, Unit]
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
        entry: Folio.Key,
        contra: Folio.Key,
        folios: FolioTable,
        market: Market.Key,
        ms: Market.Key*
    ): OMS[F] =
      new OMS[F](key, entry, contra, NonEmptySet(market, SortedSet(ms: _*)), folios) {

        /** */
        def riskCheck[C: Currency, A](a: A): Phase[A, Order] = ???

        /** This phase is implemented by the brokerage api integration code - IBRK is first. */
        def trade: Phase[Order, Execution] = ???

        /** */
        def allocate(a: Allocation): Phase[Execution, Execution] = ???

        /** */
        def settle(
            folios: FolioTable
        )(
            p: Folio.Key
        ): Phase[Execution, Unit] = ???

      }
  }
}
/*
 * example "blotter" gathered randomly from the web - for reference only
 * there is a lot wrong with this example, actually, and it won't translate directly or uniquely
 *
 * Client name
 * Trade name
 * Settlement Date
 * Buy/Sell
 * CUSIP
 * SecuritySymbol
 * SecurityDesc.
 * Quantity
 * UnitPrice
 * Principal/Proceeds
 * TotalCommission
 * Fees
 * Net Proceeds
 * Broker
 */
