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

import money._, keyval._, time._
import capital.Instrument
import refinements.{ IsLabel, Label }

import cats.implicits._
import cats.{ Monad, Order, Show }
import cats.data.NonEmptySet
import cats.derived.{ auto, semi }

import enumeratum.EnumEntry

import spire.syntax.field._

import eu.timepit.refined
import refined.refineMV
import refined.api.Refined
import refined.boolean.{ And }
import refined.collection.{ Forall, Size }
import refined.numeric.{ GreaterEqual, LessEqual }
import refined.string.{ Trimmed, Uuid => IsUuid }
import refined.char.{ Letter, UpperCase }

import refined.cats._

/** */
trait MarketData { self: Ledger with ModuleTypes =>

  /** */
  sealed abstract case class Quoted private[deftrade] (
      final val quote: (MonetaryAmount, MonetaryAmount)
  ) {

    type AssetType

    type CurrencyType // <: CurrencyLike

    /** */
    @inline final def bid: MonetaryAmount = quote match { case (bid, _) => bid }

    /** */
    @inline final def ask: MonetaryAmount = quote match { case (_, ask) => ask }

    /** */
    @inline final def spread = ask - bid

    /** */
    @inline final def mid = bid + spread / 2

    /** */
    def isDerived: Boolean = false
  }

  /** */
  object Quoted {

    /** exclusively usable by (and necessary for) macros and other lawless entities */
    protected def apply(quote: (MonetaryAmount, MonetaryAmount)): Quoted =
      new Quoted(quote) {}

    implicit def qOrder: Order[Quoted] = { import auto.order._; semi.order }
    implicit def qShow: Show[Quoted]   = { import auto.show._; semi.show }
  }

  /**
    * Represents a price quote (in currency `C`) for instruments of type `A`.
    *
    * The two parameter type constructor takes advantage of the infix syntax; `A QuotedIn B` is
    * a human-legible expression in the domain of market quotes.
    *
    * Return both a `bid` and an `ask` for the given instrument.
    *
    * Domain modelling note: `quote` does not signal intent. For example, the client may ignore
    * the returned ask and just hit the bid (if selling). Servers of this api (e.g. stockbrokers)
    * cannot not predjudice their responses when asked for a quote, as the client reveals nothing
    * about their intent.

    * Instances can come from a variety of sources including live market feeds
    *   - "Orderly market" invariant: `ask` < `bid`
    *   - must model disorderly markets: not everything that comes at you down the wire
    *     can be expected to "make sense"
    *
    * Domain consideration: `Currency` exchange depends on '''pricing''' of some kind, and one or
    * more Market(s) determine this price.
    */
  sealed abstract class QuotedIn[A, C] private[deftrade] (
      q: (MonetaryAmount, MonetaryAmount)
  ) extends Quoted(q) {
    self =>

    final type AssetType    = A
    final type CurrencyType = C

    /** */
    def tick(implicit C: Currency[C]): MonetaryAmount =
      MonetaryAmount from [BigDecimal] C.pip //  / 10 // this is a thing now

    /** */
    type CrossType

    /** */
    def cross: Option[CrossType] = None

    /**
      * Inverse quotes are peculiar to `forex`.
      */
    final def inverse(implicit C: Currency[C]): C QuotedIn A =
      new QuotedIn[C, A]((1 / self.bid, 1 / self.ask)) {

        /** TODO this is not at all obvious, explain */
        override def tick(implicit A: Currency[A]) = (self tick C) * mid

        /** */
        override def isDerived = true
      }

    /**
      * Cross quotes are also peculiar to `forex`.
      *
      * Our `A` is a `Currency`, and our `C: Currency` is the cross currency.
      */
    final def cross[C2: Currency](other: C QuotedIn C2)(implicit C: Currency[C]): A QuotedIn C2 =
      new QuotedIn[A, C2]((self.bid * other.bid, self.ask * other.ask)) {

        /** */
        override def tick(implicit C2: Currency[C2]) = (other tick C2) * mid

        /** */
        override def isDerived = true

        /** */
        override type CrossType = Currency[C]

        /** */
        override def cross: Option[CrossType] = Some(C)
      }
  }

  /** */
  object QuotedIn {

    def apply[A, C](quote: (MonetaryAmount, MonetaryAmount)): QuotedIn[A, C] =
      new QuotedIn[A, C](quote) {}

    /** */
    def bidask[A, C](
        bid: MonetaryAmount,
        ask: MonetaryAmount
    ): QuotedIn[A, C] =
      apply((bid, ask))

    /** */
    def trade[A, C: Currency](amount: MonetaryAmount): QuotedIn[A, C] =
      apply((amount, amount))
  }

  // FIXME: QuotedIn is not at all what we want here - a Stream[F, _] something
  /** An exchange rate. */
  sealed abstract case class Rate[C1, C2](
      C1: Currency[C1],
      C2: Currency[C2],
      Q: C1 QuotedIn C2
  ) {

    import Q._

    /** */
    @inline def buy(m1: Money[C1]): Money[C2] = convert(m1, ask)

    /** */
    @inline def sell(m1: Money[C1]): Money[C2] = convert(m1, bid)

    /** */
    @inline def apply(m1: Money[C1]): Money[C2] = convert(m1, mid)

    /** */
    def quote: (Money[C2], Money[C2]) = {
      val single = C1(MonetaryAmount.one)
      (buy(single), sell(single))
    }

    /** */
    def description: String = s"""
          |Quoter buys  ${C1.toString} and sells ${C2.toString} at ${bid.toString}
          |Quoter sells ${C1.toString} and buys  ${C2.toString} at ${ask.toString}""".stripMargin

    /** */
    private def convert(m1: Money[C1], rate: MonetaryAmount): Money[C2] = C2(m1.amount * rate)
  }

  /** */
  object Rate {

    /** */
    def apply[C1: Currency, C2: Currency](qi: C1 QuotedIn C2): Rate[C1, C2] =
      new Rate(Currency[C1], Currency[C2], qi) {}
  }

  /** */
  implicit class CurrencyOps[C: Currency](C: Currency[C]) {

    /** FIXME: seems kinda useless unless it can use an implicit FStream[C QuotedIn C2]
      * Exchange `Rate` factory. Implicit context provides pricing.
      */
    def /[C2](c2: Currency[C2])(implicit Q: C QuotedIn C2): Rate[C, C2] = {
      implicit val C2 = c2
      Rate[C, C2](Q)
    }
  }

  /** */
  sealed trait Tick extends EnumEntry with Serializable

  /** */
  object Tick extends DtEnum[Tick] {

    case object Bid   extends Tick
    case object Ask   extends Tick
    case object Trade extends Tick

    /** */
    lazy val values = findValues
  }

  /** */
  sealed abstract case class TickData(
      at: Instant,
      tick: Tick,
      price: MonetaryAmount,
      size: Quantity
  )

  /** */
  object TickData extends WithId[TickData] {

    /** */
    def apply(at: Instant, tick: Tick, price: MonetaryAmount, size: Quantity): TickData =
      new TickData(at, tick, price, size) {}

    implicit def tdOrder: Order[TickData] = { import auto.order._; semi.order }
    implicit def tdShow: Show[TickData]   = { import auto.show._; semi.show }

    /**
      * {{{
      * val myLimit: USD(55.47)
      * val td: TickData = 100.0 bid myLimit.amount
      * }}}
      *
      * TODO: separate syntax stuff
      */
    implicit class TickDataOps(size: Quantity) {

      /** */
      def bid(price: MonetaryAmount) = TickData(instant, Tick.Bid, price, size)

      /** */
      def ask(price: MonetaryAmount) = TickData(instant, Tick.Ask, price, size)

      /** */
      def trade(price: MonetaryAmount) = TickData(instant, Tick.Trade, price, size)
    }
  }

  /** */
  // type IsMic = Size[GreaterEqual[3] And LessEqual[4]] And Trimmed
  type IsMic = Size[GreaterEqual[3]] And Size[LessEqual[4]] And Trimmed
  // type IsMic = Size[LessEqual[4]]

  /** */
  type Mic = String Refined IsMic // market venue

  /**
    * Public or private markets from which we obtain pricing information on [[capital.Instrument]]s.
    *
    * Note: "double entry bookkeeping" in the context of a shared [[Ledger]] with
    * multiple [[OrderManagement.OMS OMS gateways]] to externally traded `Instrument`s
    * entails the following:
    *
    *   - keep contra accounts per `OMS` gateway
    *   - debit contra account when we "buy shares (units)" (creates negative balance)
    *   - credit that account when settlement happens (zeros out the balance)
    *   - "reverse polarity" when we enter a short position.
    *   - indexAndSum settled positions for reconcilliation
    */
  sealed trait Market { def entity: Party.Key; def contra: Folio.Key }

  /** Since its members are evolvable entities, `Market`s may be modelled as immutable values. */
  object Market extends WithRefinedKey[String, IsLabel, Market] {

    /** beware this is below domain semantics and not what it says */
    implicit def marketOrder: Order[Market] = ??? // { import auto.order._; semi.order }
  }

  /**
    * Models a private party whose [[Ledger.Folio]]s we run [[Ledger.Transaction]]s against.
    *
    * The `Counterparty` is assumed to have an [[Accounts.Account]] whose [[Ledger.Folio]]s are
    * recorded on on the [[Ledger]].
    */
  sealed abstract case class Counterparty(
      final val entity: Party.Key,
      final val contra: Folio.Key,
  ) extends Market

  /** */
  object Counterparty extends WithRefinedKey[String, IsUuid, Counterparty] {

    def apply(entity: Party.Key, contra: Folio.Key): Counterparty =
      new Counterparty(entity, contra) {}

    implicit def cpOrder: Order[Counterparty] = { import auto.order._; semi.order }

    implicit def cpShow: Show[Counterparty] = { import auto.show._; semi.show }
  }

  /**
    * Single effective counterparty: the `Exchange` itself.
    *   - [[Mic]]s are unique.
    *   - seller for all buyers and vice versa.
    *   - activity recorded in a `contra account`
    */
  sealed abstract case class Exchange private (
      final val entity: Party.Key,
      final val contra: Folio.Key,
  ) extends Market

  /** */
  object Exchange extends WithRefinedKey[String, IsMic, Exchange] {

    /** */
    protected[deftrade] def apply(entity: Party.Key, contra: Folio.Key): Exchange =
      new Exchange(entity, contra) {}

    /** */
    def withEntity(entity: Party.Key): Exchange => Exchange =
      x => Exchange(entity, x.contra)
  }

  /**
    * MDS := Market Data Source.
    */
  sealed abstract case class MDS(markets: NonEmptySet[Market]) {

    /** `Currency`-specific quote factory. */
    def quotedIn[C: Currency](ik: Instrument.Key): Instrument.Key QuotedIn C

    /** */
    final def quote[F[_]: Monad, C: Currency](ik: Instrument.Key): F[Money[C]] =
      Monad[F] pure (Currency[C] apply quotedIn(ik).mid)

    /** */
    final def quoteLeg[F[_]: Monad, C: Currency](leg: Leg): F[Money[C]] =
      leg match {
        case (security, quantity) => quote[F, C](security) map (_ * quantity)
      }

    /**
      * Simple per-leg pricing. Note, this method is `override`-able.
      *
      * TODO: this is so minimal as to be of questionable viablity... but is correct
      */
    def quoteTrade[F[_]: Monad, C: Currency](trade: Trade): F[Money[C]] =
      trade.toList foldMapM quoteLeg[F, C]
  }

  /** FIXME: normalize fields? */
  object MDS extends WithRefinedKey[String, IsLabel, MDS] {

    private[deftrade] def apply(markets: NonEmptySet[Market]): MDS = new MDS(markets) {

      /** FIXME: how do we specialize? */
      def quotedIn[C: Currency](ik: Instrument.Key): Instrument.Key QuotedIn C = ???
    }

    /** */
    def single(m: Market) = MDS(NonEmptySet one m)
  }

  /** placeholder */
  trait OrderBook

  /** placeholder */
  object OrderBook

}
