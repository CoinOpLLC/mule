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

import money._, keyval._
import capital.Instrument, reference.Mic

import cats.implicits._
import cats.Monad

import enumeratum.EnumEntry

import spire.syntax.field._

import eu.timepit.refined
import refined.auto._

import scala.language.higherKinds

/** */
trait MarketData { self: Ledger with ModuleTypes =>

  /**
    * Represents a price quote (in currency `C`) for instruments of type `A`.
    *
    * The two parameter type constructor takes advantage of the infix syntax; `A QuotedIn B` is
    * a human-legible expression in the domain of market quotes.
    *
    *   Instances can come from a variety of sources including live market
    *   - "Orderly market" invariant: `ask` < `bid`.
    *   - must model disorderly markets: not everything that comes at you down the wire makes sense.
    *   - used summon a pricing instance, e.g.
    *{{{val q = QuotedIn[SomeShadySpeculativeInstrument, Currency.CHF] }}}
    *       - C is intended (but not required by this base type) to represent a currency, and will
    * typically have a [[money.Currency]][C] typeclass instance
    *
    * Domain consideration: `Currency`_exchange depends on '''pricing''' of some kind.
    * One or more Market(s) determine this price.
    *     - A design that doesn't abstract over `QuotedIn`, '''including live data''', is useless.
    *     - OTOH, dead simple immutable for testing / demo also required
    */
  trait QuotedIn[A, C] extends Any {

    /** */
    final def bid: MonetaryAmount = quote match { case (bid, _) => bid }

    /** */
    final def ask: MonetaryAmount = quote match { case (_, ask) => ask }

    /**
      *
      * Return both a `bid` and an `ask` for the given instrument.
      *
      * Implementations which query live markets, but do not require live data,
      * should strongly consider caching.
      *
      * Domain modelling note: `quote` does not signal intent. For example, the client may ignore
      * the returned ask and just hit the bid (if selling). Servers of this api (e.g. stockbrokers)
      * cannot not predjudice their responses when asked for a quote, as the client reveals nothing
      * about their intent.
      */
    def quote: (MonetaryAmount, MonetaryAmount)

    /** */
    def tick(implicit C: Currency[C]): MonetaryAmount

    /** */
    def isDerived: Boolean = false

    /** */
    type CrossType

    /** */
    def cross: Option[CrossType] = None

    /** */
    @inline final def spread = ask - bid

    /** */
    @inline final def mid = bid + spread / 2
  }

  /**
    * TODO: implement
    */
  object QuotedIn {

    /**
      * Securities of any kind.
      */
    def apply[C: Currency]: QuotedIn[reference.Usin, C] = ???

    /**
      * Forex.
      */
    def apply[C1: Currency, C2: Currency]: QuotedIn[C1, C2] = ???

    /**
      * Inverse quotes are particular to Forex.
      */
    implicit def inverseQuote[C1: Currency, C2: Currency](
        implicit Q: C1 QuotedIn C2
    ): C2 QuotedIn C1 =
      new QuotedIn[C2, C1] {

        /** */
        def quote = (1 / Q.bid, 1 / Q.ask)

        /** */
        def tick(implicit C1: Currency[C1]) = Q.tick * mid

        /** */
        override def isDerived = true
      }

    /**
      * Cross quotes are particular to Forex.
      */
    implicit def crossQuote[C1: Currency, CX: Currency, C2: Currency](
        Q1X: C1 QuotedIn CX,
        QX2: CX QuotedIn C2
    ): C1 QuotedIn C2 =
      new QuotedIn[C1, C2] {

        /** */
        def quote = (Q1X.bid * QX2.bid, Q1X.ask * QX2.ask)

        /** */
        def tick(implicit C2: Currency[C2]) = QX2.tick(C2) * mid

        /** */
        override def isDerived = true

        /** */
        type CrossType = Currency[CX]

        /** */
        override def cross: Option[CrossType] = Some(Currency[CX])
      }
  }

  /**
    * Immutable value class representing a quote.
    *
    * TODO: reconsider the subtle name.
    */
  sealed abstract case class QuoteIn[A, C] private (
      final val quote: (MonetaryAmount, MonetaryAmount)
  ) extends QuotedIn[A, C] {

    /** */
    def tick(implicit C: Currency[C]): MonetaryAmount =
      MonetaryAmount from [BigDecimal] C.pip //  / 10 // this is a thing now
  }

  /** */
  object QuoteIn {

    /** */
    def apply[A, C2: Currency](
        bid: MonetaryAmount,
        ask: MonetaryAmount
    ): QuoteIn[A, C2] = new QuoteIn[A, C2]((bid, ask)) {}

    /** */
    def asTraded[A, C2: Currency](
        trade: MonetaryAmount
    ): QuoteIn[A, C2] = apply(trade, trade)
  }

  /** An exchange rate. */
  sealed abstract case class Rate[C1, C2]()(implicit
                                            C1: Currency[C1],
                                            C2: Currency[C2],
                                            Q: C1 QuotedIn C2) {
    import Q._

    /** */
    @inline def buy(m1: Mny[C1]): Mny[C2] = convert(m1, ask)

    /** */
    @inline def sell(m1: Mny[C1]): Mny[C2] = convert(m1, bid)

    /** */
    @inline def apply(m1: Mny[C1]): Mny[C2] = convert(m1, mid)

    /** */
    def quote: (Mny[C2], Mny[C2]) = {
      val single = C1(MonetaryAmount.one)
      (buy(single), sell(single))
    }

    /** */
    def description: String = s"""
          |Quoter buys  ${C1} and sells ${C2} at ${bid}
          |Quoter sells ${C1} and buys  ${C2} at ${ask}""".stripMargin

    /** */
    private def convert(m1: Mny[C1], rate: MonetaryAmount): Mny[C2] = C2(m1.amount * rate)
  }

  /** */
  object Rate {

    /** */
    def apply[C1: Currency, C2: Currency](implicit Q: C1 QuotedIn C2): Rate[C1, C2] =
      new Rate[C1, C2] {}
  }

  /** */
  implicit class CurrencyOps[C: Currency](C: C) {

    /**
      * Exchange `Rate` factory. Implicit context provides pricing.
      */
    def /[C2](cb: Currency[C2])(implicit Q: C QuotedIn C2): Rate[C, C2] = {
      implicit val C2 = cb
      Rate[C, C2]
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

  sealed abstract case class TickData(tick: Tick, price: MonetaryAmount, size: Long)

  object TickData {

    /** */
    def apply(tick: Tick, price: MonetaryAmount, size: Long): TickData =
      new TickData(tick, price, size) {}

    /** */
    def bid(price: MonetaryAmount, size: Long) = TickData(Tick.Bid, price, size)

    /** */
    def ask(price: MonetaryAmount, size: Long) = TickData(Tick.Ask, price, size)

    /** */
    def trade(price: MonetaryAmount, size: Long) = TickData(Tick.Trade, price, size)
  }

  /**
    * Public or private markets from which we obtain pricing information on [[capital.Instrument]]s.
    *
    * Note: "double entry bookkeeping" in the context of a shared [[Ledger]] with
    * multiple [[OrderManagement.OMS]] entails the following:
    *
    *   - keep contra accounts per OMS gateway
    *   - debit contra account when we "buy shares (units)" (creates negative balance)
    *   - credit that account when settlement happens (zeros out the balance)
    *   - "reverse polarity" when we enter a short position.
    *   - indexAndSum settled positions for reconcilliation
    */
  sealed trait Market {
    def entity: LegalEntity.Key
    def contra: Folio.Key
  }

  /** */
  object Market extends WithOpaqueKey[Long, Market]

  /**
    * Models a private party whose [[Ledger.Folio]]s we run [[Ledger.Transaction]]s against.
    *
    * The `Counterparty` is assumed to have an [[Accounts.Account]] whose [[Ledger.Folio]]s are
    * recorded on on the [[Ledger]].
    */
  sealed abstract case class Counterparty(
      final val label: Label,
      final val contra: Folio.Key,
      final val entity: LegalEntity.Key
  ) extends Market

  /** */
  object Counterparty {}

  /**
    * Single effective counterparty: the `Exchange` itself.
    *   - [[reference.Mic]]s are unique.
    *   - seller for all buyers and vice versa.
    *   - activity recorded in a `contra account`
    */
  sealed abstract case class Exchange private (
      final val mic: Mic,
      final val contra: Folio.Key,
      final val entity: LegalEntity.Key
  ) extends Market

  /** */
  object Exchange {

    /** */
    def fromMic(mic: Mic): Exchange = ??? // make new contra account

    /** */
    def withEntity(entity: LegalEntity.Key): Exchange => Exchange =
      x => new Exchange(x.mic, x.contra, entity) {}
  }

  /**
    * MDS := Market Data Source.
    */
  sealed abstract case class MDS(market: Market) {

    /** `Currency`-specific quote factory. */
    def quotedIn[C: Currency](ik: Instrument.Key): Instrument.Key QuotedIn C

    /** */
    final def quote[F[_]: Monad, C: Currency](ik: Instrument.Key): F[Mny[C]] =
      Monad[F] pure (Currency[C] fiat quotedIn(ik).mid)

    /** */
    final def quoteLeg[F[_]: Monad, C: Currency](leg: Leg): F[Mny[C]] =
      leg match {
        case (security, quantity) => quote[F, C](security) map (_ * quantity)
      }

    /**
      * Simple per-leg pricing. Note, this method is `override`-able.
      *
      * TODO: this is so minimal as to be of questionable viablity... but is correct
      */
    def quoteTrade[F[_]: Monad, C: Currency](trade: Trade): F[Mny[C]] =
      trade.toList foldMapM quoteLeg[F, C]
  }

  /** */
  object MDS extends WithOpaqueKey[Long, MDS] {

    /** */
    def apply(m: Market): MDS = new MDS(m) {

      /** FIXME: how do we specialize? */
      def quotedIn[C: Currency](ik: Instrument.Key): Instrument.Key QuotedIn C = ???
    }
  }
}
