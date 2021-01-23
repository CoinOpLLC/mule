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
import capital.Instruments
import refinements.{ Label }

import cats.implicits._
import cats.{ Monad, Order, Show }
import cats.derived.{ auto, semiauto }

import enumeratum.EnumEntry

import spire.syntax.field._

import eu.timepit.refined
import refined.api.Refined
import refined.boolean.{ And }
import refined.collection.{ Size }
import refined.numeric.{ GreaterEqual, LessEqual }
import refined.string.{ Trimmed }

import io.chrisdavenport.fuuid.FUUID

import refined.cats._

/**
  */
trait MarketData { self: Ledger with ModuleTypes =>

  /**
    */
  sealed abstract case class Quoted private[deftrade] (
      final val quote: (MonetaryAmount, MonetaryAmount)
  ) {

    type AssetType

    type CurrencyType // <: CurrencyLike

    /**
      */
    @inline final def bid: MonetaryAmount = quote match { case (bid, _) => bid }

    /**
      */
    @inline final def ask: MonetaryAmount = quote match { case (_, ask) => ask }

    /**
      */
    @inline final def spread = ask - bid

    /**
      */
    @inline final def mid = bid + spread / 2

    /**
      */
    def isDerived: Boolean = false
  }

  /**
    */
  object Quoted {

    /** exclusively usable by (and necessary for) macros and other lawless entities */
    protected def apply(quote: (MonetaryAmount, MonetaryAmount)): Quoted =
      new Quoted(quote) {}

    implicit def qOrder: Order[Quoted] = { import auto.order._; semiauto.order }
    implicit def qShow: Show[Quoted]   = { import auto.show._; semiauto.show }
  }

  /** Represents a price quote (in currency `C`) for instruments of type `A`.
    */
  sealed abstract class QuotedIn[A, C] private[deftrade] (
      q: (MonetaryAmount, MonetaryAmount)
  ) extends Quoted(q) {
    self =>

    final type AssetType    = A
    final type CurrencyType = C

    /**
      */
    def tick(implicit C: Currency[C]): MonetaryAmount =
      maFinancial from [BigDecimal] C.pip //  / 10 // this is a thing now

    /**
      */
    type CrossType

    /**
      */
    def cross: Option[CrossType] = None

    /** Inverse quotes are peculiar to `forex`.
      */
    final def inverse(implicit C: Currency[C]): C QuotedIn A =
      new QuotedIn[C, A]((1 / self.bid, 1 / self.ask)) {

        /** TODO this is not at all obvious, explain */
        override def tick(implicit A: Currency[A]) = (self tick C) * mid

        /**
          */
        override def isDerived = true
      }

    /** Cross quotes are also peculiar to `forex`.
      *
      * Our `A` is a `Currency`, and our `C: Currency` is the cross currency.
      */
    final def cross[C2: Currency](other: C QuotedIn C2)(implicit C: Currency[C]): A QuotedIn C2 =
      new QuotedIn[A, C2]((self.bid * other.bid, self.ask * other.ask)) {

        /**
          */
        override def tick(implicit C2: Currency[C2]) = (other tick C2) * mid

        /**
          */
        override def isDerived = true

        /**
          */
        override type CrossType = Currency[C]

        /**
          */
        override def cross: Option[CrossType] = Some(C)
      }
  }

  /**
    */
  object QuotedIn {

    def apply[A, C](quote: (MonetaryAmount, MonetaryAmount)): QuotedIn[A, C] =
      new QuotedIn[A, C](quote) {}

    /**
      */
    def bidask[A, C](
        bid: MonetaryAmount,
        ask: MonetaryAmount
    ): QuotedIn[A, C] =
      apply((bid, ask))

    /**
      */
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

    /**
      */
    @inline def buy(m1: Money[C1]): Money[C2] = convert(m1, ask)

    /**
      */
    @inline def sell(m1: Money[C1]): Money[C2] = convert(m1, bid)

    /**
      */
    @inline def apply(m1: Money[C1]): Money[C2] = convert(m1, mid)

    /**
      */
    def quote: (Money[C2], Money[C2]) = {
      val single = C1(maFinancial.one)
      (buy(single), sell(single))
    }

    /**
      */
    def description: String = s"""
          |Quoter buys  ${C1.toString} and sells ${C2.toString} at ${bid.toString}
          |Quoter sells ${C1.toString} and buys  ${C2.toString} at ${ask.toString}""".stripMargin

    /**
      */
    private def convert(m1: Money[C1], rate: MonetaryAmount): Money[C2] = C2(m1.amount * rate)
  }

  /**
    */
  object Rate {

    /**
      */
    def apply[C1: Currency, C2: Currency](qi: C1 QuotedIn C2): Rate[C1, C2] =
      new Rate(Currency[C1], Currency[C2], qi) {}
  }

  /**
    */
  implicit class CurrencyOps[C: Currency](C: Currency[C]) {

    /** FIXME: seems kinda useless unless it can use an implicit FStream[C QuotedIn C2]
      * Exchange `Rate` factory. Implicit context provides pricing.
      */
    def /[C2](c2: Currency[C2])(implicit Q: C QuotedIn C2): Rate[C, C2] = {
      implicit val C2 = c2
      Rate[C, C2](Q)
    }
  }

  /**
    */
  sealed trait Tick extends EnumEntry with Serializable

  /**
    */
  object Tick extends DtEnum[Tick] {

    case object Bid   extends Tick
    case object Ask   extends Tick
    case object Trade extends Tick

    /**
      */
    lazy val values = findValues
  }

  /**
    */
  sealed abstract case class TickData(
      at: Instant,
      tick: Tick,
      price: MonetaryAmount,
      size: Quantity
  )

  /**
    */
  object TickData {

    /**
      */
    def apply(at: Instant, tick: Tick, price: MonetaryAmount, size: Quantity): TickData =
      new TickData(at, tick, price, size) {}

    implicit def tdOrder: Order[TickData] = { import auto.order._; semiauto.order }
    implicit def tdShow: Show[TickData]   = { import auto.show._; semiauto.show }

    /** {{{
      * val myLimit: USD(55.47)
      * val td: TickData = 100.0 bid myLimit.amount
      * }}}
      *
      * TODO: separate syntax stuff
      */
    implicit class TickDataOps(size: Quantity) {

      /**
        */
      def bid(price: MonetaryAmount) = TickData(instant, Tick.Bid, price, size)

      /**
        */
      def ask(price: MonetaryAmount) = TickData(instant, Tick.Ask, price, size)

      /**
        */
      def trade(price: MonetaryAmount) = TickData(instant, Tick.Trade, price, size)
    }
  }

  /**
    */
  object TickDataSets extends KeyValueStores.KV[Label, TickData]

  /**
    */
  // type IsMIC = Size[GreaterEqual[3] And LessEqual[4]] And Trimmed
  type IsMIC = Size[GreaterEqual[3]] And Size[LessEqual[4]] And Trimmed
  // type IsMIC = Size[LessEqual[4]]

  /**
    */
  type MIC = String Refined IsMIC // market venue

  /** Public or private markets from which we obtain pricing information on [[capital.Instrument]]s.
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
  sealed trait Market { def host: Parties.Key; def contra: Folios.Key; def meta: Metas.Id }

  /** Markets evolve.
    */
  object Market {

    /** beware - naming here follows machine semantics not domain semantics!!!
      *
      * Invariant: Markets are assigned unique `contra` Folios.
      */
    implicit def marketOrder: Order[Market] = Order by (_.contra)
  }

  object Markets

  /** Models a private party whose [[Ledger.Folio]]s we run [[Ledger.Transaction]]s against.
    *
    * The `Counterparty` is assumed to have an [[Accounts.Account]] whose [[Ledger.Folio]]s are
    * recorded on on the [[Ledger]].
    */
  sealed abstract case class Counterparty private (
      final val host: NaturalPersons.Key,
      final val contra: Folios.Key,
      final val meta: Metas.Id
  ) extends Market

  /**
    */
  object Counterparty {

    def apply(host: NaturalPersons.Key, contra: Folios.Key, meta: Metas.Id): Counterparty =
      new Counterparty(host, contra, meta) {}

    def mk[F[_]](host: Parties.Key, meta: Meta): F[Counterparty] = ???

    implicit def cpShow: Show[Counterparty] = { import auto.show._; semiauto.show }
  }

  object Counterparties extends KeyValueStores.KV[FUUID, Counterparty]

  /** Single effective counterparty: the `Exchange` itself.
    *   - [[MIC]]s are unique.
    *   - seller for all buyers and vice versa.
    *   - activity recorded in a `contra account`
    */
  sealed abstract case class Exchange private (
      final val host: LegalEntities.Key,
      final val contra: Folios.Key,
      final val meta: Metas.Id
  ) extends Market

  /**
    */
  object Exchange {

    /**
      */
    protected[deftrade] def apply(
        host: LegalEntities.Key,
        contra: Folios.Key,
        meta: Metas.Id
    ): Exchange =
      new Exchange(host, contra, meta) {}

    /**
      */
    def withEntity(host: Parties.Key): Exchange => Exchange =
      x => Exchange(host, x.contra, x.meta)

    implicit def exShow: Show[Exchange] = { import auto.show._; semiauto.show }
  }

  /**
    */
  object Exchanges extends KeyValueStores.KV[MIC, Exchange]

  /**
    */
  object ExchangeSets extends KeyValueStores.KV[Label, Exchanges.Key]

  /** MDS := Market Data Source.
    */
  sealed abstract case class MDS private (
      provider: Parties.Key,
      markets: ExchangeSets.Key,
      meta: Metas.Id
  ) {

    /** `Currency`-specific quote factory. */
    def quotedIn[C: Currency](ik: Instruments.Key): Instruments.Key QuotedIn C

    /**
      */
    final def quote[F[_]: Monad, C: Currency](ik: Instruments.Key): F[Money[C]] =
      Monad[F] pure (Currency[C] apply quotedIn(ik).mid)

    /**
      */
    final def quoteLeg[F[_]: Monad, C: Currency](leg: Leg): F[Money[C]] =
      leg match {
        case (security, quantity) => quote[F, C](security) map (_ * quantity)
      }

    /** Simple per-leg pricing. Note, this method is `override`-able.
      *
      * TODO: this is so minimal as to be of questionable viablity... but is correct
      */
    def quoteTrade[F[_]: Monad, C: Currency](trade: Trade): F[Money[C]] =
      trade.toNel foldMapM quoteLeg[F, C]
  }

  /**
    */
  object MDS {

    private[deftrade] def apply(
        provider: Parties.Key,
        markets: ExchangeSets.Key,
        meta: Metas.Id,
    ): MDS =
      new MDS(provider, markets, meta) {

        /** FIXME: how do we specialize? */
        def quotedIn[C: Currency](ik: Instruments.Key): Instruments.Key QuotedIn C = ???
      }
  }

  /**
    */
  object MDSs extends KeyValueStores.KV[Label, MDS]

  /** placeholder */
  trait OrderBook

  /** placeholder */
  object OrderBook
}
