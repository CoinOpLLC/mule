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
package model.layers

import time._, money._, keyval._

import model.slices.{ Meta, Metas }

import cats.implicits._

import cats.kernel.{ Monoid }
import cats.{ Eq, Show }
import cats.data.NonEmptyMap
import cats.derived.{ auto, semiauto }
import cats.effect.{ Sync }

import eu.timepit.refined
import refined.cats._

import io.chrisdavenport.fuuid.FUUID

import fs2.Stream

/** Models the performance and recording of [[Trade]]s between [[Folio]]s as [[Transaction]]s.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait Ledger { module: ModuleTypes with Person with Paper =>

  case class Ledgers(
      trades: Trades.ValueStore[IO],
      folios: Folios.KeyValueStore[IO],
      portfolios: Portfolios.ValueStore[IO],
      transactions: Transactions.ValueStore[IO],
      confirmations: Confirmations.KeyValueStore[IO]
  )

  val ledgers: Ledgers
  import ledgers._

  val metas: Metas.ValueStore[IO]

  /**
    */
  sealed trait Pricer {

    /**
      */
    type Priced

    /**
      */
    type CurrencyTag

    /** Note that we are using `price` as a ''verb'' here.
      * TODO: consider how to memoize
      */
    val price: Priced => Stream[IO, Money[CurrencyTag]]

    /**
      */
    implicit val C: Currency[CurrencyTag]
  }

  /** TODO: The different kinds of `Pricer`, listed in order of abstraction (most to least),
    * Model / Market / Book / Cfg.
    */
  object Pricer {

    /**
      */
    abstract class Aux[P, C] protected (
        override val price: P => Stream[IO, Money[C]]
    )(implicit final override val C: Currency[C])
        extends Pricer {
      final type Priced      = P
      final type CurrencyTag = C
    }

    /** Price data per unit of instrument.
      */
    sealed abstract class Instrument[C: Currency] private (
        final override val price: Instruments.Key => Stream[IO, Money[C]]
    ) extends Pricer.Aux[Instruments.Key, C](price)

    /** Deploy `Pricer`s by creating search paths per `Currency` and instantiating
      * these pricers in the implicit context.
      */
    object Instrument {

      /** summon implicit value */
      def apply[C: Currency](implicit pricer: Instrument[C]): Instrument[C] =
        pricer

      /**
        */
      def apply[C: Currency](
          price: Instruments.Key => Stream[IO, Money[C]]
      ): Instrument[C] =
        new Instrument(price) {}

      /**
        */
      def instance[C: Currency](
          price: Instruments.Key => Stream[IO, Money[C]]
      ): Instrument[C] =
        Instrument(price)

      /**
        */
      def empty[C: Currency]: Instrument[C] =
        instance(_ => Stream.empty[IO])

      /** @return an `Instrument` which be functions as a two element search path
        * of `Pricer.Instrument`s.
        *
        * TODO: Looks like `Pricer.Instrument` is a [[cats.Monoid]]
        * ''not commutative'': price given by `a` (if given) has precedence.
        */
      def combine[C: Currency](
          a: Instrument[C],
          b: Instrument[C]
      ): Instrument[C] =
        instance { instrument =>
          (a price instrument) ++ (b price instrument) take 1
        }

      /**
        */
      implicit def instrumentPricerMonoid[C: Currency]: Monoid[Instrument[C]] =
        Monoid.instance(empty[C], combine[C])
    }

    /**
      */
    sealed abstract class Book[C: Currency] private (
        folios: Folios.KeyValueStore[IO],
        k: Folios.Key
    ) extends Pricer.Aux[Instruments.Key, C]((instrument: Instruments.Key) => {
          import money.Financial._

          // val positions: Stream[F, Position] = for {
          //   position <- folios selectAll k
          // } yield position

          Stream(3.14, 6.18).map(Currency[C] fiat _.to[MonetaryAmount]).covary[IO]
        }) {

      /** FIXME: need a method to pull the timetamps from the Transaction stream.
        */
      final def lots(instrument: Instruments.Key): Stream[IO, (Quantity, Instant)] = ???
      // folios.rows filter (_._1 === instrument) map (r => (r._2._1, ???))
    }

    object Book
  }

  /** This object serves as the root of a dependent type tree for `ledger entries`.
    */
  object Entry {

    /**
      */
    type Key = Instruments.Key

    /**
      */
    type Value = Quantity

    /** Enables volume discounts or other quantity-specific pricing. */
    sealed abstract case class Pricer[C: Currency] private (
        final override val price: Entry => Stream[IO, Money[C]]
    ) extends module.Pricer.Aux[Entry, C](price)

    /**
      */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[C: Currency](implicit pricer: Pricer[C]): Pricer[C] =
        pricer

      /**
        */
      def instance[C: Currency](price: Entry => Stream[IO, Money[C]]): Pricer[C] =
        new Pricer(price) {}

      /** Create a pricer from a pricing function. */
      implicit def default[C: Currency: module.Pricer.Instrument]: Pricer[C] =
        instance {
          case (instrument, quantity) =>
            module.Pricer.Instrument[C] price instrument map (_ * quantity)
        }
    }
  }

  /** Generic `ledger entry`.
    * Any kind of `ledger entry` must carry with it a `key`:
    * `value`s are '''fungeable''' ''within'' `key`s, but not ''across'' them.
    */
  final type Entry = (Entry.Key, Entry.Value)

  /** How much of a given [[Paper.Instrument]] is held.
    *
    * Can also be thought of as a [[Trade]] [[Leg]] at rest.
    *
    * Note, this is just a type alias.
    */
  final type Position = Entry

  /**
    */
  final lazy val Position = Entry

  /** [[Entry]] in motion.
    *
    * Note, this is just a type alias.
    */
  final type Leg = Entry

  /**
    */
  final lazy val Leg = Entry

  /** A set of (open) [[Position]]s.
    *
    * A `Folio` can be thought of as a "flat" portfolio",
    * i.e. a portfolio without sub portfolios.
    */
  final type Folio = Map[Entry.Key, Entry.Value]

  /** A `Folio` key value store holds (open) [[Trade]]s,
    * indexed by opaque [[Account]] identifiers.
    */
  object Folio {

    /** Conceptually, lifts all the [[Position]]s into `Map`s,
      * and sums them as the `Map`s form commutative groups.
      *
      * Implementation may differ for efficiency.
      */
    def apply(ps: Position*): Folio = ??? // indexAndSum(ps.toList)

    /**
      */
    def empty: Folio = Map.empty
  }

  /**
    */
  case object Folios extends KeyValueStores.MKV[FUUID, Position, Entry.Key, Entry.Value]

  /** Total view of [[Position]]s including those `escrowed` for transfer
    * and those `expected` to settle.
    *
    * This class is used by the settlement machine elves.
    */
  sealed abstract case class Portfolio private (
      final val open: Folios.Key,
      final val escrowed: Folios.Key,
      final val expected: Folios.Key
  )

  /**
    */
  object Portfolio {

    def apply(
        open: Folios.Key,
        escrowed: Folios.Key,
        expected: Folios.Key
    ): Portfolio =
      new Portfolio(open, escrowed, expected) {}

    implicit lazy val portfolioEq: Eq[Portfolio]     = { import auto.eq._; semiauto.eq }
    implicit lazy val portfolioShow: Show[Portfolio] = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object Portfolios extends ValueStores.VS[Portfolio]

  /** A [[Folio]] in motion, with the exception that unlike a `Folio`, a `Trade` cannot be empty.
    */
  final type Trade = NonEmptyMap[Entry.Key, Entry.Value]

  /**
    */
  object Trade {

    def apply(trade: Trade): Trade = trade

    /**
      */
    def apply(l: Leg, ls: Leg*): Trade = ??? // indexAndSum(l, ls: _*)

    /** Enables package deals, or portfolio valuation informed by covariance,
      * or other holistic methodology.
      */
    sealed abstract case class Pricer[C: Currency] private (
        final override val price: Trade => Stream[IO, Money[C]]
    ) extends module.Pricer.Aux[Trade, C](price)

    /**
      */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[C: Currency](implicit pricer: Pricer[C]): Pricer[C] =
        pricer

      /**
        */
      def instance[C: Currency](
          price: Trade => Stream[IO, Money[C]]
      ): Pricer[C] =
        new Pricer(price) {}

      /** Create a pricer from a pricing function. */
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      implicit def default[C: Currency: Leg.Pricer]: Pricer[C] =
        instance { trade =>
          val lp = Leg.Pricer[C]
          val prices = for {
            leg   <- Stream evals (Sync[IO] delay trade.toNel)
            price <- lp price leg
          } yield price
          prices foldMap identity
        }
    }

    /**
      */
    def of[C: Currency: Trade.Pricer](
        trade: Trade
    ): Stream[IO, (Trade, Money[C])] =
      for {
        amount <- Trade.Pricer[C] price trade
      } yield (trade, amount)
  }

  /** In contrast to a [[Folio]] `store`, [[Trade]] `store`s hold simple, ''immutable'' `value`s.
    *
    */
  case object Trades extends ValueStores.NEMKV[Leg, Entry.Key, Entry.Value]

  /** The concrete record for `Ledger` updates.
    *
    * Note: by exploiting the `content addressed` nature of the `store` and
    * recording the `Id` of an `SADT` instance, the [[Transaction]] record affords a
    * '''contemporaneous attestation''' of the data and its association with the transaction.
    *
    * This may be essential to the construction of certain smart [[io.deftrade.contracts contract]]
    * systems, especially
    * in conjunction with digital signatures. Thus the metadata is bound into the Ricardian
    * lineage of the contract.
    */
  sealed abstract case class Transaction private (
      at: Instant,
      folioA: Folios.Key,
      tradeA: Trades.Id,
      folioB: Folios.Key,
      tradeB: Trades.Id,
      meta: Metas.Id
  )

  /** Because `Transaction`s are immutable, we model them as pure values.
    */
  object Transaction {

    /**
      */
    def apply(
        at: Instant,
        folioA: Folios.Key,
        tradeA: Trades.Id,
        folioB: Folios.Key,
        tradeB: Trades.Id,
        meta: Metas.Id
    ): Transaction =
      new Transaction(at, folioA, tradeA, folioB, tradeB, meta) {}

    /**
      */
    def singleLeg(
        folioA: Folios.Key,
        tradeA: Trades.Id,
        folioB: Folios.Key,
        leg: Leg,
        meta: Meta
    ): IO[Transaction] =
      multiLeg(folioA, tradeA, folioB, Trade(leg), meta)

    /**
      */
    def multiLeg(
        folioA: Folios.Key,
        tradeA: Trades.Id,
        folioB: Folios.Key,
        trade: Trade,
        meta: Meta
    ): IO[Transaction] =
      for {
        tid <- trades put trade
        mid <- metas put meta map (_._1)
      } yield Transaction(instant, folioA, tradeA, folioB, tid._1, mid)

    implicit lazy val transactionEq: Eq[Transaction]     = { import auto.eq._; semiauto.eq }
    implicit lazy val transactionShow: Show[Transaction] = { import auto.show._; semiauto.show }
  }

  case object Transactions extends ValueStores.VS[Transaction]

  /** Note that [[Folios.Id]] is actually an `update event` for the specified [[Folios.Key]].
    * TODO: get this to work across nodes in a cluster
    */
  sealed abstract case class Confirmation private (at: Instant, from: Folios.Id, to: Folios.Id)

  /**
    */
  object Confirmation {

    /**
      */
    private[model] def apply(at: Instant, from: Folios.Id, to: Folios.Id): Confirmation =
      new Confirmation(at, from, to) {}

    implicit lazy val confirmationEq: Eq[Confirmation]     = { import auto.eq._; semiauto.eq }
    implicit lazy val confirmationShow: Show[Confirmation] = { import auto.show._; semiauto.show }

    /** Process which drives the exchange of [[Trade]]s between [[Folio]]s.
      *
      * FIXME: define and implement
      */
    trait Settlement

    /**
      */
    object Settlement

    /** For a given [[Transaction]], what part of the [[Trade]] remains unsettled?
      */
    def unsettled: Transactions.Id => Stream[IO, Confirmations.Id] => IO[Folio] =
      ???

    /** Monotonic, in that [[Transaction]]s are never observed to "unsettle".
      *
      * Nota Bene - '''memoize''' these if need be
      */
    def settled: Transactions.Id => Stream[IO, Confirmations.Id] => IO[Boolean] =
      xn => cs => unsettled(xn)(cs) map (_ === Folio.empty)
  }

  /**
    */
  case object Confirmations extends KeyValueStores.KV[Transactions.Id, Confirmation]

  /** '''Cash''' is ''fungable''and ''self-pricing''.
    */
  final def payment[C: Currency](
      payCash: Folios.Key => Money[C] => IO[Result[Folios.Id]]
  )(
      drawOn: Folios.Key
  ): (Trade, Money[C]) => IO[Result[Trades.Id]] = {
    case (trade, amount) =>
      for {
        idb <- trades put trade
        _   <- payCash(drawOn)(amount)
      } yield Result(idb._1.some)
  }
}
