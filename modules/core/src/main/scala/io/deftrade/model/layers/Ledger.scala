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

import time._, money._, keyval._, capital._

import cats.implicits._

import cats.kernel.{ Monoid }
import cats.{ Eq, Functor, Show }
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
trait Ledger { module: ModuleTypes =>

  /**
    */
  sealed trait Pricer {

    /**
      */
    type Priced

    /**
      */
    type CurrencyTag

    /**
      */
    type EffectType[_]

    /** Note that we are using `price` as a ''verb'' here.
      * TODO: consider how to memoize
      */
    val price: Priced => Stream[EffectType, Money[CurrencyTag]]

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
    abstract class Aux[F[_], P, C] protected (
        override val price: P => Stream[F, Money[C]]
    )(implicit final override val C: Currency[C])
        extends Pricer {
      final type Priced        = P
      final type CurrencyTag   = C
      final type EffectType[_] = F[_]
    }

    /** Price data per unit of instrument.
      */
    sealed abstract class Instrument[F[_], C: Currency] private (
        final override val price: Instruments.Key => Stream[F, Money[C]]
    ) extends Pricer.Aux[F, Instruments.Key, C](price)

    /** Deploy `Pricer`s by creating search paths per `Currency` and instantiating
      * these pricers in the implicit context.
      */
    object Instrument {

      /** summon implicit value */
      def apply[F[_], C: Currency](implicit pricer: Instrument[F, C]): Instrument[F, C] =
        pricer

      /**
        */
      def instance[F[_], C: Currency](
          price: Instruments.Key => Stream[F, Money[C]]
      ): Instrument[F, C] =
        new Instrument(price) {}

      /**
        */
      def empty[F[_], C: Currency]: Instrument[F, C] =
        instance(_ => Stream.empty[F])

      /** @return an `Instrument` which be functions as a two element search path
        * of `Pricer.Instrument`s.
        *
        * TODO: Looks like `Pricer.Instrument` is a [[cats.Monoid]]
        * ''not commutative'': price given by `a` (if given) has precedence.
        */
      def combine[F[_], C: Currency](
          a: Instrument[F, C],
          b: Instrument[F, C]
      ): Instrument[F, C] =
        instance { instrument =>
          (a price instrument) ++ (b price instrument) take 1
        }

      /**
        */
      implicit def instrumentPricerMonoid[F[_]: Sync, C: Currency]: Monoid[Instrument[F, C]] =
        Monoid.instance(empty[F, C], combine[F, C])
    }

    /**
      */
    sealed abstract class Book[F[_], C: Currency] private (
        folios: Folios.KeyValueStore[F],
        k: Folios.Key
    ) extends Pricer.Aux[F, Instruments.Key, C]((instrument: Instruments.Key) => {
          import money.Financial._

          // val positions: Stream[F, Position] = for {
          //   position <- folios selectAll k
          // } yield position

          Stream(3.14, 6.18).map(Currency[C] fiat _.to[MonetaryAmount]).covary[F]
        }) {

      /** FIXME: need a method to pull the timetamps from the Transaction stream.
        */
      final def lots(instrument: Instruments.Key): Stream[F, (Quantity, Instant)] = ???
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
    sealed abstract case class Pricer[F[_], C: Currency] private (
        final override val price: Entry => Stream[F, Money[C]]
    ) extends module.Pricer.Aux[F, Entry, C](price)

    /**
      */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[F[_], C: Currency](implicit pricer: Pricer[F, C]): Pricer[F, C] =
        pricer

      /**
        */
      def instance[F[_], C: Currency](price: Entry => Stream[F, Money[C]]): Pricer[F, C] =
        new Pricer(price) {}

      /** Create a pricer from a pricing function. */
      implicit def default[F[_]: Sync, C: Currency: module.Pricer.Instrument[F, *]]: Pricer[F, C] =
        instance {
          case (instrument, quantity) =>
            module.Pricer.Instrument[F, C] price instrument map (_ * quantity)
        }
    }
  }

  /** Generic `ledger entry`.
    * Any kind of `ledger entry` must carry with it a `key`:
    * `value`s are '''fungeable''' ''within'' `key`s, but not ''across'' them.
    */
  final type Entry = (Entry.Key, Entry.Value)

  /** How much of a given [[capital.Instrument]] is held.
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
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)

    /**
      */
    def empty: Folio = Map.empty
  }

  /**
    */
  object Folios extends KeyValueStores.MKV[FUUID, Position, Entry.Key, Entry.Value]

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
  object Portfolios extends ValueStores.VS[Portfolio]

  /** A [[Folio]] in motion, with the exception that unlike a `Folio`, a `Trade` cannot be empty.
    */
  final type Trade = NonEmptyMap[Entry.Key, Entry.Value]

  /**
    */
  object Trade {

    def apply(trade: Trade): Trade = trade

    /**
      */
    def apply(l: Leg, ls: Leg*): Trade = indexAndSum(l, ls: _*)

    /** Enables package deals, or portfolio valuation informed by covariance,
      * or other holistic methodology.
      */
    sealed abstract case class Pricer[F[_], C: Currency] private (
        final override val price: Trade => Stream[F, Money[C]]
    ) extends module.Pricer.Aux[F, Trade, C](price)

    /**
      */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[F[_], C: Currency](implicit pricer: Pricer[F, C]): Pricer[F, C] =
        pricer

      /**
        */
      def instance[F[_], C: Currency](
          price: Trade => Stream[F, Money[C]]
      ): Pricer[F, C] =
        new Pricer(price) {}

      /** Create a pricer from a pricing function. */
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      implicit def default[F[_]: Sync, C: Currency: Leg.Pricer[F, *]]: Pricer[F, C] =
        instance { trade =>
          val lp = Leg.Pricer[F, C]
          val prices = for {
            leg   <- Stream evals (Sync[F] delay trade.toNel)
            price <- lp price leg
          } yield price
          prices foldMap identity
        }
    }

    /**
      */
    def of[F[_]: Sync, C: Currency: Trade.Pricer[F, *]](
        trade: Trade
    ): Stream[F, (Trade, Money[C])] =
      for {
        amount <- Trade.Pricer[F, C] price trade
      } yield (trade, amount)
  }

  /** In contrast to a [[Folio]] `store`, [[Trade]] `store`s hold simple, ''immutable'' `value`s.
    *
    * Data Vault Classification:
    */
  object Trades extends ValueStores.NEMKV[Leg, Entry.Key, Entry.Value]

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
    def singleLeg[F[_]: Sync](
        folioA: Folios.Key,
        tradeA: Trades.Id,
        folioB: Folios.Key,
        leg: Leg,
        meta: Meta
    ): F[Transaction] =
      multiLeg(folioA, tradeA, folioB, Trade(leg), meta)

    /**
      */
    def multiLeg[F[_]: Sync](
        folioA: Folios.Key,
        tradeA: Trades.Id,
        folioB: Folios.Key,
        trade: Trade,
        meta: Meta
    ): F[Transaction] =
      for {
        tid <- trades[F] put trade
        mid <- metas[F] put meta map (_._1)
      } yield Transaction(instant, folioA, tradeA, folioB, tid._1, mid)

    implicit lazy val transactionEq: Eq[Transaction]     = { import auto.eq._; semiauto.eq }
    implicit lazy val transactionShow: Show[Transaction] = { import auto.show._; semiauto.show }
  }

  object Transactions extends ValueStores.VS[Transaction]

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
    def unsettled[F[_]]: Transactions.Id => Stream[F, Confirmations.Id] => F[Folio] =
      ???

    /** Monotonic, in that [[Transaction]]s are never observed to "unsettle".
      *
      * Nota Bene - '''memoize''' these if need be
      */
    def settled[F[_]: Functor]: Transactions.Id => Stream[F, Confirmations.Id] => F[Boolean] =
      xn => cs => unsettled(xn)(cs) map (_ === Folio.empty)
  }

  /**
    */
  object Confirmations extends KeyValueStores.KV[Transactions.Id, Confirmation]

  /** '''Cash''' is ''fungable''and ''self-pricing''.
    */
  final def payment[F[_]: Sync, C: Currency](
      payCash: Folios.Key => Money[C] => F[Result[Folios.Id]]
  )(
      drawOn: Folios.Key
  ): (Trade, Money[C]) => F[Result[Trades.Id]] = {
    case (trade, amount) =>
      for {
        idb <- trades[F] put trade
        _   <- payCash(drawOn)(amount)
      } yield Result(idb._1.some)
  }

  def trades[F[_]]: Trades.ValueStore[F]                  = ???
  def metas[F[_]]: Metas.ValueStore[F]                    = ???
  def transactions[F[_]]: Transactions.ValueStore[F]      = ???
  def confirmations[F[_]]: Confirmations.KeyValueStore[F] = ???
}
