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
import cats.{ Eq, Order, Show }
import cats.data.NonEmptyMap
import cats.derived.{ auto, semi }
import cats.effect.{ Sync }

import eu.timepit.refined
import refined.cats._

import fs2.{ Pipe, Stream }
import io.deftrade.keyval.WithKey.KeyCompanion
import io.getquill.ast.Value

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
    *
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
        final override val price: capital.Instrument.Key => Stream[F, Money[C]]
    ) extends Pricer.Aux[F, capital.Instrument.Key, C](price)

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
          price: capital.Instrument.Key => Stream[F, Money[C]]
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
        folios: Folio.Store[F],
        k: Folio.Key
    ) extends Pricer.Aux[F, capital.Instrument.Key, C]((instrument: capital.Instrument.Key) => {
          import money.Financial._

          // val positions: Stream[F, Position] = for {
          //   position <- folios selectAll k
          // } yield position

          Stream(3.14, 6.18).map(Currency[C] fiat _.to[MonetaryAmount]).covary[F]
        }) {

      /** FIXME: need a method to pull the timetamps from the Transaction stream.
        */
      final def lots(instrument: capital.Instrument.Key): Stream[F, (Quantity, Instant)] = ???
      // folios.rows filter (_._1 === instrument) map (r => (r._2._1, ???))
    }

    object Book
  }

  /** This object serves as the root of a dependent type tree for `ledger entries`.
    */
  object Entry {

    /**
      */
    type Key = Instrument.Key

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
  type Entry = (Entry.Key, Entry.Value)

  /** How much of a given [[capital.Instrument]] is held.
    *
    * Can also be thought of as a [[Trade]] [[Leg]] at rest.
    *
    * Note, this is just a type alias.
    */
  type Position = Entry

  /**
    */
  final lazy val Position = Entry

  /** A [[Entry]] in motion.
    *
    * Note, this is just a type alias.
    */
  type Leg = Entry

  /**
    */
  final lazy val Leg = Entry

  /** A set of (open) [[Position]]s.
    *
    * A `Folio` can be thought of as a "flat" portfolio",
    * i.e. a portfolio without sub portfolios.
    */
  type Folio = Map[Entry.Key, Entry.Value]

  /** A `Folio` key value store holds (open) [[Trade]]s,
    * indexed by opaque [[Account]] identifiers.
    *
    * Specifically: a `Map` of `Map`s, which, normalized and written out as a list,
    * has rows of type: {{{
    *   (Folio.Key, Instrument.Key, Quantity)
    * }}}
    */
  object Folio extends WithFuuidKey[Position] {

    /** Conceptually, lifts all the [[Position]]s into `Map`s,
      * and sums them as the `Map`s form commutative groups.
      *
      * Implementation differs, for efficiency.
      */
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)

    /**
      */
    def empty: Folio = Map.empty
  }

  /**
    */
  final lazy val Folios =
    KeyValueStore(Folio, KeyValueStore.Param.MKV).deriveKV[Entry.Key, Entry.Value]

  /** A [[Folio]] in motion, with the exception that unlike a `Folio`, a `Trade` cannot be empty.
    */
  type Trade = NonEmptyMap[Entry.Key, Entry.Value]

  /**
    */
  object Trade extends WithId.Aux[Leg] {

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
  final lazy val Trades = ValueStore(Trade, ValueStore.Param.NEMKV).deriveKV[Entry.Key, Entry.Value]

  /** Root of the transaction metadata abstract datatype.
    */
  type Meta

  /** Persisted as an [[keyval.SADT]].
    *
    * Nota Bene: By exploiting the `content addressed` nature of the `store` and
    * recording the `Id` of an `SADT` instance, the [[Transaction]] record affords a
    * '''contemporaneous attestation''' of the data and its association with the transaction.
    *
    * This may be essential to the construction of certain smart [[contract]] systems, especially
    * in conjunction with digital signatures. Thus the metadata is bound into the Ricardian
    * lineage of the contract.
    */
  val Meta: WithSADT[Meta]

  /**
    */
  final lazy val Metas = ValueStore(Meta, ValueStore.Param.SADT(Meta)).deriveV[SADT.Aux[Meta]]

  /** The concrete record for `Ledger` updates.
    */
  sealed abstract case class Transaction private (
      at: Instant,
      folioA: Folio.Key,
      tradeA: Trade.Id,
      folioB: Folio.Key,
      tradeB: Trade.Id,
      meta: Meta.Id
  )

  /** Because `Transaction`s are immutable, we model them as pure values.
    */
  object Transaction extends WithId.Aux[Transaction] {

    /**
      */
    private def apply(
        at: Instant,
        folioA: Folio.Key,
        tradeA: Trade.Id,
        folioB: Folio.Key,
        tradeB: Trade.Id,
        meta: Meta.Id
    ): Transaction =
      new Transaction(at, folioA, tradeA, folioB, tradeB, meta) {}

    /**
      */
    def singleLeg[F[_]: Sync](
        trades: Trades.ValueStore[F],
        metas: Metas.ValueStore[F]
    )(
        folioA: Folio.Key,
        tradeA: Trade.Id,
        folioB: Folio.Key,
        leg: Leg,
        meta: Meta
    ): F[Transaction] =
      multiLeg(trades, metas)(folioA, tradeA, folioB, Trade(leg), meta)

    /**
      */
    def multiLeg[F[_]: Sync](
        trades: Trades.ValueStore[F],
        metas: Metas.ValueStore[F]
    )(
        folioA: Folio.Key,
        tradeA: Trade.Id,
        folioB: Folio.Key,
        trade: Trade,
        meta: Meta
    ): F[Transaction] =
      for {
        tid <- trades put trade
        mid <- metas put meta map (_._1)
      } yield Transaction(instant, folioA, tradeA, folioB, tid._1, mid)

    /**
      */
    implicit lazy val transactionEq: Eq[Transaction] = { import auto.eq._; semi.eq }

    /**
      */
    implicit lazy val transactionShow: Show[Transaction] = { import auto.show._; semi.show }
  }

  /**
    */
  final lazy val Transactions = ValueStore(Transaction, ValueStore.Param.V).deriveV[Transaction]

  /** Note that [[Folio.Id]] is actually an `update event` for the specified [[Folio.Key]].
    * TODO: get this to work across nodes in a cluster
    */
  sealed abstract case class Confirmation private (at: Instant, from: Folio.Id, to: Folio.Id)

  /**
    */
  object Confirmation extends WithKey.Aux[Transaction.Id, Confirmation] {

    /**
      */
    private[model] def apply(at: Instant, from: Folio.Id, to: Folio.Id): Confirmation =
      new Confirmation(at, from, to) {}

    /**
      */
    object Key extends KeyCompanion[Transaction.Id] {
      implicit def order = Order[Transaction.Id]
      implicit def show  = Show[Transaction.Id]
    }

    /**
      */
    implicit lazy val confirmationEq: Eq[Confirmation] = { import auto.eq._; semi.eq }

    /**
      */
    implicit lazy val confirmationShow: Show[Confirmation] = { import auto.show._; semi.show }

    /** Drives the exchange of [[Trade]]s between [[Folio]]s.
      *
      * FIXME: define and implement
      */
    trait Agent

    /**
      */
    object Agent

    /** For a given [[Transaction]], what part of the [[Trade]] remains unsettled?
      */
    def unsettled[F[_]]: Transaction.Id => Stream[F, Confirmation.Id] => F[Folio] =
      ???

    /** Monotonic, in that [[Transaction]]s are never observed to "unsettle".
      *
      * Nota Bene - '''memoize''' these if need be
      */
    def settled[F[_]: cats.Functor]: Transaction.Id => Stream[F, Confirmation.Id] => F[Boolean] =
      xn => cs => unsettled(xn)(cs) map (_ === Folio.empty)

  }

  /**
    */
  final lazy val Confirmations =
    KeyValueStore(Confirmation, KeyValueStore.Param.V).deriveV[Confirmation]

  /** '''Cash''' is ''fungable''and ''self-pricing''.
    */
  final def payment[F[_]: Sync, C: Currency](
      trades: Trades.ValueStore[F]
  )(
      payCash: Folio.Key => Money[C] => F[Result[Folio.Id]]
  )(
      drawOn: Folio.Key
  ): (Trade, Money[C]) => F[Result[Trade.Id]] = {
    case (trade, amount) =>
      for {
        idb <- trades put trade
        _   <- payCash(drawOn)(amount)
      } yield Result(idb._1.some)
  }

  /** wip
    */
  def unsettled2[F[_]](cs: Confirmation.Store[F])(xn: Transaction.Id): F[Trade] = ???
  // Confirmation.unsettled(xn)(cs select xn) FIXME
}
