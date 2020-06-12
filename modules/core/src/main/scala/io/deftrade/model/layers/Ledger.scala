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
import cats.{ Eq, Show }
import cats.derived.{ auto, semi }
import cats.effect.Sync

import eu.timepit.refined
import refined.cats._

import fs2.{ Pipe, Stream }

/**
  * Models the performance and recording of [[Trade]]s between [[Folio]]s as [[Transaction]]s.
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
trait Ledger { module: ModuleTypes =>

  /**  */
  sealed trait Pricer {

    /**  */
    type Priced

    /**  */
    type CurrencyTag

    /**  */
    type Effect[_]

    /** Note that we are using `price` as a ''verb'' here.  */
    val price: Priced => Stream[Effect, Money[CurrencyTag]]

    /**  */
    implicit val C: Currency[CurrencyTag]

    /**  */
    implicit val F: Sync[Effect]
  }

  /**
    * TODO: The three different kinds of `Pricer`, listed in order of abstraction (most to least):
    *
    *   - `Model`: reports a ''fair value'' modelled price
    *     - may depend on `market data` for callibration
    *     - therefore limited by accuracy (in practice)
    *   - `Mark`: reports the current ''mark to market'' price
    *     - may depend on a `model` for interpolation to thinly traded / untraded assets...
    *     - therefore extensible to all assets (theoretically)
    *   - `Book`: the price we paid for the asset
    *     - only covers the assets we own(ed).
    */
  object Pricer {

    /**   */
    abstract class Aux[F[_], P, C] protected (
        override val price: P => Stream[F, Money[C]]
    ) extends Pricer {
      final type Priced      = P
      final type CurrencyTag = C
      final type Effect[x]   = F[x]
    }
  }

  /** Price data per unit of instrument. */
  sealed abstract case class InstrumentPricer[F[_], C] private (
      final override val price: Instrument.Key => Stream[F, Money[C]]
  ) extends Pricer.Aux[F, Instrument.Key, C](price)

  /**
    * Deploy `Pricer`s by creating search paths per `Currency` and instantiating
    * these pricers in the implicit context.
    */
  object InstrumentPricer {

    /** summon implicit value */
    def apply[F[_]: Sync, C: Currency](
        implicit pricer: InstrumentPricer[F, C]
    ): InstrumentPricer[F, C] = pricer

    /**  */
    def instance[F[_], C](
        price: Instrument.Key => Stream[F, Money[C]]
    )(
        implicit
        _C: Currency[C],
        _F: Sync[F]
    ): InstrumentPricer[F, C] =
      new InstrumentPricer(price) { implicit val C = _C; implicit val F = _F }

    /** */
    def empty[F[_]: Sync, C: Currency]: InstrumentPricer[F, C] =
      instance(_ => Stream.empty[F])

    /**
      * @return an `InstrumentPricer` which be functions as a two element search path
      * of `InstrumentPricer`s.
      *
      * TODO: Looks like `InstrumentPricer` is a [[cats.Monoid]]
      * Not commutative: price given by `a` (if given) has precedence.
      */
    def combine[F[_]: Sync, C: Currency](
        a: InstrumentPricer[F, C],
        b: InstrumentPricer[F, C]
    ): InstrumentPricer[F, C] =
      instance { instrument =>
        (a price instrument) ++ (b price instrument) take 1
      }

    /** */
    implicit def instrumentPricerMonoid[F[_]: Sync, C: Currency]: Monoid[InstrumentPricer[F, C]] =
      Monoid instance (empty[F, C], combine[F, C])
  }

  /**
    * How much of a given [[capital.Instrument]] is held.
    *
    * Can also be thought of as a [[Trade]] [[Leg]] at rest.
    */
  type Position = (Instrument.Key, Quantity)

  /** */
  object Position {

    /**  Enables volume discounts or other quantity-specific pricing. */
    sealed abstract case class Pricer[F[_], C] private (
        final override val price: Position => Stream[F, Money[C]]
    ) extends module.Pricer.Aux[F, Position, C](price)

    /** */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[F[_], C: Currency](implicit pricer: Pricer[F, C]): Pricer[F, C] =
        pricer

      /** */
      def instance[F[_], C](
          price: Position => Stream[F, Money[C]]
      )(implicit _C: Currency[C], _F: Sync[F]): Pricer[F, C] =
        new Pricer(price) { implicit val C = _C; implicit val F = _F }

      /** Create a pricer from a pricing function. */
      implicit def default[F[_]: Sync, C: Currency: InstrumentPricer[F, *]]: Pricer[F, C] =
        instance {
          case (instrument, quantity) => InstrumentPricer[F, C] price instrument map (_ * quantity)
        }
    }
  }

  /** A [[Position]] in motion. */
  type Leg = Position

  /** placeholder */
  lazy val Leg = Position

  /**
    * A set of [[Position]]s.
    *
    * A `Folio` can be thought of as a "flat portfolio", i.e. a portfolio without
    * sub portfolios.
    *
    * A `Folio` can also be thought of as a "sheet" (as its name suggests) in a spreadsheet.
    *
    * Finally, a `Folio` can also be thought of as a [[Trade]] at rest.
    */
  type Folio = Map[Instrument.Key, Quantity]

  /**
    * A `Folio` store is a `Map` of `Map`s, which, normalized and written out as a list,
    * has rows of type: {{{
    *   (Folio.Key, Instrument.Key, Quantity)
    * }}}
    */
  object Folio extends WithFuuidKey[Position] { // sicc hacc

    /**
      * Conceptually, lifts all the [[Position]]s into `Map`s,
      * and sums them as the `Map`s form commutative groups.
      *
      * Implementation differs, for efficiency.
      */
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)

    /** */
    def empty: Folio = Map.empty
  }

  /** A [[Folio]] in motion. */
  type Trade = Folio

  /**
    * In contrast to a [[Folio]] `store`, [[Trade]] `store`s hold simple, ''immutable'' `value`s.
    */
  object Trade extends WithId[Leg] { // (sic)

    /** */
    def apply(ps: Leg*): Trade = indexAndSum(ps.toList)

    /** */
    def empty: Trade = Map.empty

    /**
      * Enables package deals, or portfolio valuation informed by covariance,
      * or other holistic methodology.
      */
    sealed abstract case class Pricer[F[_], C] private (
        final override val price: Trade => Stream[F, Money[C]]
    ) extends module.Pricer.Aux[F, Trade, C](price)

    /**    */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[F[_], C: Currency](implicit pricer: Pricer[F, C]): Pricer[F, C] =
        pricer

      /** */
      def instance[F[_], C](
          price: Trade => Stream[F, Money[C]]
      )(implicit _C: Currency[C], _F: Sync[F]): Pricer[F, C] =
        new Pricer(price) { implicit val C = _C; implicit val F = _F }

      /** Create a pricer from a pricing function. */
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      implicit def default[F[_]: Sync, C: Currency: Leg.Pricer[F, *]]: Pricer[F, C] =
        instance { trade =>
          val lp = Leg.Pricer[F, C]
          val prices = for {
            leg   <- Stream evals (Sync[F] delay trade.toList)
            price <- lp price leg
          } yield price
          prices foldMap identity
        }
    }

    /**  */
    def of[F[_]: Sync, C: Currency: Trade.Pricer[F, *]](
        trade: Trade
    ): Stream[F, (Trade, Money[C])] =
      for {
        amount <- Trade.Pricer[F, C] price trade
      } yield (trade, amount)
  }

  /**
    * Root of [[Transaction]] metadata ADT.
    *
    * TODO: this is tabula rasa right now do something with it
    */
  sealed abstract class Meta

  /** */
  object Meta extends WithId[Misc.Aux[Meta]] {

    /** */
    def apply: Meta = new Meta {}

    /** */
    implicit lazy val metaEq: Eq[Meta] = { import auto.eq._; semi.eq }

    /** */
    implicit lazy val metaShow: Show[Meta] = { import auto.show._; semi.show }
  }

  /**
    * The concrete record for `Ledger` updates.
    *
    * Do we mean `Transaction` in the ''business'' sense, or the ''computer science'' sense?
    * '''Yes''': both parties must agree upon the result, under all semantics for the term.
    *
    * The exact semantics will depend on the higher level context
    *  (eg booking a trade vs receiving notice of settlement).
    *
    * Note: there is no currency field; cash payments are reified in currency-as-instrument.
    * Store the '''cryptographic hash''' of whatever metadata there is.
    */
  sealed abstract case class Transaction private (
      at: Instant,
      from: Folio.Key,
      to: Folio.Key,
      trade: Trade.Id,
      meta: Meta.Id
  )

  /**
    * Because `Transaction`s are immutable, we model them as pure value classes
    *
    * No `Transaction` is created except within the context
    * of an effectful functor - e.g. `F[_]: Sync: ContextShift` among other possibilities.
    */
  object Transaction extends WithId[Transaction] {

    private def apply(
        at: Instant,
        from: Folio.Key,
        to: Folio.Key,
        trade: Trade.Id,
        meta: Meta.Id
    ): Transaction =
      new Transaction(at, from, to, trade, meta) {}

    /**  */
    def single[F[_]: Sync](record: (Trade, Meta) => Stream[F, (Trade.Id, Meta.Id)])(
        from: Folio.Key,
        to: Folio.Key,
        instrument: Instrument.Key,
        amount: Quantity,
        meta: Meta
    ): Stream[F, Transaction] = multi(record)(from, to, Trade(instrument -> amount), meta)

    /**   */
    def multi[F[_]: Sync](record: (Trade, Meta) => Stream[F, (Trade.Id, Meta.Id)])(
        from: Folio.Key,
        to: Folio.Key,
        trade: Trade,
        meta: Meta
    ): Stream[F, Transaction] =
      for {
        ids <- record(trade, meta)
      } yield Transaction(instant, from, to, ids._1, ids._2)

    /** */
    implicit lazy val transactionEq: Eq[Transaction] = { import auto.eq._; semi.eq }

    /** */
    implicit lazy val transactionShow: Show[Transaction] = { import auto.show._; semi.show }
  }

  /**
    *
    * '''Cash''' is:
    *   - ''fungable''
    *   - ''self-pricing''
    *
    *   - a bank account is a kind of [[capital.Instrument]]
    *   - we know ''our'' cash `instrument` details (e.g. `USD` bank accounts).
    *       - must track quantity (`balance`) per such `instrument` of ours
    *       - don't want to have to need to know `instrument` details of the other party
    *   - the [[capital.Instrument.Key key]] is a `String Refined `[[capital.keys.IsUsBan]]
    *   - a `Folio` can contain a number of such `instrument`s and associated quantities
    *     (bank balances)
    *   - `instrument.symbol === currency.code` for ''all'' `cash instrument`s
    *   - the `Trade`s recorded in the `Transaction` log do ''not'' record `cash instrument`
    *     details but instead
    *     deal in '''reified''' `cash instrument`s
    *         - with [[capital.Instrument.Key]]s constucted
    *     by taking the [[money.CurrencyLike.code three letter currency code]]
    *     as the '''exact''' `Instrument.Key`
    *   - The allocation of cash from/to each account is recorded in
    *     the [[keyval.KeyValueStore]] for `Folio`s.
    *
    *   - creates a complete (balanced), ''paid'' `Trade` with the `reified cash instrument`
    *   - records that `Trade` in its store and returns the id, which is ready-to-use in
    *     creating a [[Transaction]].
    *   - NOTE: it is up to the payCash function to effectfully subtract the amount
    *     from the cash position
    *   - TODO: ensure the `payCash` update is atomic (should return zero or one `Folio.Id`)
    *   - TODO: is is possible or desirable to generalize fungability
    *     to asset classes other than currencies
    *
    *   @return `Stream` effect will effectively subtract amount from the `against` folio
    */
  final def payment[F[_]: Sync, C: Currency](
      recordTrade: Trade => Stream[F, Trade.Id],
      payCash: Folio.Key => Money[C] => Stream[F, Folio.Id],
  )(
      drawOn: Folio.Key
  ): Pipe[F, (Trade, Money[C]), Result[Trade.Id]] =
    _ flatMap {
      case (trade, amount) =>
        for {
          tId <- recordTrade(trade)
          _   <- payCash(drawOn)(amount)
        } yield Result(tId.some)
    }
}
