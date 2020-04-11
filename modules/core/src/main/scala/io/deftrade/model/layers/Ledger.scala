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

import cats.{ Eq, Show }
import cats.derived.{ auto, semi }
import cats.effect.Sync

import eu.timepit.refined
import refined.cats._

import fs2.Stream

import io.circe.{ Decoder, Encoder, Json }

/**
  * Models the performance and recording of [[Trade]]s between [[Folio]]s as [[Transaction]]s.
  */
trait Ledger { module: ModuleTypes =>

  /**  */
  sealed trait Pricer {

    /**  */
    type Thing

    /**  */
    type CurrencyTag

    /**  */
    type EffectType[_]

    /**  */
    val price: Thing => Stream[EffectType, Money[CurrencyTag]]

    /**  */
    implicit val C: Currency[CurrencyTag]

    /**  */
    implicit val F: Sync[EffectType]
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

    /** This version of Aux is called the ''untitled fois gras patttern''. */
    abstract class Aux[F[_]: Sync, T, C: Currency](
        override val price: T => Stream[F, Money[C]]
    )(
        implicit
        final override val C: Currency[C],
        final override val F: Sync[F]
    ) extends Pricer {
      final type Thing         = T
      final type CurrencyTag   = C
      final type EffectType[x] = F[x]
    }
  }

  /** Price data per unit of instrument. */
  sealed abstract case class InstrumentPricer[F[_]: Sync, C: Currency](
      final override val price: Instrument.Key => Stream[F, Money[C]]
  ) extends Pricer.Aux[F, Instrument.Key, C](price)

  /** */
  object InstrumentPricer {

    /** */
    def apply[F[_], C: Currency: InstrumentPricer[F, *]]: InstrumentPricer[F, C] = implicitly

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def apply[F[_]: Sync, C: Currency](
        price: Instrument.Key => Stream[F, Money[C]]
    ): InstrumentPricer[F, C] =
      new InstrumentPricer(price) {}

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def empty[F[_]: Sync, C: Currency]: InstrumentPricer[F, C] =
      new InstrumentPricer(
        _ => Stream.empty[F]
      ) {}

    /**
      * @return an `InstrumentPricer` which be functions as a two element search path
      * of `InstrumentPricer`s.
      *
      * TODO: Looks like `InstrumentPricer` is a [[cats.Monoid]]
      * Not commutative: price given by `a` (if given) has precedence.
      */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def combine[F[_]: Sync, C: Currency](
        a: InstrumentPricer[F, C],
        b: InstrumentPricer[F, C]
    ): InstrumentPricer[F, C] =
      InstrumentPricer { instrument =>
        (a price instrument) ++ (b price instrument) take 1
      }
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
    sealed abstract case class Pricer[F[_]: Sync, C: Currency](
        final override val price: Position => Stream[F, Money[C]]
    ) extends module.Pricer.Aux[F, Position, C](price)

    /** */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[F[_], C: Currency: Pricer[F, *]]: Pricer[F, C] = implicitly

      /** */
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      def apply[F[_]: Sync, C: Currency](price: Position => Stream[F, Money[C]]): Pricer[F, C] =
        new Pricer(price) {}

      /** Create a pricer from a pricing function. */
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      implicit def default[F[_]: Sync, C: Currency: InstrumentPricer[F, *]]: Pricer[F, C] =
        apply {
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
    sealed abstract case class Pricer[F[_]: Sync, C: Currency](
        final override val price: Trade => Stream[F, Money[C]]
    ) extends module.Pricer.Aux[F, Trade, C](price)

    /**    */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[F[_], C: Currency: Pricer[F, *]]: Pricer[F, C] = implicitly

      /** */
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      def apply[F[_]: Sync, C: Currency](
          price: Trade => Stream[F, Money[C]]
      ): Pricer[F, C] =
        new Pricer(price) {}

      /** Create a pricer from a pricing function. */
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      implicit def default[F[_]: Sync, C: Currency: Leg.Pricer[F, *]]: Pricer[F, C] =
        Pricer { trade =>
          val lp = Leg.Pricer[F, C]
          val prices = for {
            leg   <- Stream evals (Sync[F] delay trade.toList)
            price <- lp price leg
          } yield price
          prices foldMap identity
        }
    }
  }

  /**
    * Requirements for dealing with '''cash''':
    *   - cash has to be fungable and self-pricing.
    *   - for legal tender instruments, we only ever know ''our'' actual instrument (bank acct).
    *   - need to keep quantity per such "instrument" (bank acct)!
    *
    * So:
    *   - a bank account is a kind of [[capital.Instrument]],
    *   - and the key is `String Refined IsBan`,
    *   - and a `Folio` can contain a number of such "instruments" and associated quantities
    *     (bank balances.)
    *   - `instrument.symbol === currency.code` for all fungible currency instruments
    *   - the `Trade`s recorded in the books do not record these bank account details but instead
    *     deal in generic instruments with [[capital.Instrument.Key]]s constucted systematically
    *     from Currency names.
    *   - the bank account allocation details are recoverable through the
    *     [[keyval.KeyValueStore]] for `Folio`s.
    *
    * FIXME: need `PricedTrade => Trade` function.
    *
    * TODO: is is possible or desirable to generalize fungability
    * to asset classes other than currencies
    */
  sealed abstract case class PricedTrade[C](trade: Trade, amount: Money[C]) {

    /**
      * FIXME: this belongs at a higher level but it's proven valuable to work through here.
      *
      * @return `Stream` effect will effectively subtract amount from the `against` folio
      *   via a "coin selection" algo (allocating across multi bank accts)
      * - creates a complete, "paid" trade with a single unified, universal, anonymous,
      *   currency specific instrument of `amount`
      * - records that `Trade` in its store and returns the id, which is ready-to-use in
      *   creating a [[Transaction]].
      */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    final def paid[F[_]: Sync](
        recordTrade: Trade => Stream[F, Trade.Id],
        cashPositions: Currency[C] => Folio.Key => Stream[F, List[Position]],
        payCash: Folio.Key => Position => Stream[F, Folio.Id]
    )(
        drawAgainst: Folio.Key
    )(
        implicit
        C: Currency[C],
        F: Sync[F]
    ): Stream[F, Result[Trade.Id]] = {
      // find legal tender of right currency in folio
      val ret = for {
        t         <- recordTrade(trade)
        cps       <- cashPositions(C)(drawAgainst)
        h         <- Stream evals (F delay cps)
        receiptId <- payCash(drawAgainst)(h)
      } yield Result(t.some) // (h._1, h._2 - amount)
      ret // FIXME this is wrong, just checking compilation`
    } // pos balance after subtraction of amount?
    // yes: subtract amount
    // no: subtract balance and recurse
  }

  /** */
  object PricedTrade {

    /**  */
    def of[F[_]: Sync, C: Currency: Trade.Pricer[F, *]](
        trade: Trade
    ): Stream[F, PricedTrade[C]] =
      for {
        amount <- Trade.Pricer[F, C] price trade
      } yield new PricedTrade[C](trade, amount) {}
  }

  /**
    * type alias
    */
  type ValuedFolio[C] = PricedTrade[C]

  /** */
  lazy val ValuedFolio = PricedTrade

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
        instant: Instant,
        from: Folio.Key,
        to: Folio.Key,
        rc: Trade.Id,
        meta: Meta.Id
    ): Transaction =
      new Transaction(instant, from, to, rc, meta) {}

    /**  */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
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
    * All metadata is `json`.
    *
    * We can encode and decode `ADT`s from `json`, so the mapping is universal.
    */
  sealed abstract class Meta private (meta: Json) { type Adt }

  /**
    * The Meta store is content-addressed: entries are indexed with their own Sha.
    *
    * Therefore, if you have the Sha (from a [[Transaction]], for instance) ''and'' access to
    * a `Meta` key value store containing the value, you have access to the value itself.
    *
    * Note this value is effectively unforgeable / self validating.
    */
  object Meta extends WithId[Meta] {

    /** */
    final case class Aux[T] private (meta: Json) extends Meta(meta) { final type Adr = T }

    /** */
    def apply[T: Encoder: Decoder](meta: Json): Meta = Aux[T](meta)

    /** */
    implicit def metaEq[T]: Eq[Meta.Aux[T]] = { import auto.eq._; semi.eq }

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    implicit def metaShow[T]: Show[Meta.Aux[T]] = { import auto.show._; semi.show }
  }
}
