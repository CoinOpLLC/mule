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

import implicits._, time._, money._, keyval._, capital._
import refinements.Sha256

import cats.implicits._

import cats.effect.Sync
import fs2.Stream

import eu.timepit.refined
import refined.api.Refined

import io.circe.Json

import scala.language.higherKinds

/**
  * Tabulation of `Ledger`s of `Folio`s from `Transaction`s.
  *
  * A note on `Mark` vs `Price`: as attested generally in use within the domain of finance,
  * and specifically as used here: we distinguish between marking and pricing:
  *   - Marking: what stuff is worth. [[capital.Instrument]]s are marked
  * with respect to market trading data.
  *   - Pricing: what stuff ought to be worth. "Fair value", if you will, for various values
  * of "fair and "value". [[capital.Instrument]]s are priced based on pricing models.
  * It should also be noted that the sophistication with which marks are extrapolated from market
  * data creates ambiguities between the two categories in the real world; model mindfully. :|
  */
trait Ledger { self: ModuleTypes =>

  /** nb this is where fresh key policy is decided for the ledger */
  final def defaultFresh: Fresh[Folio.Key] = Fresh.zeroBasedIncr

  /**
    * How much of a given [[capital.Instrument]] is held.
    *
    * Can also be thought of as a [[Trade]] [[Leg]] at rest.

    * Positions are Marked, as opposed to Priced: the mark-to-market is necessary for net balance
    * presentation and margin calculations.
    */
  type Position = (Instrument.Key, Quantity)

  /** */
  object Position

  /** A [[Position]] in motion. */
  type Leg = Position

  /** */
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
    * Tricky semantics: the collection of all [[Folio]]s is a [[scala.collection.Map]] of `Map`s.
    * FIXME: csv won't work as is; needs (K1, (K2, V)) => (K1, K2, V) type function on Row...
    * ... shapeless?
    */
  object Folio extends WithOpaqueKey[Long, Position] { // sicc hacc

    /**
      * Conceptually, lifts all the [[Position]]s into `Map`s,
      * and sums them as the `Map`s form commutative groups.
      *
      * Implementation differs, for efficiency.
      *
      * TODO: verify implementation against concept.
      */
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)

    /** */
    def empty: Folio = Map.empty
  }

  /** A [[Folio]] in motion. */
  type Trade = Folio

  /**
    * In contrast to a [[Folio]] store, [[Trade]] [[io.deftrade.keyval.stores]] hold
    * immutable entries.
    */
  object Trade extends WithId[Leg] { // sicc - to be continued

    /** */
    def apply(ps: Leg*): Trade = indexAndSum(ps.toList)

    /** */
    def empty: Trade = Map.empty
  }

  /** raw price data usually comes this way */
  sealed abstract case class InstrumentPricer[F[_], C](
      price: Instrument.Key => Stream[F, Result[Mny[C]]]
  )

  /** TODO: need some way of composing these... */
  object InstrumentPricer {

    // type KMES[F[_], A, C] = Kleisli[Stream[F, *], A, Mny[C]]

    private def apply[F[_]: Sync, C: Currency](
        price: Instrument.Key => Stream[F, Result[Mny[C]]]
    ) =
      new InstrumentPricer(price) {}

    /** */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def combine[F[_]: Sync, C: Currency](
        a: InstrumentPricer[F, C],
        b: InstrumentPricer[F, C]
    ): InstrumentPricer[F, C] = // FIXME this code is lazy and not in a good way
      apply(instrument => (a price instrument) ++ (b price instrument) take 1)
  }

  /**    */
  sealed abstract case class LegPricer[C](price: Leg => Result[Mny[C]])

  /** TODO: Revisit decision to make these part of the implicit context. */
  sealed abstract case class TradePricer[C](price: Trade => Result[Mny[C]])

  /**    */
  object TradePricer {

    /** Summon a pricer for a given currency. */
    def apply[C: Currency: TradePricer]: TradePricer[C] = implicitly

    /** Create a pricer from a pricing function. */
    def apply[C: Currency](price: Trade => Result[Mny[C]]): TradePricer[C] =
      new TradePricer(price) {}
  }

  /** */
  sealed abstract case class PricedTrade[C](trade: Trade.Id, amount: Mny[C])

  /** */
  object PricedTrade {

    /** res ipsa fixit pleaz */
    def HACK_SELECT(trade: Trade.Id): Trade = ???

    /**      */
    def apply[C: Currency: TradePricer](trade: Trade.Id): Result[PricedTrade[C]] =
      for {
        amount <- TradePricer[C] price HACK_SELECT(trade)
      } yield new PricedTrade[C](trade, amount) {}
  }

  /** */
  sealed abstract case class SettlableTrade private (trade: Trade)

  /**
    * An even exchange.
    */
  object SettlableTrade {

    def apply[C: Currency](folio: Folio.Key)(pt: PricedTrade[C]) =
      new SettlableTrade(cashOut(folio)(pt.trade, pt.amount)) {}

    final def cashOut[C: Currency](
        folio: Folio.Key
    )(
        trade: Trade.Id,
        amount: Mny[C]
    ): Trade = ???
  }

  /** type alias */
  type ValuedFolio[C] = PricedTrade[C]

  /** */
  lazy val ValuedFolio = PricedTrade

  /**
    * For [[Ledger]] updates, the `Transaction` is the concrete record of record, so to speak.
    *
    * Do we mean in the business sense or the computer science sense?
    * Yes: both parties must agree upon the result.
    *
    * The exact semantics will depend on the higher level context
    *  (eg booking a trade vs receiving notice of settlement).
    *
    * Note: there is no currency field; cash payments are reified in currency-as-instrument.
    * Store the _cryptographic hash_ of whatever metadata there is.
    */
  sealed abstract case class Transaction private (
      recordedAt: Instant,
      from: Folio.Key,
      to: Folio.Key,
      trade: Trade.Id,
      metaHash: Sha256
  )

  /**
    * Model as pure value classes, because `Transaction`s don't make sense
    * as anything but immutable.
    *
    * Useful invariant: No `Transaction` is created except within the context
    * of an effectful functor - e.g. `F[_]: Sync: ContextShift` among other possibilities.
    */
  object Transaction extends WithId[Transaction] {

    /** */
    private val hackSha256: Sha256 = Refined unsafeApply (0 until 256 / 8 map (_.toByte)).toArray

    /** */
    final type MetaSha = Meta => Sha256

    private def apply(from: Folio.Key, to: Folio.Key, rc: Trade.Id, metaHash: Sha256): Transaction =
      new Transaction(instant, from, to, rc, metaHash) {}

    /**
      * Digester. FIXME: actually implement
      */
    def digest: MetaSha = _ => hackSha256

    /** f'rinstance */
    type Meta = Json

    /**       */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def single[F[_]: Sync](record: Trade => Stream[F, Trade.Id])(
        from: Folio.Key,
        to: Folio.Key,
        instrument: Instrument.Key,
        amount: Quantity,
        meta: Json
    ): Stream[F, Transaction] = multi(record)(from, to, Trade(instrument -> amount), meta)

    /**       */
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def multi[F[_]: Sync](record: Trade => Stream[F, Trade.Id])(
        from: Folio.Key,
        to: Folio.Key,
        trade: Trade,
        meta: Json
    ): Stream[F, Transaction] =
      for {
        id <- trade |> record
      } yield Transaction(from, to, id, meta |> digest)

    // /** TODO: investigate kittens for this. */
    // implicit def hash: Hash[Transaction] = Hash.fromUniversalHashCode[Transaction]
  }
}
