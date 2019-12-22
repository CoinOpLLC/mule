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
  * Basic transacting.
  *
  * TODO: consider introducing `Mark`ing as a variant of `Pricing`.
  * A note on `Mark` vs `Price`: as attested generally within the domain of finance,
  * and specifically as used here: we propose to distinguish between marking and pricing:
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
    */
  type Position = (Instrument.Key, Quantity)

  /** placeholder */
  object Position

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
  object Folio extends WithOpaqueKey[Long, Position] { // sicc hacc

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
    * In contrast to a [[Folio]] store, [[Trade]] [[io.deftrade.keyval.stores]] hold
    * immutable entries.
    *
    * TODO: `Trade` stores should use WithHashId to get the natural reuse of `Trade`s
    * and minimization of store size.
    */
  object Trade extends WithId[Leg] { // sicc - to be continued

    /** */
    def apply(ps: Leg*): Trade = indexAndSum(ps.toList)

    /** */
    def empty: Trade = Map.empty
  }

  /**  */
  sealed trait Pricer {
    type Thing
    type CurrencyTag
    type EffectType[_]
    val price: Thing => Stream[EffectType, Mny[CurrencyTag]]
    implicit val C: Currency[CurrencyTag]
    implicit val F: Sync[EffectType]
  }

  /**
    * The three different kinds of `Pricer`, listed in order of abstraction (most to least):
    *
    *   - `Model`: reports a ''fair value'' modelled price
    *     - may depend on `market data` for callibration
    *     - therefore limited by accuracy (in practice)
    *   - `Mark`: reports the current ''mark to market'' price
    *     - may depend on a `model` for interpolation to thinly traded / untraded assets...
    *     - therefore extensible to all assets (theoretically)
    *   - `Book`: the price we paid for the asset
    *     - only covers the assets we own(ed).
    *
    * FIXME: implement this
    */
  object Pricer {

    /** I call this the untitled fois gras patttern. */
    abstract class Aux[F[_]: Sync, T, C: Currency](
        override val price: T => Stream[F, Mny[C]]
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
      final override val price: Instrument.Key => Stream[F, Mny[C]]
  ) extends Pricer.Aux[F, Instrument.Key, C](price)

  /** */
  object InstrumentPricer {

    /** */
    def apply[F[_], C: Currency: InstrumentPricer[F, *]]: InstrumentPricer[F, C] = implicitly

    /** */
    def apply[F[_]: Sync, C: Currency](
        price: Instrument.Key => Stream[F, Mny[C]]
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

  /**  Enables volume discounts or other quantity-specific pricing. */
  sealed abstract case class LegPricer[F[_]: Sync, C: Currency](
      final override val price: Leg => Stream[F, Mny[C]]
  ) extends Pricer.Aux[F, Leg, C](price)

  /** */
  object LegPricer {

    /** Summon a pricer for a given currency. */
    def apply[F[_], C: Currency: LegPricer[F, *]]: LegPricer[F, C] = implicitly

    /** */
    def apply[F[_]: Sync, C: Currency](price: Leg => Stream[F, Mny[C]]): LegPricer[F, C] =
      new LegPricer(price) {}

    /** Create a pricer from a pricing function. */
    def default[F[_]: Sync, C: Currency: InstrumentPricer[F, *]]: LegPricer[F, C] =
      apply {
        case (instrument, quantity) => InstrumentPricer[F, C] price instrument map (_ * quantity)
      }
  }

  /**
    * Enables package deals, or portfolio valuation informed by covariance,
    * or other holistic methodology.
    */
  sealed abstract case class TradePricer[F[_]: Sync, C: Currency](
      final override val price: Trade => Stream[F, Mny[C]]
  ) extends Pricer.Aux[F, Trade, C](price)

  /**    */
  object TradePricer {

    /** Summon a pricer for a given currency. */
    def apply[F[_], C: Currency: TradePricer[F, *]]: TradePricer[F, C] = implicitly

    /** */
    def apply[F[_]: Sync, C: Currency](
        price: Trade => Stream[F, Mny[C]]
    ): TradePricer[F, C] =
      new TradePricer(price) {}

    /** Create a pricer from a pricing function. */
    def default[F[_]: Sync, C: Currency: LegPricer[F, *]]: TradePricer[F, C] =
      TradePricer { trade =>
        val lp = LegPricer[F, C]
        @SuppressWarnings(Array("org.wartremover.warts.Any"))
        val prices = for {
          leg   <- Stream evals (Sync[F] delay trade.toList)
          price <- lp price leg
        } yield price
        prices foldMap identity
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
    *     [[keyval.stores.KeyValueStore]] for `Folio`s.
    *
    * TODO: is is possible or desirable to generalize fungability
    * to asset classes other than currencies?
    */
  sealed abstract case class PricedTrade[C](trade: Trade, amount: Mny[C]) {

    /**
      * @return `Stream` effect will effectively subtract amount from the `against` folio
      *   via a "coin selection" algo (allocating across multi bank accts)
      * - creates a complete, "paid" trade with a single unified, universal, anonymous,
      *   currency specific instrument of `amount`
      * - records that `Trade` in its store and returns the id, which is ready-to-use in
      *   creating a [[Transaction]].
      */
    final def paid[F[_]: Sync](
        against: Folio.Key
    )(
        trade: Trade,
        amount: Mny[C]
    )(
        implicit
        C: Currency[C]
    ): Stream[F, Result[Trade.Id]] =
      // find legal tender of right currency in folio
      // pos balance after subtraction of amount?
      // yes: subtract amount
      // no: subtract balance and recurse
      ???
  }

  /** */
  object PricedTrade {

    /**  */
    def of[F[_]: Sync, C: Currency: TradePricer[F, *]](
        trade: Trade
    ): Stream[F, PricedTrade[C]] =
      for {
        amount <- TradePricer[F, C] price trade
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
      recordedAt: Instant,
      from: Folio.Key,
      to: Folio.Key,
      trade: Trade.Id,
      metaHash: Sha256
  )

  /** f'rinstance */
  type Meta = Json

  import refinements.IsSha256

  /**
    * The Meta store is content-addressed: entries are indexed with their own Sha256.
    *
    * Therefore, if you have the Sha (from a [[Transaction]], for instance) ''and'' access to
    * a `Meta` key value store containing the value, you have access to the value itself.
    *
    * Note this value is effectively unforgeable / self validating.
    *
    * FIXME String should be byte string.
    * FIXME `WithHashId` should be a thing.
    */
  object Meta extends WithRefinedKey[String, IsSha256, Meta]

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
