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

import time._, money._, keyval._, capital._, refinements.Sha256

import cats.implicits._
// import cats.{
//   // Foldable,
//   // Hash,
//   // Monad,
//   // MonoidK
// }

import eu.timepit.refined
import refined.api.Refined

import io.circe.Json

// import scala.language.higherKinds

/**
  * Tabulation of `Ledger`s of `Folio`s from `Transaction`s.
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

  /** In contrast to a [[Folio]] store, [[Trade]] stores holds immutable entries. */
  object Trade extends WithId[Trade] {

    /** */
    def apply(ps: Leg*): Trade = indexAndSum(ps.toList)

    /** */
    def empty: Trade = Map.empty
  }

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

    def HACK(trade: Trade.Id): Trade = ???

    /**      */
    def apply[C: Currency: TradePricer](trade: Trade.Id): Result[PricedTrade[C]] =
      for {
        amount <- TradePricer[C] price HACK(trade)
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
      debitFrom: Folio.Key,
      creditTo: Folio.Key,
      trade: Trade.Id,
      metaSha: Sha256
  )

  /**
    * Model as pure value classes, because `Transaction`s don't make sense
    * as anything but immutable.
    */
  object Transaction extends WithId[Transaction] {

    /** */
    private val hackSha256: Sha256 = Refined unsafeApply (0 until 256 / 8 map (_.toByte)).toArray

    /** */
    final type MetaSha = Meta => Sha256

    def apply(
        recordedAt: Instant,
        debitFrom: Folio.Key,
        creditTo: Folio.Key,
        trade: Trade.Id,
        metaSha: Sha256
    ): Transaction =
      new Transaction(
        instant,
        debitFrom,
        creditTo,
        trade,
        metaSha
      ) {}

    /**
      * Digester. FIXME: actually implement
      */
    def digest: MetaSha = _ => hackSha256

    /** f'rinstance */
    type Meta = Json

    /** */
    def single(
        debitFrom: Folio.Key,
        creditTo: Folio.Key,
        instrument: Instrument.Key,
        amount: Quantity,
        meta: Json
    ) = ???
    // Transaction(
    //   instant,
    //   debitFrom,
    //   creditTo,
    //   Trade(instrument -> amount),
    //   meta |> digest
    // )

    // /**
    //   * ex nihilo, yada yada ...
    //   * TODO: interop with `fs2.Stream[cats.effect.IO, ?]`
    //   */
    // def empty[F[_]: Monad: MonoidK: Foldable]: F[Transaction] = MonoidK[F].empty[Transaction]
    //
    // /** TODO: investigate kittens for this. */
    // implicit def hash: Hash[Transaction] = Hash.fromUniversalHashCode[Transaction]
  }
}
