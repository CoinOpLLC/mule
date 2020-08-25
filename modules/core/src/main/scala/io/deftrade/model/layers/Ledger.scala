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

/**
  * Models the performance and recording of [[Trade]]s between [[Folio]]s as [[Transaction]]s.
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

  /**
    * TODO: The different kinds of `Pricer`, listed in order of abstraction (most to least):
    *
    *   - `Model`: reports a ''fair value'' modelled price
    *     - may depend on `market data` for callibration
    *     - therefore limited by accuracy (in practice)
    *   - `Market`: reports the current ''mark to market'' price
    *     - may depend on a `model` for interpolation to thinly traded / untraded assets...
    *     - therefore extensible to all assets (theoretically)
    *   - `Book`: the price we paid for the asset
    *     - only covers the assets we own(ed).
    *     - captured from our `Transaction` stream.
    *   - `Cfg`: just load the pricing data from a config file (or .csv file).
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

    /**
      * Deploy `Pricer`s by creating search paths per `Currency` and instantiating
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

      /**
        * @return an `Instrument` which be functions as a two element search path
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

  /**
    * Entry as in ''double entry''. This object serves as the root of a dependent type tree.
    */
  object Entry {

    /**
      */
    type Key = Instrument.Key

    /**
      */
    type Value = Quantity

    /**
      */
    type Table = Map[Key, Value]

    /**
      */
    type NonEmptyTable = NonEmptyMap[Key, Value]
  }

  /**
    * Any kind of `ledger entry` must carry with it a `key`:
    * `value`s are assumed fungeable within `key`s, but not across them.
    */
  type Entry = (Entry.Key, Entry.Value)

  /**
    * How much of a given [[capital.Instrument]] is held.
    *
    * Can also be thought of as a [[Trade]] [[Leg]] at rest.
    */
  type Position = Entry

  /**
    */
  object Position {

    /** Enables volume discounts or other quantity-specific pricing. */
    sealed abstract case class Pricer[F[_], C: Currency] private (
        final override val price: Position => Stream[F, Money[C]]
    ) extends module.Pricer.Aux[F, Position, C](price)

    /**
      */
    object Pricer {

      /** Summon a pricer for a given currency. */
      def apply[F[_], C: Currency](implicit pricer: Pricer[F, C]): Pricer[F, C] =
        pricer

      /**
        */
      def instance[F[_], C: Currency](price: Position => Stream[F, Money[C]]): Pricer[F, C] =
        new Pricer(price) {}

      /** Create a pricer from a pricing function. */
      implicit def default[F[_]: Sync, C: Currency: module.Pricer.Instrument[F, *]]: Pricer[F, C] =
        instance {
          case (instrument, quantity) =>
            module.Pricer.Instrument[F, C] price instrument map (_ * quantity)
        }
    }
  }

  /** A [[Position]] in motion.
    */
  type Leg = Entry

  /**
    */
  lazy val Leg = Position

  /**
    * A set of (open) [[Position]]s.
    *
    * A `Folio` can be thought of as a "flat" portfolio",
    * i.e. a portfolio without sub portfolios.
    *
    * A `Folio` can also be thought of as a "sheet" in a spreadsheet.
    */
  type Folio = Entry.Table

  /**
    * A `Folio` key value store holds (open) [[Trade]]s,
    * indexed by opaque [[Account]] identifiers.
    *
    * Specifically: a `Map` of `Map`s, which, normalized and written out as a list,
    * has rows of type: {{{
    *   (Folio.Key, Instrument.Key, Quantity)
    * }}}
    */
  object Folio extends WithFuuidKey[Position] {

    /**
      * Conceptually, lifts all the [[Position]]s into `Map`s,
      * and sums them as the `Map`s form commutative groups.
      *
      * Implementation differs, for efficiency.
      */
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)

    /**
      */
    def empty: Folio = Map.empty
  }

  /** A [[Folio]] in motion. */
  type Trade = Entry.NonEmptyTable

  /**
    * In contrast to a [[Folio]] `store`, [[Trade]] `store`s hold simple, ''immutable'' `value`s.
    */
  object Trade extends WithId.Aux[Leg] {

    /**
      */
    def apply(l: Leg, ls: Leg*): Trade = indexAndSum(l, ls: _*)

    /**
      * Enables package deals, or portfolio valuation informed by covariance,
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

  /**
    * Root of the transaction metadata abstract datatype.
    */
  type Meta

  /**
    * Persisted as an [[keyval.SADT]].
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
    * The concrete record for `Ledger` updates.
    *
    * Do we mean `Transaction` in the ''business'' sense, or the ''computer science'' sense?
    * '''Yes''': both parties must agree upon the result, under all semantics for the term.
    *
    * The exact semantics will depend on the higher level context: a `Transaction`
    * memorializing a booked `Trade` will spawn a cascade of `Transaction`s (and [[Confirmation]]s)
    * as that `Transaction` is settled.
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
  object Transaction extends WithId.Aux[Transaction] {

    import Meta._

    /**
      */
    private def apply(
        at: Instant,
        from: Folio.Key,
        to: Folio.Key,
        trade: Trade.Id,
        meta: Meta.Id
    ): Transaction =
      new Transaction(at, from, to, trade, meta) {}

    /**
      */
    def singleLeg[F[_]: Sync](
        trades: Trade.StoreM[F],
        metas: Meta.Store[F]
    )(
        from: Folio.Key,
        to: Folio.Key,
        leg: Leg,
        meta: Meta
    ): F[Transaction] =
      multiLeg(trades, metas)(from, to, Trade(leg), meta)

    /**
      */
    def multiLeg[F[_]: Sync](
        trades: Trade.StoreM[F],
        metas: Meta.Store[F]
    )(
        from: Folio.Key,
        to: Folio.Key,
        trade: Trade,
        meta: Meta
    ): F[Transaction] =
      for {
        tid <- trades putNem trade
        mid <- metas put (SADT from meta) map (_._1)
      } yield Transaction(instant, from, to, tid._1, mid)

    /**
      */
    implicit lazy val transactionEq: Eq[Transaction] = { import auto.eq._; semi.eq }

    /**
      */
    implicit lazy val transactionShow: Show[Transaction] = { import auto.show._; semi.show }
  }

  /** Realize that `Folio { type UpdatedEvent = Id }`.
    *
    * A trail (`Stream`) of `Confirmations` is low-touch auditable.
    *
    * Nota Bene: the `Transaction.Id` is chained into the `Confirmation.Id`s that go into the
    * settlement computation.
    *
    * Q: How do we settle a `Trade`?
    *     - the net effect must be to transfer the `Trade` between `Folio`s.
    *
    * A: Use two fibers (per `Trade`)
    *     - one side (fiber) does:
    *         - `expect.n = trade.n |> diodePos` // the thing being traded, typically
    *         - `escrow.n = trade.n |> diodeNeg` // the payment, typically
    *         - workflow option one:
    *             - "manual" payment transfer
    *             - `fiber.wait(escrow.n === empty)` // reactive
    *         - workflow option two
    *             - assumes programmatic transfer capability for assets! (USD typically)
    *             - `open += escrow.n              ` // proactive
    *             - `escrow.n = empty`
    *     - other side (fiber) does:
    *         - same, except for `Trade` * -1
    *     - join on both fibers (''now'' `trade.n` can be settled)
    *     - both sides:
    *         - `open += expect.n`
    *         - `expect.n = empty`
    *     - ''nota bene''
    *         - no `Folio.Key`s need be exchanged at all
    *             - `join` does all the work
    *         - both `expect` and `escrow` are left empty in a successful settlement
    *             - eliminated on snapshot
    *
    * TODO: get this to work across nodes in a cluster
    */
  sealed abstract case class Confirmation private (from: Folio.Id, to: Folio.Id)

  /** TODO: implement the ''No deletion'' policy for this `KeyValueStore`
    */
  object Confirmation extends WithKey.Aux[Transaction.Id, Confirmation] {

    /**
      */
    private[model] def apply(from: Folio.Id, to: Folio.Id): Confirmation =
      new Confirmation(from, to) {}

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

    /**
      * Drives the exchange of [[Trade]]s between [[Folio]]s.
      *
      * FIXME: define and implement
      */
    trait Agent

    /**
      */
    object Agent

    /**
      * For a given [[Transaction]], what part of the [[Trade]] remains unsettled?
      */
    def unsettled[F[_]]: Transaction.Id => Stream[F, Confirmation.Id] => F[Folio] =
      ???

    /**
      * Monotonic, in that [[Transaction]]s are never observed to "unsettle".
      *
      * Nota Bene - '''memoize''' these if need be
      */
    def settled[F[_]: cats.Functor]: Transaction.Id => Stream[F, Confirmation.Id] => F[Boolean] =
      xn => cs => unsettled(xn)(cs) map (_ === Folio.empty)

  }

  /**
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
      trades: Trade.Store[F]
  )(
      payCash: Folio.Key => Money[C] => Stream[F, Result[Folio.Id]]
  )(
      drawOn: Folio.Key
  ): Pipe[F, (Trade, Money[C]), Result[Trade.Id]] =
    _ flatMap {
      case (trade, amount) =>
        // for {
        //   id <- trades putMap trade
        //   _  <- payCash(drawOn)(amount)
        // } yield Result(id.some)
        ???
    }

  /** wip
    */
  def unsettled2[F[_]](cs: Confirmation.Store[F])(xn: Transaction.Id): F[Trade] = ???
  // Confirmation.unsettled(xn)(cs select xn) FIXME
}
