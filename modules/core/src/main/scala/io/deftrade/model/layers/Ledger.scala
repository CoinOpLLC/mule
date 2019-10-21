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

import implicits._, time._, money._, keyval._, capital._, refinements.Sha256

import cats.implicits._
import cats.{ Eq, Foldable, Hash, Monad, MonoidK, Show }
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
import refined.api.Refined
import refined.W
import refined.numeric.Interval
import refined.cats.refTypeOrder

import io.circe.Json

import scala.language.higherKinds

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

  /** */
  lazy val Trade = Folio

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
  sealed abstract case class PricedTrade[C](trade: Trade, amount: Mny[C])

  /** */
  object PricedTrade {

    /**      */
    def apply[C: Currency: TradePricer](trade: Trade): Result[PricedTrade[C]] =
      for {
        amount <- TradePricer[C] price trade
      } yield new PricedTrade[C](trade, amount) {}

    /**
      * Reduct to "currency as `Instrument`" convention, according to the contents
      * of a certain (implicit) [[Wallet]].
      *
      * Creates what could be called a `FairTrade`...
      */
    def normalize[C: Currency](pt: PricedTrade[C])(implicit ci: Wallet.Aux[C]): Trade = ???
  }

  /** type alias */
  type ValuedFolio[C] = PricedTrade[C]

  /** */
  lazy val ValuedFolio = PricedTrade

  /**
    * Models ready cash per currency.
    *
    * In this way, implicit values of `Wallet` can be used to inject maps ("pricers") between
    * (certain) [[Folio]]s and (certain) [[money.Currency]]s into the implicit context.
    *
    * @note The `C` type parameter is purely phantom; in particular, implicit [[money.Currency]]
    * values are '''not''' carried by instances of this class.
    */
  /** FIXME move this outside the object DUH */
  sealed abstract case class Wallet(C: CurrencyLike, folio: Folio) {
    final type CurrencyType = C.Type
  }

  /**
    * Wallet folios are guarranteed non-empty, in that there is at least one Position.
    */
  object Wallet extends WithOpaqueKey[Long, Wallet] {

    final class Aux[C](C: Currency[C], folio: Folio) extends Wallet(C, folio) {}

    /** */
    def apply(c: CurrencyLike, folio: Folio): Wallet = new Wallet(c, folio) {}

    /**
      * type parameter is checked for `Currency` status
      * TODO: additional validation?
      */
    def apply[C: Currency](p: Position, ps: Position*): Wallet =
      Wallet(Currency[C], Folio(p +: ps: _*))

    /** type parameter is checked for `Currency` status */
    private[deftrade] def apply[C: Currency](folio: Folio): Wallet = Wallet(Currency[C], folio)
  }

  /**
    * For [[Ledger]] updates, the `Transaction` is the concrete record of record, so to speak.
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
      trade: Trade,
      metaSha: Sha256
  )

  /**
    * Do we mean in the business sense or the computer science sense?
    *
    * Yes: both parties must agree upon the result.
    */
  object Transaction extends WithOpaqueKey[Long, Transaction] {

    /** */
    private val hackSha256: Sha256 = Refined unsafeApply (0 until 256 / 8 map (_.toByte)).toArray

    /** */
    final type MetaSha = Meta => Sha256

    def apply(
        recordedAt: Instant,
        debitFrom: Folio.Key,
        creditTo: Folio.Key,
        trade: Trade,
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
    ) = Transaction(
      instant,
      debitFrom,
      creditTo,
      Trade(instrument -> amount),
      meta |> digest
    )

    /**
      * ex nihilo, yada yada ...
      * TODO: interop with `fs2.Stream[cats.effect.IO, ?]`
      */
    def empty[F[_]: Monad: MonoidK: Foldable]: F[Transaction] = MonoidK[F].empty[Transaction]

    /** TODO: investigate kittens for this. */
    implicit def hash: Hash[Transaction] = Hash.fromUniversalHashCode[Transaction]
  }

  /**
    * Each [[Account]] is created with a [[Roster]], specifying the beneficial owners
    * and their crew.
    *
    * Although the signature guarrantees a measure of safety, construction is private to the
    * class.
    *
    * @param principals Each has specified their share of the (capital) [[Account]].
    * @param nonPrincipals N.b. the signature and what it implies:
    *
    *   - a total function returning a `NonEmptySet`
    *   - implies that all non-Princple [[Role]]s must be filled with at least one [[Entity]].
    *
    */
  sealed abstract case class Roster private (
      principals: UnitPartition[LegalEntity.Key, Quantity],
      nonPrincipals: LegalEntity.Role => NonEmptySet[LegalEntity.Key]
  ) {
    import LegalEntity.{ Key, Role }

    /** */
    lazy val roles: NonEmptyMap[Role, NonEmptySet[Key]] =
      NonEmptyMap of (
        Role.Principal -> principals.keys,
        Role.nonPrincipals map (np => (np, nonPrincipals(np))): _*
    )

    /** */
    def withAgent(agent: Key): Roster =
      Roster unsafe (
        principals,
        role =>
          role match {
            case Role.Agent => NonEmptySet one agent
            case np         => nonPrincipals(np)
        }
      )

    /** */
    def withAuditor(auditor: LegalEntity.Key): Roster =
      Roster unsafe (
        principals,
        role =>
          role match {
            case Role.Auditor => NonEmptySet one auditor
            case np           => nonPrincipals(np)
        }
      )
  }

  /**
    * Creation patterns for account management teams.
    */
  object Roster {

    import LegalEntity.{ Key, Role }

    /**
      * By default, all share in [[Roster.nonPrincipals]] responsibilities equally,
      * regardless of their share of the principle pie
      */
    def forPrinciples(principles: UnitPartition[Key, Quantity]): Roster =
      unsafe(
        principles,
        _ => NonEmptySet(principles.keys.head, principles.keys.tail)
      )

    /**
      * Splits partition equally among [[LegalEntity.Role.Principal]]s.
      */
    def equalSplitFrom(rs: Map[Role, Key]): Result[Roster] = ???

    /** */
    def single(entity: Key): Roster =
      unsafe(
        principals = UnitPartition single entity,
        nonPrincipals = _ => NonEmptySet one entity
      )

    private def unsafe(
        principals: UnitPartition[Key, Quantity],
        nonPrincipals: Role => NonEmptySet[Key]
    ): Roster = new Roster(principals, nonPrincipals) {}
  }

  /**
    * Predicate defining a very conventional looking account numbering scheme.
    */
  type IsAccountNo = Interval.Closed[W.`100000100100L`.T, W.`999999999999L`.T]

  /**
    * `Account`s consist of:
    *   - a `Roster`: who gets to do what, and who are the beneficial owners.
    *   - a `Folio` (list of instruments and their quantities)
    */
  sealed abstract case class Account(roster: Roster, folioKey: Folio.Key)

  /** */
  object Account extends WithRefinedKey[Long, IsAccountNo, Account] {

    /** */
    def apply(roster: Roster, folio: Folio.Key): Account = new Account(roster, folio) {}

    /** */
    def single(entity: LegalEntity.Key, folio: Folio.Key) = Account(Roster single entity, folio)

    /** TODO: use kittens? Why or why not? */
    implicit def eq = Eq.fromUniversalEquals[Account]

    /** TODO: placeholder */
    implicit def show = Show.fromToString[Account]
  }
}
