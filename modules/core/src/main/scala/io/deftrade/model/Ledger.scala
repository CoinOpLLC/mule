package io.deftrade
package model

import time._, money._, keyval._, capital._

import cats.implicits._
import cats.{ Eq, Foldable, Hash, Monad, MonoidK }
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
import refined.W
import refined.numeric.Interval

import io.circe.Json

import scala.language.higherKinds

/**
  * Tabulation of `Ledger`s of `Folio`s from `Transaction`s.
  */
abstract class Ledger[Q: Financial] { self =>

  /** */
  final type Quantity = Q

  /** Domain specific tools for dealing with `Quantity`s */
  final val Quantity = Financial[Quantity]

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

  /** A [[Position]] in motion */
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

    /** */
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)

    /** */
    def empty: Folio = Map.empty
  }

  /** A [[Folio]] in motion. */
  type Trade = Folio

  /** */
  lazy val Trade = Folio

  /**
    * Models ready cash per currency.
    *
    * In this way, implicit values of `Wallet` can be used to inject maps ("pricers") between
    * (certain) [[Folio]]s and (certain) [[money.Currency]]s into the implicit context.
    *
    * @note The `C` type parameter is purely phantom; in particular, implicit [[money.Currency]]
    * values are '''not''' carried by instances of this class.
    */
  sealed abstract case class Wallet[C] private (folio: Folio)

  /**
    * Wallet folios are guarranteed non-empty, in that there is at least one Position.
    */
  object Wallet extends WithOpaqueKey[Long, Folio] {

    /**
      * type parameter is checked for `Currency` status
      * TODO: additional validation?
      */
    def apply[C: Currency](p: Position, ps: Position*): Wallet[C] =
      new Wallet[C](Folio(p +: ps: _*)) {}

    /** type parameter is checked for `Currency` status */
    private[deftrade] def apply[C: Currency](folio: Folio): Wallet[C] = new Wallet[C](folio) {}
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
      metaSha: Array[Byte]
  )

  /**
    * Do we mean in the business sense or the computer science sense?
    *
    * Yes: both parties must agree upon the result .
    */
  object Transaction extends WithOpaqueKey[Long, Transaction] {

    /** */
    type Meta = io.circe.Json // f'rinstance

    /**
      * TODO: define and implement `metaSha` field
      */
    def simple(
        debitFrom: Folio.Key,
        creditTo: Folio.Key,
        instrument: Instrument.Key,
        amount: Quantity,
        meta: Option[Json]
    ): Transaction =
      new Transaction(
        instant,
        debitFrom,
        creditTo,
        Trade(instrument -> amount),
        Array.empty[Byte] // notice that typesafety is no help here - need moar shapeless
      ) {}

    /**
      * ex nihilo, yada yada ... Make sure I can plug in fs2.Stream[cats.effect.IO, ?] etc here
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
        Role.nonPrincipals.map(np => (np, nonPrincipals(np))): _*
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
      * Splits partition equally among [[Role.Principal]]s.
      */
    def equalSplitFrom(rs: Map[Role, Key]): Result[Roster] = ???

    /** */
    def single(key: Key): Roster =
      unsafe(
        principals = UnitPartition single key,
        nonPrincipals = _ => NonEmptySet one key
      )

    private def unsafe(
        principals: UnitPartition[Key, Quantity],
        nonPrincipals: Role => NonEmptySet[Key]
    ) = new Roster(principals, nonPrincipals) {}
  }

  type AccountNo = Interval.Closed[W.`100000100100L`.T, W.`999999999999L`.T]
  implicit def eqHackHackHack: Eq[AccountNo] = Eq.fromUniversalEquals[AccountNo]

  /**
    * `Account`s consist of:
    *   - a `Roster`: who gets to do what, and who are the beneficial owners.
    *   - a `Folio` (list of instruments and their quantities)
    */
  sealed abstract case class Account(roster: Roster, folioKey: Folio.Key)

  /** */
  object Account extends WithPredicateKey[Long, AccountNo, Account] {

    def unsafe(roster: Roster, folioKey: Folio.Key) = new Account(roster, folioKey) {}

    def simple(le: LegalEntity.Key, f: Folio.Key): Account = unsafe(Roster single le, f)

    implicit def eq = Eq.fromUniversalEquals[Account]
  }
}
