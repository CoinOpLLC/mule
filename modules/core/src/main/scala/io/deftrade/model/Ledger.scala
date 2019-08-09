package io.deftrade
package model

import time._, money._, keyval._, capital._

import cats._
import cats.implicits._
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

  import io.deftrade.keyval.Fresh

  /** nb this is where fresh key policy is decided for the ledger */
  implicit def defaultFresh: Fresh[Folio.Key] = Fresh.zeroBasedIncr

  /** Domain specific tools for dealing with `Quantity`s */
  type Quantity = Q
  val Quantity = Financial[Quantity]

  /**
    * How much of a given [[capital.Instrument]] is held.
    *
    * Can also be thought of as a [[Leg]] at rest.
    */
  type Position = (Instrument.Key, Quantity)
  object Position

  /** A [[Position]] in motion */
  type Leg = Position
  lazy val Leg = Position

  /**
    * A set of [[Position]]s.
    *
    * A `Folio` can be thought of as a "flat portfolio", i.e. a portfolio without
    * sub portfolios.
    *
    * Finally, a `Folio` can also be thought of as a [[Trade]] at rest.
    */
  type Folio = Map[Instrument.Key, Quantity]
  object Folio extends WithOpaqueKey[Long, Folio] {
    def empty: Folio                = Map.empty
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)
  }

  /** A [[Folio]] in motion. */
  type Trade = Folio
  lazy val Trade = Folio

  /**
    * Models ready cash per currency.
    *
    * nb the `C` type parameter is purely phantom
    */
  sealed abstract case class Wallet[C] private (folio: Folio)

  /**
    * Wallet folios are guarranteed non-empty, in that there is at least one Position.
    *
    * (TODO: is this worth a new type?)
    * Also, when creating `Wallet`s, the `C` type parameter is checked for `Currency` status.
    */
  object Wallet extends WithOpaqueKey[Long, Folio] {

    private[deftrade] def apply[C: Currency](folio: Folio): Wallet[C] = new Wallet[C](folio) {}

    def apply[C: Currency](p: Position, ps: Position*): Wallet[C] =
      new Wallet[C](Folio(p +: ps: _*)) {}
  }

  /**
    * For [[Ledger]] updates, the `Transaction` is the concrete record of record, so to speak.
    *
    * A timestamp is required of all `Recorded Transaction`s, assigned by the `Recorder`:
    *  the transaction is provisional until dated, returned as a receipt.
    *
    * The exact semantics will depend on the higher level context
    *  (eg booking a trade vs receiving notice of settlement).
    *
    * nb: there is no currency field; cash payments are reified in currency-as-instrument.
    * Store the _cryptographic hash_ of whatever metadata there is.
    */
  sealed abstract case class Transaction(
      recordedAt: Option[Instant],
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

    def apply(
        debitFrom: Folio.Key,
        creditTo: Folio.Key,
        instrument: Instrument.Key,
        amount: Quantity,
        meta: Option[Json]
    ): Transaction =
      new Transaction(
        none,
        debitFrom,
        creditTo,
        Trade(instrument -> amount),
        Array.empty[Byte] // FIXME, and notice that typesafety is no help here - need moar shapeless
      ) {}

    /**
      * ex nihilo, yada yada ... Make sure I can plug in fs2.Stream[cats.effect.IO, ?] etc here
      */
    def empty[F[_]: Monad: MonoidK: Foldable]: F[Transaction] = MonoidK[F].empty[Transaction]

    implicit def order: Eq[Transaction]  = Eq.fromUniversalEquals[Transaction]
    implicit def hash: Hash[Transaction] = Hash.fromUniversalHashCode[Transaction]
  }

  /** Support for multiple contingent deal legs */
  sealed abstract case class AllOrNone(xs: List[Transaction])

  /** */
  object AllOrNone {}

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
    lazy val roles: NonEmptyMap[LegalEntity.Role, NonEmptySet[LegalEntity.Key]] =
      NonEmptyMap of (
        LegalEntity.Role.Principal -> principals.keys,
        LegalEntity.Role.nonPrincipals.map(np => (np, nonPrincipals(np))): _*
    )

    def withAgent(agent: LegalEntity.Key): Roster =
      Roster.unsafe(
        principals,
        role =>
          role match {
            case LegalEntity.Role.Agent => NonEmptySet one agent
            case np                     => nonPrincipals(np)
        }
      )

    def withAuditor(auditor: LegalEntity.Key): Roster = ??? // refactor with above
  }

  /** creation patterns for teams */
  object Roster {

    /**
      */
    private def unsafe(
        principals: UnitPartition[LegalEntity.Key, Quantity],
        nonPrincipals: LegalEntity.Role => NonEmptySet[LegalEntity.Key]
    ) = new Roster(principals, nonPrincipals) {}

    /**
      * Splits partition equally among [[LegalEntity.Role.Principal]]s.
      */
    def equalSplitFrom(rs: Map[LegalEntity.Role, LegalEntity.Key]): Result[Roster] = ???

    /**
      *
      */
    def single(key: LegalEntity.Key): Roster =
      unsafe(
        principals = UnitPartition single key,
        nonPrincipals = _ => NonEmptySet one key
      )
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
