package io.deftrade
package model

import money._, keyval._

import enumeratum._

import cats._
import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
import refined.W
// import refined.api.Refined
import refined.numeric._
// import refined.{ cats => refinedCats, _ }

/** package level API */
abstract class EntityAccountMapping[Q: Financial] extends Ledger[Q] { self =>

  /**
    * There are a finite enumeration of [[Roles]].
    * Every `Role` is mapped to a [[LegalEntity]] via a [[Roster]].
    */
  sealed trait Role extends EnumEntry

  /**
    * Enumerated `Role`s.
    * Note: this enum is not dependant on the type parameter and so may be hoisted up.
    * Actually a primitive, in other words.
    */
  object Role extends Enum[Role] with CatsEnum[Role] {

    /**
      * There is _always_ a distinguished `Role`, the `Principal`.
      */
    sealed trait Principal extends Role

    /**
      * A type representing all `Role`s _other_ than `Princple`.
      */
    sealed trait NonPrincipal extends Role
    object NonPrincipal // TODO extractor

    /**
      * The [[LegalEntity]] which is the economic actor responsible for establishing the [[Account]].
      *
      * Semantics for `Principal` are conditioned on the status of account, for examples:
      * - beneficial owner for an asset
      * - responsible party for a liability
      * - shareholder for equity
      * - business unit chief for revenue and expenses
      */
    case object Principal extends Principal

    /**
      * The primary delegate selected by a `Principal`.
      * Also, simply, the `LegalEntity`(s) whose names are listed on the `Account`,
      * and the primary point of contact for the `Account`.
      *
      * `Agents` have authortity to initiate `Transactions` which establish or remove `Position`s
      * from the `Ledger`.
      *
      * By convention a `Princple` is their own `Agent` unless otherwise specified.
      */
    case object Agent extends NonPrincipal

    /**
      * The primary delegate selected by the `Agent`.
      * `LegalEntity`(s) with responsibility for, and authority over,
      * the disposition of assets in the `Account`.
      *
      * In particular, `Manager`s may initiate `Transaction`s which will settle to the `Ledger`,
      * so long as the `Position`s are already entered in the `Ledger` - i.e. the `Instrument` is
      * known to be tradeable on the ledger.
      *
      * (All publicly listed and traded assets are treated as tradeable on the `Ledger`
      * by convention.)
      *
      * An `Agent` is their own `Manager` unless otherwise specified.
      */
    case object Manager extends NonPrincipal

    /**
      * `Auditor`s are first class entities, each with a package of rights and responsibilities
      * which is situation and juristiction specific.
      *
      * Practically, what this means is that `Auditor`s will have a (possibly limited) view
      * into the state of the `Ledger`,
      * and (possibly) the ability to block the settlement of `Transaction`s to the `Ledger`
      * or even intitiate `Transaction`s.
      *
      * Actions of the `Auditor` may include the publishing of specific summaries of its views
      * into the `Ledger` to establish common knowledge for participants in `Ledger` `Transaction`s.
      *
      * N.B.: the `Auditor` need not be a regulatory entity; in particular this role might
      * be suited e.g. to a Risk Manager, operating in the context of a hedge fund.
      */
    case object Auditor extends NonPrincipal

    /** The `findValues` macro collects all `value`s in the order written. */
    lazy val values: IndexedSeq[Role] = findValues

    /** FIXME use NonPrincipal::extractor */
    lazy val nonPrincipals: IndexedSeq[NonPrincipal] = values collect { case np: NonPrincipal => np }
  }

  /**
    * Each [[Account]] is created with a `Roster`, specifying the beneficial owners and their crew.
    */
  sealed abstract case class Roster private (
      principals: UnitPartition[LegalEntity.Key, Quantity],
      nonPrincipals: Role.NonPrincipal => NonEmptySet[LegalEntity.Key]
  ) {
    lazy val roles: NonEmptyMap[Role, NonEmptySet[LegalEntity.Key]] =
      NonEmptyMap of (
        Role.Principal -> principals.keys,
        Role.nonPrincipals.map(np => (np, nonPrincipals(np))): _*
    )

    def withAgent(agent: LegalEntity.Key): Roster =
      Roster.unsafe(
        principals,
        role =>
          role match {
            case Role.Agent => NonEmptySet one agent
            case np         => nonPrincipals(np)
        }
      )

    def withAuditor(auditor: LegalEntity.Key): Roster = ??? // refactor with above
  }

  object Roster {

    private def unsafe(
        principals: UnitPartition[LegalEntity.Key, Quantity],
        nonPrincipals: Role.NonPrincipal => NonEmptySet[LegalEntity.Key]
    ) = new Roster(principals, nonPrincipals) {}

    /** Pplits partition equally among `Principal`s - especially useful for singleton principals. */
    def fromRoles(rs: Map[Role, LegalEntity.Key]): Result[Roster] = ???

    def single(key: LegalEntity.Key): Roster =
      unsafe(
        principals = UnitPartition single key,
        nonPrincipals = _ => NonEmptySet one key
      )
  }

  type ValidRange = Interval.Closed[W.`100000100100L`.T, W.`999999999999L`.T]
  implicit def eqValidRangeHackHackHack: Eq[ValidRange] = Eq.fromUniversalEquals[ValidRange]

  /**
    * `Account`s consist of:
    * - a `Roster`: who gets to do what, and who are the beneficial owners.
    * - a `Folio` (list of instruments and their quantities)
    */
  sealed abstract case class Account(roster: Roster, folioKey: Folio.Key)

  /** */
  object Account extends WithRefinedKey[Long, ValidRange, Account] {

    def unsafe(roster: Roster, folioKey: Folio.Key) = new Account(roster, folioKey) {}

    // type Key = Long Refined ValidRange // remember you only get one free (value class) wrapper
    //
    // object Key {
    //   implicit def orderAccountId: cats.Order[Key] = cats.Order by { _.value }
    //   implicit lazy val fresh: Fresh[Key]          = Fresh.zeroBasedIncr[Long, ValidRange]
    // }

    def simple(le: LegalEntity.Key, f: Folio.Key): Account = unsafe(Roster single le, f)

    implicit def eq = Eq.fromUniversalEquals[Account]
  }
}
