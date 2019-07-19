package io.deftrade
package model

import money._, keyval._

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
    * Each [[Account]] is created with a `Roster`, specifying the beneficial owners and their crew.
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
  object Account extends WithRefinedKey[Long, AccountNo, Account] {

    def unsafe(roster: Roster, folioKey: Folio.Key) = new Account(roster, folioKey) {}

    def simple(le: LegalEntity.Key, f: Folio.Key): Account = unsafe(Roster single le, f)

    implicit def eq = Eq.fromUniversalEquals[Account]
  }
}
