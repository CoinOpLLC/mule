package io.deftrade
package model
package layers

import keyval._

import cats.implicits._
import cats.{ Eq, Show }
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
import refined.W
import refined.numeric.Interval
import refined.cats.refTypeOrder

trait Accounts { self: Ledger with ModuleTypes =>

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
    *   - implies that all non-Princple [[Role]]s must be filled with at least one [[Party]].
    *
    */
  sealed abstract case class Roster private (
      principals: UnitPartition[Party.Key, Quantity],
      nonPrincipals: Role => NonEmptySet[Party.Key]
  ) {
    import Party.Key

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
    def withAuditor(auditor: Party.Key): Roster =
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

    import Party.Key

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

  /**
    * Accounts are modelled as long lived entities that can evolve over time.
    * FIXME: `Roster` needs json to fit in a column. Normalize?
    */
  object Account extends WithRefinedKey[Long, IsAccountNo, Account] {

    /** */
    def apply(roster: Roster, folio: Folio.Key): Account = new Account(roster, folio) {}

    /** */
    def single(entity: Party.Key, folio: Folio.Key) = Account(Roster single entity, folio)

    /** TODO: use kittens? Why or why not? */
    implicit def eq = Eq.fromUniversalEquals[Account]

    /** TODO: placeholder */
    implicit def show = Show.fromToString[Account]
  }
}
