package io.deftrade
package model
package layers

import keyval._

import cats.implicits._
import cats.{ Eq, Show }
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
import refined.numeric.Interval

/**
  * Models the relation of [[Party]]s to [[Folio]]s, including the definition of [[Role]]s.
  */
trait Accounts { self: Ledger with ModuleTypes =>

  /**
    * Each [[Account]] is created with a [[Roster]], specifying the beneficial owners
    * and their crew.
    *
    * Note: `nonPrincipal` [[Party]]s must be specified for each [[Role.NonPrincipal]]
    */
  sealed abstract case class Roster private (
      principals: UnitPartition[Party.Key, Quantity],
      private val nps: Role.NonPrincipal Map NonEmptySet[Party.Key]
  ) {

    import Party.Key

    /** */
    lazy val nonPrincipals: Role.NonPrincipal => NonEmptySet[Party.Key] = nps(_)

    /** */
    lazy val roles: Role => NonEmptySet[Key] = {
      case Role.Principal        => principals.keys
      case Role.NonPrincipal(np) => nonPrincipals(np)
    }

    /** */
    def withAgent(agent: Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Agent -> (NonEmptySet one agent))
      )

    /** */
    def withManager(manager: Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Manager -> (NonEmptySet one manager))
      )

    /** */
    def withAuditor(auditor: Party.Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Auditor -> (NonEmptySet one auditor))
      )
  }

  /**
    * Creation patterns for account management teams.
    */
  object Roster extends WithKey.Aux[Account.Key, (Party.Key, Role, Option[Quantity])] {

    val Key = Account.Key

    private[deftrade] def apply(
        principals: UnitPartition[Party.Key, Quantity],
        nonPrincipals: Map[Role.NonPrincipal, NonEmptySet[Party.Key]]
    ): Roster =
      new Roster(principals, nonPrincipals) {}

    /**
      * By default, all share in [[Roster.nonPrincipals]] responsibilities equally,
      * regardless of their share of the principle pie
      */
    def forPrinciples(principals: UnitPartition[Party.Key, Quantity]): Roster =
      apply(
        principals,
        nonPrincipals = Map.empty withDefault { _ =>
          NonEmptySet(principals.keys.head, principals.keys.tail)
        }
      )

    /**
      * Splits partition equally among [[Role.Principal]]s.
      */
    def equalSplitFrom(rs: Map[Role, Key]): Result[Roster] = ???

    /** */
    def single(entity: Party.Key): Roster =
      apply(
        principals = UnitPartition single entity,
        nonPrincipals = Map.empty withDefault { _ =>
          NonEmptySet one entity
        }
      )
  }

  /**
    * Predicate defining a very conventional looking account numbering scheme.
    */
  type IsAccountNo = Interval.Closed[100000100108L, 999999999999L]

  /**
    * `Account`s consist of:
    *   - a `Folio` of settled [[Ledger.Transaction]]s
    *   - a `Folio` of `Transaction`s not yet settled
    *
    *  A [[Roster]] - who gets to do what, and who are the beneficial owners - is linked to the
    * `Account` via its own table, indexed by the [[Account.Key]].
    */
  sealed abstract case class Account(
      // roster: Roster,
      settled: Folio.Key,
      unsettled: Folio.Key,
  )

  /**
    * Accounts are modelled as long lived entities that can evolve over time.
    * FIXME: `Roster` needs json to fit in a column. Normalize?
    */
  object Account extends WithRefinedKey[Long, IsAccountNo, Account] {

    /** FIXME implement - just need to settle on a way of selecting UUIDs for ppl */
    private def freshFolioKey: Folio.Key = ???

    /** */
    private[deftrade] def apply(s: Folio.Key, u: Folio.Key): Account =
      new Account(s, u) {}

    /** FIXME: unfvck and implement */
    def fromRoster(roster: Roster) = ???

    /** FIXME: unfvck and implement */
    def singleParty(party: Party.Key) = ???

    /** TODO: use kittens? Why or why not? */
    implicit def eq = Eq.fromUniversalEquals[Account]

    /** TODO: placeholder */
    implicit def show = Show.fromToString[Account]
  }
}
