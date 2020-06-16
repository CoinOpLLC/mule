package io.deftrade
package model
package layers

import keyval._

import cats.implicits._
import cats.{ Eq, Show }
import cats.data.{ NonEmptySet }
import cats.derived.{ auto, semi }

import eu.timepit.refined
import refined.numeric.Interval

/**
  * Models the relation of [[Party]]s to [[Folio]]s, including the definition of [[Role]]s.
  */
trait Accounts { self: Ledger with ModuleTypes =>

  /**
    * Predicate defining a very conventional looking account numbering scheme.
    */
  type IsAccountNo = Interval.Closed[100000100100108L, 999999999999999L]

  /**
    * `Account`s consist of:
    *   - a `Folio` of settled [[Ledger.Transaction]]s
    *   - a `Folio` of `Transaction`s not yet settled
    *
    *  A [[Roster]] - specifies a mapping of [[Party]]s to [[Role]]s,
    * and who are the beneficial owners - is linked to the
    * `Account` via its own table, indexed by the [[Account.Key]].
    */
  sealed abstract case class Account(
      settled: Folio.Key,
      unsettled: Folio.Key,
  )

  /**
    * Accounts are modelled as long lived entities that can evolve over time.
    */
  object Account extends WithRefinedKey[Long, IsAccountNo, Account] {

    /**
      *   TODO: domanin model issue: need to configure one of several ways of selecting UUIDs
      */
    protected[deftrade] def freshFolioKey = Folio.Key.random

    /** */
    protected[deftrade] def apply(s: Folio.Key, u: Folio.Key): Account =
      new Account(s, u) {}

    /** */
    protected[deftrade] def empty = apply(freshFolioKey, freshFolioKey)

    implicit def accountEq: Eq[Account]     = { import auto.eq._; semi.eq }
    implicit def accountShow: Show[Account] = { import auto.show._; semi.show }
  }

  /**
    * Each [[Account]] is created with a [[Roster]], specifying the beneficial owners
    * and their crew.
    *
    * Note: [[Party]]s '''must''' be specified for each [[Role.NonPrincipal non principal role]]
    */
  sealed abstract case class Roster private (
      principals: UnitPartition[Party.Key, Quantity],
      private val nps: Map[Role.NonPrincipal, NonEmptySet[Party.Key]]
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

    private[deftrade] def fromValues(vs: List[Value]): Roster = {
      import Role.{ NonPrincipal, Principal }
      val (xs, nonPrincipals) = vs.foldLeft(
        (List.empty[(Party.Key, Quantity)], Map.empty[NonPrincipal, NonEmptySet[Party.Key]])
      ) {
        case ((us, nps), value) =>
          value match {
            case (p, Principal, Some(u)) => ((p, u) :: us, nps)
            case (p, NonPrincipal(r), None) =>
              (us, nps updated (r, (nps get r).fold(NonEmptySet one p)(_ add p)))
          }
      }
      val Right(principals) = UnitPartition exact (xs: _*)
      Roster(principals, nonPrincipals)
    }

    private[deftrade] def toValues(roster: Roster): List[Value] = {
      val ps = roster.principals.kvs.toSortedMap.toList.foldLeft(List.empty[Value]) {
        case (vs, (party, share)) => (party, (Role.Principal: Role), share.value.some) :: vs
      }
      val nps = for {
        role  <- Role.nonPrincipals
        party <- roster.nonPrincipals(role).toList
      } yield (party, role, None)
      ps ++ nps
    }

    /** most general public creation method */
    def from(
        principals: UnitPartition[Party.Key, Quantity],
        nonPrincipals: Map[Role.NonPrincipal, NonEmptySet[Party.Key]]
    ): Roster =
      apply(
        principals,
        nonPrincipals withDefault { _ =>
          NonEmptySet(principals.keys.head, principals.keys.tail)
        }
      )

    /**
      * By default, all share in [[Roster.nonPrincipals]] responsibilities equally,
      * regardless of their share of the principle pie.
      */
    def fromPrinciples(principals: UnitPartition[Party.Key, Quantity]): Roster =
      from(principals, Map.empty)

    /** */
    def single(entity: Party.Key): Roster =
      fromPrinciples(principals = UnitPartition single entity)

    /**
      * Splits partition equally among [[Role.Principal]]s.
      */
    def equalSplitFrom(ps: Party.Key*): Result[Roster] =
      for {
        slices <- UnitPartition fair [Party.Key, Quantity] (ps: _*)
      } yield fromPrinciples(slices)

    /** */
    implicit def eq: Eq[Roster]     = ??? // { import auto.eq._; semi.eq }
    implicit def show: Show[Roster] = ??? // { import auto.show._; semi.show }
  }
}
