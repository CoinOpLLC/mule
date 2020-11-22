package io.deftrade
package model
package layers

import keyval._, refinements._

import cats.implicits._
import cats.{ Eq, Show }
import cats.data.{ NonEmptyList, NonEmptySet }
import cats.derived.{ auto, semi }

import eu.timepit.refined
import refined.cats._
import refined.numeric.Interval

import io.chrisdavenport.fuuid.FUUID

/**
  * Models the relation of [[Party]]s to [[Folio]]s, including the definition of [[Role]]s.
  */
trait Accounts { self: Ledger with ModuleTypes =>

  /**
    */
  type Contact

  /** #FIXME implement
    */
  implicit def contactShow: Show[Contact] = ???

  /**
    */
  val Contact: WithId.Aux[SADT.Aux[Contact]]

  /**
    */
  final lazy val Contacts = ValueStore(Contact, ValueStore.Param.V).deriveV[SADT.Aux[Contact]]

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
      roster: Roster.Id,
      open: Folio.Key,
      escrowed: Folio.Key,
      expected: Folio.Key
  )

  /**
    * Accounts are modelled as long lived entities that can evolve over time.
    */
  object Account extends WithRefinedKey[Long, IsAccountNo, Account] {

    /**
      *   TODO: domanin model issue: need to configure one of several ways of selecting UUIDs
      */
    protected[deftrade] def freshFolioKey = Folio.Key.random

    /**
      */
    protected[deftrade] def apply(
        roster: Roster.Id,
        open: Folio.Key,
        escrowed: Folio.Key,
        expected: Folio.Key
    ): Account =
      new Account(roster, open, escrowed, expected) {}

    /**
      */
    def fromRoster(roster: Roster.Id): Account =
      apply(roster, freshFolioKey, freshFolioKey, freshFolioKey)

    /** alt version FIXME: implement */
    def fromRoster[F[_]](roster: Roster): F[Account.Id] =
      ???

    implicit def accountEq: Eq[Account] = { import auto.eq._; semi.eq }
    implicit def accountShow: Show[Account] = { import auto.show._; semi.show }
  }

  /**
    */
  final lazy val Accounts = KeyValueStore(Account, KeyValueStore.Param.V).deriveV[Account]

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

    /**
      */
    lazy val nonPrincipals: Role.NonPrincipal => NonEmptySet[Party.Key] = nps(_)

    /**
      */
    lazy val roles: Role => NonEmptySet[Party.Key] = {
      case Role.Principal        => principals.keys
      case Role.NonPrincipal(np) => nonPrincipals(np)
    }

    /**
      */
    def withAgent(agent: Party.Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Agent -> (NonEmptySet one agent))
      )

    /**
      */
    def withManager(manager: Party.Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Manager -> (NonEmptySet one manager))
      )

    /**
      */
    def withAuditor(auditor: Party.Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Auditor -> (NonEmptySet one auditor))
      )
  }

  /** TODO: revert this to a tuple.
    */
  case class RosterValue(party: Party.Key, role: Role, stake: Option[Quantity])

  /**
    * Creation patterns for account management teams.
    */
  object Roster extends WithId.Aux[RosterValue] {

    implicit def valueShow: Show[Value] = { import auto.show._; semi.show }
    implicit def valueEq: Eq[Value] = { import auto.eq._; semi.eq }

    private[deftrade] def apply(
        principals: UnitPartition[Party.Key, Quantity],
        nonPrincipals: Map[Role.NonPrincipal, NonEmptySet[Party.Key]]
    ): Roster =
      new Roster(principals, nonPrincipals) {}

    private[deftrade] def to(vs: NonEmptyList[Value]): Roster = {
      import Role.{ NonPrincipal, Principal }
      val (xs, nonPrincipals) = vs.foldLeft(
        (List.empty[(Party.Key, Quantity)], Map.empty[NonPrincipal, NonEmptySet[Party.Key]])
      ) {
        case ((us, nps), value) =>
          value match {
            case RosterValue(p, Principal, Some(u)) => ((p, u) :: us, nps)
            case RosterValue(p, NonPrincipal(r), None) =>
              (us, nps.updated(r, (nps get r).fold(NonEmptySet one p)(_ add p)))
          }
      }
      val Right(principals) = UnitPartition exact (xs: _*)
      Roster(principals, nonPrincipals)
    }

    private[deftrade] def from(roster: Roster): NonEmptyList[Value] = {
      val ps = roster.principals.kvs.toNel map {
        case (party, share) => RosterValue(party, Role.principal, share.value.some)
      }
      val nps = for {
        role  <- Role.nonPrincipals
        party <- roster.nonPrincipals(role).toList
      } yield RosterValue(party, role, None)
      ps ++ nps
    }

    /** most general public creation method */
    def from(
        principals: UnitPartition[Party.Key, Quantity],
        nonPrincipals: Map[Role.NonPrincipal, NonEmptySet[Party.Key]]
    ) =
      Roster(
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

    /**
      */
    def single(entity: Party.Key): Roster =
      fromPrinciples(principals = UnitPartition single entity)

    /**
      * Splits partition equally among [[Role.Principal]]s.
      */
    def equalSplitFrom(ps: Party.Key*): Result[Roster] =
      for {
        slices <- UnitPartition fair [Party.Key, Quantity] (ps: _*)
      } yield fromPrinciples(slices)

    /**
      */
    implicit def eq: Eq[Roster] = { import auto.eq._; semi.eq }
    implicit def show: Show[Roster] = { import auto.show._; semi.show }
  }

  import Roster._

  /**
    */
  final lazy val Rosters =
    ValueStore(Roster, ValueStore.Param.Aux(Roster.from, Roster.to))
      .deriveV[RosterValue]

  /**
    * Models financial market participants.
    *
    * Presumed real world actors under the aegis of, and registered with, real world
    * juristictions.
    *
    * Small step towards privacy by design: `Tax.No`'s are not used as `Key`s.
    */
  sealed trait Party {
    def name: Label
    def taxNo: Tax.No
    def contact: Contact.Id
  }

  /**
    * Players that are recognized by the system (ours).
    */
  object Party extends WithFuuidKey[Party] {

    /**
      */
    def apply(name: Label, taxNo: Tax.No, contact: Contact.Id): Party =
      taxNo match {
        case Tax.Ssn(ssn) => NaturalPerson(name, ssn, contact)
        case Tax.Ein(ein) => LegalEntity(name, ein, contact)
      }

    implicit def partyEq: Eq[Party] = { import auto.eq._; semi.eq }
    implicit def partyShow: Show[Party] = { import auto.show._; semi.show }
  }

  /**
    * `NaturalPerson`s are `Party`s.
    */
  sealed abstract case class NaturalPerson(
      name: Label,
      ssn: Tax.Ssn,
      contact: Contact.Id
  ) extends Party {

    /**
      */
    final def taxNo: Tax.No = { import refined.auto._; ssn }
  }

  /**
    */
  object NaturalPerson extends WithFuuidKey[NaturalPerson] {

    /**
      */
    def apply(name: Label, ssn: Tax.Ssn, contact: Contact.Id): NaturalPerson =
      new NaturalPerson(name, ssn, contact) {}

    import refined.cats._

    implicit def naturalPersonEq: Eq[NaturalPerson] = { import auto.eq._; semi.eq }
    implicit def naturalPersonShow: Show[NaturalPerson] = { import auto.show._; semi.show }
  }

  /**
    */
  final lazy val NaturalPersons =
    KeyValueStore(NaturalPerson, KeyValueStore.Param.V).deriveV[NaturalPerson]

  /**
    */
  sealed abstract case class LegalEntity private (
      name: Label,
      ein: Tax.Ein,
      contact: Contact.Id
  ) extends Party {

    /**
      */
    final def taxNo: Tax.No = { import refined.auto._; ein }
  }

  /**
    */
  object LegalEntity extends WithFuuidKey[LegalEntity] {

    /**
      */
    def apply(name: Label, ein: Tax.Ein, contact: Contact.Id): LegalEntity =
      new LegalEntity(name, ein, contact) {}

    import refined.cats._

    implicit def legalEntityEq: Eq[LegalEntity] = { import auto.eq._; semi.eq }
    implicit def legalEntityShow: Show[LegalEntity] = { import auto.show._; semi.show }
  }

  /**
    */
  final lazy val LegalEntities =
    KeyValueStore(LegalEntity, KeyValueStore.Param.V).deriveV[LegalEntity]
}
