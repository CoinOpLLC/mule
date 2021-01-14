package io.deftrade
package model
package layers

import keyval._, refinements._

import cats.implicits._
import cats.{ Eq, Show }
import cats.data.{ NonEmptyList, NonEmptySet }
import cats.derived.{ auto, semi }

import eu.timepit.refined
import refined.api.Refined
import refined.cats._
import refined.numeric.Interval

import io.chrisdavenport.fuuid.FUUID

/** Models the relation of [[Party]]s to [[Folio]]s, including the definition of [[Role]]s.
  */
trait Accounts { self: Ledger with ModuleTypes =>

  /**
    */
  type Contact

  /**
    */
  val Contacts: ValueStores.SADT[Contact]

  /**
    */
  type IsAccountNo = Interval.Closed[100000100100108L, 999999999999999L]

  /** `Accounts` link the personal information of the account holders
    * with the financial data of the ledgers.
    */
  sealed abstract case class Account(
      roster: Rosters.Id,
      open: Folios.Key,
      escrowed: Folios.Key,
      expected: Folios.Key
  )

  /** Accounts are modelled as long lived entities that can evolve over time.
    */
  object Account {

    /**   TODO: domanin model issue: need to configure one of several ways of selecting UUIDs
      */
    protected[deftrade] def freshFolioKey = FUUID fromUUID java.util.UUID.randomUUID()

    /**
      */
    protected[deftrade] def apply(
        roster: Rosters.Id,
        open: Folios.Key,
        escrowed: Folios.Key,
        expected: Folios.Key
    ): Account =
      new Account(roster, open, escrowed, expected) {}

    /**
      */
    def fromRoster(roster: Rosters.Id): Account =
      apply(roster, freshFolioKey, freshFolioKey, freshFolioKey)

    /** alt version FIXME: implement */
    def fromRoster[F[_]](roster: Roster): F[Accounts.Id] =
      ???

    implicit def accountEq: Eq[Account] = { import auto.eq._; semi.eq }
    implicit def accountShow: Show[Account] = { import auto.show._; semi.show }
  }

  /**
    */
  object Accounts extends KeyValueStores.KV[Long Refined IsAccountNo, Account]

  /** Each [[Account]] is created with a [[Roster]].
    */
  sealed abstract case class Roster private (
      principals: UnitPartition[Parties.Key, Quantity],
      private val nps: Map[Role.NonPrincipal, NonEmptySet[Parties.Key]]
  ) {

    /**
      */
    lazy val nonPrincipals: Role.NonPrincipal => NonEmptySet[Parties.Key] = nps(_)

    /**
      */
    lazy val roles: Role => NonEmptySet[Parties.Key] = {
      case Role.Principal        => principals.keys
      case Role.NonPrincipal(np) => nonPrincipals(np)
    }

    /**
      */
    def withAgent(agent: Parties.Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Agent -> (NonEmptySet one agent))
      )

    /**
      */
    def withManager(manager: Parties.Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Manager -> (NonEmptySet one manager))
      )

    /**
      */
    def withAuditor(auditor: Parties.Key): Roster =
      Roster(
        principals = principals,
        nonPrincipals = nps + (Role.Auditor -> (NonEmptySet one auditor))
      )
  }

  /** TODO: revert this to a tuple.
    */
  case class RosterValue(party: Parties.Key, role: Role, stake: Option[Quantity])

  /** Creation patterns for account management teams.
    */
  object Roster {

    implicit def valueShow: Show[Roster] = { import auto.show._; semi.show }
    implicit def valueEq: Eq[Roster] = { import auto.eq._; semi.eq }

    private[deftrade] def apply(
        principals: UnitPartition[Parties.Key, Quantity],
        nonPrincipals: Map[Role.NonPrincipal, NonEmptySet[Parties.Key]]
    ): Roster =
      new Roster(principals, nonPrincipals) {}

    private[deftrade] def to(vs: NonEmptyList[RosterValue]): Roster = {
      import Role.{ NonPrincipal, Principal }
      val (xs, nonPrincipals) = vs.foldLeft(
        (List.empty[(Parties.Key, Quantity)], Map.empty[NonPrincipal, NonEmptySet[Parties.Key]])
      ) { case ((us, nps), value) =>
        value match {
          case RosterValue(p, Principal, Some(u)) => ((p, u) :: us, nps)
          case RosterValue(p, NonPrincipal(r), None) =>
            (us, nps.updated(r, (nps get r).fold(NonEmptySet one p)(_ add p)))
        }
      }
      val Right(principals) = UnitPartition exact (xs: _*)
      Roster(principals, nonPrincipals)
    }

    private[deftrade] def from(roster: Roster): NonEmptyList[RosterValue] = {
      val ps = roster.principals.kvs.toNel map { case (party, share) =>
        RosterValue(party, Role.principal, share.value.some)
      }
      val nps = for {
        role  <- Role.nonPrincipals
        party <- roster.nonPrincipals(role).toList
      } yield RosterValue(party, role, None)
      ps ++ nps
    }

    /** most general public creation method */
    def from(
        principals: UnitPartition[Parties.Key, Quantity],
        nonPrincipals: Map[Role.NonPrincipal, NonEmptySet[Parties.Key]]
    ) =
      Roster(
        principals,
        nonPrincipals withDefault { _ =>
          NonEmptySet(principals.keys.head, principals.keys.tail)
        }
      )

    /** By default, all share in [[Roster.nonPrincipals]] responsibilities equally,
      * regardless of their share of the principle pie.
      */
    def fromPrinciples(principals: UnitPartition[Parties.Key, Quantity]): Roster =
      from(principals, Map.empty)

    /**
      */
    def single(entity: Parties.Key): Roster =
      fromPrinciples(principals = UnitPartition single entity)

    /** Splits partition equally among [[Role.Principal]]s.
      */
    def equalSplitFrom(ps: Parties.Key*): Result[Roster] =
      for {
        slices <- UnitPartition fair [Parties.Key, Quantity] (ps: _*)
      } yield fromPrinciples(slices)

    /**
      */
    implicit def eq: Eq[Roster] = { import auto.eq._; semi.eq }
    implicit def show: Show[Roster] = { import auto.show._; semi.show }
  }

  object Rosters extends ValueStores.Codec[Roster, RosterValue](Roster.from, Roster.to)

  // import Roster._

  /** Models financial market participants.
    */
  sealed abstract class Party {
    def name: Label
    def taxNo: Tax.No
    def contact: Contacts.Id
  }

  /** Players that are recognized by the system (ours).
    */
  object Party {

    /**
      */
    def apply(name: Label, taxNo: Tax.No, contact: Contacts.Id): Party =
      taxNo match {
        case Tax.Ssn(ssn) => NaturalPerson(name, ssn, contact)
        case Tax.Ein(ein) => LegalEntity(name, ein, contact)
      }

    implicit def partyEq: Eq[Party] = { import auto.eq._; semi.eq }
    implicit def partyShow: Show[Party] = { import auto.show._; semi.show }
  }

  object Parties extends KeyValueStores.KV[FUUID, Party]

  /** `NaturalPerson`s are `Party`s.
    */
  sealed abstract case class NaturalPerson(
      name: Label,
      ssn: Tax.Ssn,
      contact: Contacts.Id
  ) extends Party {

    /**
      */
    final def taxNo: Tax.No = { import refined.auto._; ssn }
  }

  /**
    */
  object NaturalPerson {

    /**
      */
    def apply(name: Label, ssn: Tax.Ssn, contact: Contacts.Id): NaturalPerson =
      new NaturalPerson(name, ssn, contact) {}

    import refined.cats._

    implicit def naturalPersonEq: Eq[NaturalPerson] = { import auto.eq._; semi.eq }
    implicit def naturalPersonShow: Show[NaturalPerson] = { import auto.show._; semi.show }
  }

  /**
    */
  object NaturalPersons extends KeyValueStores.KV[FUUID, NaturalPerson]

  /**
    */
  sealed abstract case class LegalEntity private (
      name: Label,
      ein: Tax.Ein,
      contact: Contacts.Id
  ) extends Party {

    /**
      */
    final def taxNo: Tax.No = { import refined.auto._; ein }
  }

  /**
    */
  object LegalEntity {

    /**
      */
    def apply(name: Label, ein: Tax.Ein, contact: Contacts.Id): LegalEntity =
      new LegalEntity(name, ein, contact) {}

    import refined.cats._

    implicit def legalEntityEq: Eq[LegalEntity] = { import auto.eq._; semi.eq }
    implicit def legalEntityShow: Show[LegalEntity] = { import auto.show._; semi.show }
  }

  object LegalEntities extends KeyValueStores.KV[FUUID, LegalEntity]
}
