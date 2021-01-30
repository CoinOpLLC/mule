package io.deftrade
package model
package layers

import keyval._

import cats.implicits._
import cats.{ Eq, Show }
import cats.data.{ NonEmptyList, NonEmptySet }
import cats.derived.{ auto, semiauto }

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
  type IsAccountNo = Interval.Closed[100000100100108L, 999999999999999L]

  /** `Accounts` link the personal information of the account holders
    * with the financial data of the ledgers.
    */
  sealed abstract case class Account private (
      final val roster: Rosters.Id,
      final val positions: Portfolios.Id
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
        positions: Portfolios.Id
    ): Account =
      new Account(roster, positions) {}

    /** alt version FIXME: implement */
    def fromRoster[F[_]](roster: Roster): F[Accounts.Id] =
      // freshFolioKey, freshFolioKey, freshFolioKey
      ???

    implicit def accountEq: Eq[Account]     = { import auto.eq._; semiauto.eq }
    implicit def accountShow: Show[Account] = { import auto.show._; semiauto.show }
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
      case np: Role.NonPrincipal => nonPrincipals(np)
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

    implicit def valueShow: Show[Roster] = { import auto.show._; semiauto.show }
    implicit def valueEq: Eq[Roster]     = { import auto.eq._; semiauto.eq }

    private[deftrade] def apply(
        principals: UnitPartition[Parties.Key, Quantity],
        nonPrincipals: Map[Role.NonPrincipal, NonEmptySet[Parties.Key]]
    ): Roster =
      new Roster(principals, nonPrincipals) {}

    private[deftrade] def to(vs: NonEmptyList[RosterValue]): Roster = {
      import Role.{ NonPrincipal, Principal }
      val (xs, nonPrincipals) = vs.foldLeft(
        (List.empty[(Parties.Key, Quantity)], Map.empty[NonPrincipal, NonEmptySet[Parties.Key]])
      ) {
        case ((us, nps), value) =>
          value match {
            case RosterValue(p, Principal, Some(u)) => ((p, u) :: us, nps)
            case RosterValue(p, NonPrincipal(r), None) =>
              (us, nps.updated(r, (nps get r).fold(NonEmptySet one p)(_ add p)))
            case _ => ??? // (sic)
          }
      }
      val Right(principals) = UnitPartition exact (xs: _*)
      Roster(principals, nonPrincipals)
    }

    private[deftrade] def from(roster: Roster): NonEmptyList[RosterValue] = {
      val ps = roster.principals.kvs.toNel map {
        case (party, share) =>
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
    implicit def eq: Eq[Roster]     = { import auto.eq._; semiauto.eq }
    implicit def show: Show[Roster] = { import auto.show._; semiauto.show }
  }

  object Rosters extends ValueStores.Codec[Roster, RosterValue](Roster.from, Roster.to)

  import keyval.DtEnum

  import enumeratum.{ EnumEntry }

  import cats.implicits._

  /**
    * There are a finite enumeration of roles which [[Party]]s may take on with respect to
    * [[layers.Accounts.Account]]s.
    *
    * Contextual note: each `Role` is mapped to a [[Party]] via a [[layers.Accounts.Roster]].
    */
  sealed trait Role extends EnumEntry

  /**
    * Enumerated `Role`s.
    */
  object Role extends DtEnum[Role] {

    /**
      */
    sealed trait Principal extends Role

    /**
      * That [[Party]] which is the market participant
      * responsible for establishing the [[layers.Accounts.Account]].
      */
    case object Principal extends Principal {

      /**
        * A test for all `Role`s ''other than'' `Princple`.
        */
      def unapply(role: Role): Option[Principal] =
        if (role === this) this.some else none
    }

    @inline final def principal: Role = Principal

    /**
      */
    sealed trait NonPrincipal extends Role

    /**
      */
    object NonPrincipal {

      /**
        * Extractor for all `Role`s ''other than'' `Princple`.
        */
      def unapply(role: Role): Option[NonPrincipal] =
        role match {
          case Principal        => none
          case np: NonPrincipal => np.some
        }
    }

    /**
      * The primary delegate selected by a `Principal`.
      */
    case object Agent extends NonPrincipal

    /**
      * The primary delegate selected by the `Agent`.
      */
    case object Manager extends NonPrincipal

    /**
      * `Auditor`s are first class participants, with a package of rights and responsibilities.
      */
    case object Auditor extends NonPrincipal

    /** The `findValues` macro collects all `value`s in the order written. */
    lazy val values = findValues

    /**
      */
    lazy val nonPrincipals = (values collect { case NonPrincipal(np) => np }).toList
  }
}
