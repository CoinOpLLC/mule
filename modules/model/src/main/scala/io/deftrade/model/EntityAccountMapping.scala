package io.deftrade
package model

import money._, time._, keyval._, repos._, refinements._

import enumeratum._

import cats._
import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
import refined.W
import refined.api.Refined
import refined.boolean.Or
import refined.numeric._
// import refined.{ cats => refinedCats, _ }

import io.circe.Json

/**
  * There are a finite enumeration of [[Roles]].
  * Every `Role` is mapped to a [[LegalEntity]] via a [[Roster]].
  */
sealed trait Role extends EnumEntry

/**
  * Enumerated `Role`s.
  */
object Role extends Enum[Role] with CatsEnum[Role] {

  /**
    * There is _always_ a distinguished `Role`, the `Principal`.
    */
  sealed trait Principal extends Role

  /**
    * A type representing all `Role`s _other_ than `Princple`.
    */
  sealed trait NonPrinciple extends Role

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
  case object Agent extends NonPrinciple

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
  case object Manager extends NonPrinciple

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
  case object Auditor extends NonPrinciple

  /** The `findValues` macro collects all `value`s in the order written. */
  lazy val values: IndexedSeq[Role] = findValues

  lazy val nonPrincipals: IndexedSeq[NonPrinciple] = values collect { case np: NonPrinciple => np }
}

/**
  * `LegalEntities` model real world actors.
  * See Also: `model.Role`s.
  */
sealed trait LegalEntity extends Serializable {
  def name: VarChar
  def dtid: LegalEntity.Dtid
  def meta: Json
}

/** `LegalEntity`s recognized by the system. */
object LegalEntity extends WithKey[Long, LegalEntity] {

  import refined.auto._

  /** Domain Typed Id */
  type Dtid = String Refined (IsSsn Or IsEin)

  /**
    *`NaturalPerson`s are people. Also, `NaturalPerson`s are `LegalEntity`s.
    */
  final case class NaturalPerson(
      name: VarChar,
      ssn: Ssn,
      dob: LocalDate,
      meta: Json
  ) extends LegalEntity {
    def dtid = ssn
  }

  /**
    * `Corporation`s are `LegalEntity`s too!
    */
  final case class Corporation(name: VarChar, ein: Ein, meta: Json) extends LegalEntity {
    def dtid = ein
  }

  implicit def eqEntity = Eq.fromUniversalEquals[LegalEntity]

  implicit def showEntity = Show.show[LegalEntity] {
    _.toString // this can evolve!
  }

  /**
    * The default `Fresh` instance placed in scope by `LegalEntity` is zeroBasedIncr.
    * This is one possible policy decision; there are others. TODO: revisit.
    */
  implicit def freshEntityKey: Fresh[Key] = Fresh.zeroBasedIncr
}

/** package level API */
abstract class EntityAccountMapping[Q: Financial] extends Ledger[Q] { self =>

  /**
    * Each [[Account]] is created with a `Roster`, specifying the beneficial owners and their crew.
    */
  sealed abstract case class Roster private (
      principals: UnitPartition[LegalEntity.Key, Quantity],
      nonPrincipals: Role.NonPrinciple => NonEmptySet[LegalEntity.Key]
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
        nonPrincipals: Role.NonPrinciple => NonEmptySet[LegalEntity.Key]
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
    * = a `Folio` (list of instruments and their quantities), OR a list of sub `Account`s.
    * = (Tree structures only. No loops, no reconvergence at all permitted.)
    *
    * How composition of `Roster`s and `Vault::SubAccount`s works:
    * - conjuction.
    * - that's it (you're welcome.)
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

  /**
    * TODO: this scheme seems fit to current purposes,
    * but a ZK validation scheme which didn't expose the `Account.Key`'s would be ideal.
    *
    * Another approoach might be to merkelize the `Roster` to expose only the relevant bits.
    */
  type AccountAuth           = (Account.Key, Signature)
  type FolioAuth             = (Folio.Key, AccountAuth)
  type FolioAuths            = (FolioAuth, FolioAuth)
  type AuthorizedTransaction = (Transaction, FolioAuths)

  /**
  FIXME this needs to move to test / sample client
    */
  type LegalEntities = LegalEntities.Table

  object LegalEntities extends SimplePointInTimeRepository[cats.Id, LegalEntity.Key, LegalEntity]

  import Account.Key._ // for implicits

  implicit val freshAccountNo: Fresh[Account.Key] = ??? // to compile duh
  object Accounts extends SimplePointInTimeRepository[cats.Id, Account.Key, Account]
  type Accounts = Accounts.Table

}
