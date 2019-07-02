package io.deftrade
package model

import money._
import time.{ LocalDate }

import kves._

import repos._

import enumeratum._

import cats._
import cats.implicits._
import cats.data.{ NonEmptyMap, NonEmptySet }

import eu.timepit.refined
// import refined.{ cats => refinedCats, _ }
import refined.api.Refined
import refined.numeric._

import io.circe.Json

/**
  * `LegalEntities` model real world actors.
  * See Also: `model.Role`s.
  */
sealed trait LegalEntity extends Product with Serializable {
  def name: LegalEntity.VarChar
  def dtid: LegalEntity.DTID
  def meta: Json
}

/** `LegalEntity`s recognized by the system. */
object LegalEntity extends WithKey[Long, LegalEntity] {

  /**
    * RDB friendly `String`s that are born usable as is.
    * Defaults to Postgres, which is the shorter limit (126)
    */
  type VarChar = VarChar126

  /** Postgres optimizes strings less than this. */
  type VarChar126 = String Refined refinements.VarChar126

  /** Typical SQL */
  type VarChar255 = String Refined refinements.VarChar255

  /**
    * Post Randomization SSN validation: i.e., cursory only.
    * See also:
    * https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs
    * https://www.ssa.gov/employer/randomization.html
    * https://www.ssa.gov/history/ssn/geocard.html
    */
  type SSN = String Refined refinements.SSN

  /**
    * An `LegalEntity` represents a legal (e.g. corporate, or non-profit) body.
    * TODO: refine (no pun intended) the requirements on US EINs.
    * TODO: Internationalize with an ADT.
    */
  type EIN = String Refined refinements.EIN

  /**
    * An algo in my head told me to do this.
    */
  type AIN = String Refined refinements.AIN

  import refined.boolean.Or
  lazy val rf = refinements
  // type DTID = String Refined Or[rf.SSN, rf.EIN Or rf.AIN] // adding another Or kills auto derivation
  type DTID = String Refined Or[rf.SSN, rf.EIN] // adding another Or kills auto derivation

  import refined.auto._

  /**
    *`NaturalPerson`s are people. Also, `NaturalPerson`s are `LegalEntity`s.
    */
  final case class NaturalPerson(
      name: VarChar,
      ssn: SSN,
      dob: LocalDate,
      meta: Json
  ) extends LegalEntity {
    def dtid = ssn
  }

  /**
    * `Corporation`s are `LegalEntity`s too!
    */
  final case class Corporation(name: VarChar, ein: EIN, meta: Json) extends LegalEntity {
    def dtid = ein
  }

  /** TODO flesh this concept out... minimum viable PM */
  final case class Algorithm(name: VarChar, ain: AIN, meta: Json) extends LegalEntity {
    def dtid = ??? // ain FIXME
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

/**
  * Every `LegalEntity` needs a `Role`.
  * There are a finite enumeration of `Role`s.
  */
sealed trait Role extends EnumEntry

/**
  * There is _always_ a distinguished `Role`, the `Principle`.
  * `Principle` `LegalEntity` status may enforced by the type system using this.
  */
sealed trait Principle extends Role

/**
  * A type representing all `Role`s _other_ than `Princple`.
  */
sealed trait NonPrinciple extends Role

/**
  * Enumerated `Role`s.
  */
object Role extends Enum[Role] with CatsEnum[Role] {

  /**
    * The `LegalEntity` which is the economic actor responsible for establishing the `Account`.
    *
    * Semantics for `Principle` are conditioned on the status of account, for examples:
    * - beneficial owner for an asset
    * - responsible party for a liability
    * - shareholder for equity
    * - business unit chief for revenue and expenses
    */
  case object Principle extends Principle

  /**
    * The primary delegate selected by a `Principle`.
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

  lazy val nonPrinciples: IndexedSeq[NonPrinciple] = values collect { case np: NonPrinciple => np }
}

object LegalEntities extends SimplePointInTimeRepository[cats.Id, LegalEntity.Key, LegalEntity]

/** package level API */
abstract class EntityAccountMapping[Q: Financial] extends Ledger[Q] { self =>

  /**  */
  type LegalEntities = LegalEntities.Table

  /**
    * Who does what. Or should. And shouldn't.
    */
  final case class Roster private (
      principles: UnitPartition[LegalEntity.Key, Quantity],
      nonPrinciples: NonPrinciple => NonEmptySet[LegalEntity.Key]
  ) {
    lazy val roles: NonEmptyMap[Role, NonEmptySet[LegalEntity.Key]] =
      NonEmptyMap of (
        Role.Principle -> principles.keys,
        Role.nonPrinciples.map(np => (np, nonPrinciples(np))): _*
    )

    def withAgent(agent: LegalEntity.Key): Roster =
      copy(nonPrinciples = {
        case Role.Agent => NonEmptySet one agent
        case np         => nonPrinciples(np)
      })

    def withAuditor(auditor: LegalEntity.Key): Roster = ??? // refactor with above
  }

  object Roster {

    def fromRoles(rs: Map[Role, LegalEntity.Key]): Result[Roster] = ???

    def single(key: LegalEntity.Key): Roster =
      Roster(
        principles = UnitPartition single key,
        nonPrinciples = _ => NonEmptySet one key
      )
  }

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
  case class Account(roster: Roster, folioKey: Folio.Key)
  object Account {
    import refinements.ValidRange
    implicit def eqValidRangeHackHackHack: Eq[ValidRange] = Eq.fromUniversalEquals[ValidRange]
    type Key = Long Refined ValidRange // remember you only get one free (value class) wrapper
    object Key {
      implicit def orderAccountId: cats.Order[Key] = cats.Order by { _.value }
      implicit lazy val fresh: Fresh[Key]          = Fresh.zeroBasedIncr[Long, ValidRange]
    }

    def freshFolioKey: Folio.Key = ??? // FIXME: this is now repo-dependent

    def empty(key: LegalEntity.Key) = simple(key, freshFolioKey)

    def simple(le: LegalEntity.Key, f: Folio.Key) = Account(Roster single le, f)

    implicit def eq = Eq.fromUniversalEquals[Account]
  }

  import Account.Key._ // for implicits

  object Accounts extends SimplePointInTimeRepository[cats.Id, Account.Key, Account]
  type Accounts = Accounts.Table

  /**
    * TODO: this scheme is adequate for my purposes, but a ZK validation scheme which didn't expose
    * the `Account.Key`'s would be ideal.
    * another approoach would be to merkelize the `Roster` to expose only the relevant bits.
    */
  type AccountAuth           = (Account.Key, Signature)
  type FolioAuth             = (Folio.Key, AccountAuth)
  type FolioAuths            = (FolioAuth, FolioAuth)
  type AuthorizedTransaction = (Transaction, FolioAuths)
}

object refinements {

  import refined.W
  import refined.boolean.{ And }
  import refined.collection.{ MaxSize, NonEmpty }
  import refined.string.{ MatchesRegex, Trimmed }

  import refined.api.Validate

  type ValidRange = Interval.Closed[W.`100000100100L`.T, W.`999999999999L`.T]

  final case class SSN private ()
  object SSN {

    implicit def ssnValidate: Validate.Plain[String, SSN] =
      Validate.fromPredicate(predicate, t => s"$t is certainly not a valid SSN", SSN())

    private val regex = "^(\\d{3})-(\\d{2})-(\\d{4})$".r.pattern
    private val predicate: String => Boolean = s => {
      val matcher = regex matcher s
      matcher.find() && matcher.matches() && {
        import matcher.group
        val an      = group(1).toInt
        def gn      = group(2).toInt
        def sn      = group(3).toInt
        def checkAn = 0 < an && an != 666 /* sic */ && an < 900
        checkAn && 0 < gn && 0 < sn
      }
    }
  }

  type EIN = EIN.MRx
  object EIN {
    type Pattern = W.`"[0-9]{2}-[0-9]{7}"`.T
    type MRx     = MatchesRegex[Pattern]
  }

  type AIN = AIN.MRx
  object AIN {
    type Pattern = W.`"666-[A-F]{2}-[0-9]{6}"`.T
    type MRx     = MatchesRegex[Pattern]
  }

  type VarChar126 = NonEmpty And Trimmed And MaxSize[`W`.`126`.T]
  type VarChar255 = NonEmpty And Trimmed And MaxSize[`W`.`255`.T]
}
