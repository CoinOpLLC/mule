package io.deftrade
package model

import money._
import time.{ LocalDate }

import kves._
import OpaqueId.Fresh

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

import scala.collection.immutable.{ SortedMap, SortedSet }

/**
  * `Entities` model real world actors.
  * See Also: `model.Role`s.
  */
sealed trait Entity extends Product with Serializable {
  def name: Entity.VarChar
  def dtid: Entity.DTID
  def meta: Json
}

/** `Entity`s recognized by the system. */
object Entity extends IdPC[Long, Entity] {

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
    * Post Randomization SSN validation. I.e., cursory only.
    * https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs
    * https://www.ssa.gov/employer/randomization.html
    * https://www.ssa.gov/history/ssn/geocard.html
    */
  type SSN = String Refined refinements.SSN

  type EIN = String Refined refinements.EIN

  /** TODO fill in the placeholder... also: what else? */
  type AIN = String Refined refinements.AIN

  import refined.boolean.Or
  lazy val rf = refinements
  type DTID = String Refined Or[rf.SSN, rf.EIN] // adding another Or kills auto derivation

  import refined.auto._

  /** `People` are people. Also, `People` are `Entity`s.  */
  final case class Person(name: VarChar, ssn: SSN, dob: LocalDate, meta: Json) extends Entity {
    def dtid = ??? // ssn FIXME this keeps breaking, and then working again
  }

  /** `Corporation`s are `Entity`s too! */
  final case class Corporation(name: VarChar, ein: EIN, meta: Json) extends Entity {
    def dtid = ein
  }

  /** TODO flesh this concept out... minimum viable PM */
  final case class Algorithm(name: VarChar, /* ain: AIN, */ meta: Json) extends Entity {
    def dtid = ??? // ain FIXME
  }

  implicit def eq = Eq.fromUniversalEquals[Entity]
}

/**
  * Every `Entity` needs a `Role`.
  * There are a finite enumeration of `Role`s.
  */
sealed trait Role extends EnumEntry

/**
  * There is _always_ a distinguished `Role`, the `Principle`.
  * `Principle` `Entity` status may enforced by the type system using this.
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
    * The `Entity` which is the economic actor responsible for establishing the `Account`.
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
    * Also, simply, the `Entity`(s) whose names are listed on the `Account`,
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
    * `Entity`(s) with responsibility for, and authority over,
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
    * be suited to a risk manager function.
    */
  case object Auditor extends NonPrinciple

  /** The `findValues` macro collects all `value`s in the order written. */
  lazy val values: IndexedSeq[Role] = findValues

  /** this is just a hack to use `SortedSet`s etc */
  implicit def orderRoles: Order[Role] = Order by (_.entryName)

  lazy val nonPrinciples: IndexedSeq[NonPrinciple] = values collect { case np: NonPrinciple => np }
}

object Entities extends SimplePointInTimeRepository[cats.Id, Entity.Id, Entity]

/** package level API */
abstract class EntityAccountMapping[Q: Financial] extends Ledger[Q] { self =>

  /**  */
  type Entities = Entities.Table

  /**
    * Who does what. Or should. And shouldn't.
    */
  final case class Roster private (
      principles: Partition[Entity.Id, Quantity],
      nonPrinciples: NonPrinciple => NonEmptySet[Entity.Id]
  ) {
    import Cats._ // FIXME: DOESN'T HAVE ORDER! AND SHOULDN'T!
    val roles: NonEmptyMap[Role, NonEmptySet[Entity.Id]] =
      NonEmptyMap of (
        Role.Principle -> principles.keys,
        Role.nonPrinciples.map(np => (np, nonPrinciples(np))): _*
    )
  }

  object Roster {
    def single(eid: Entity.Id): Roster =
      Roster(
        principles = Partition single eid,
        nonPrinciples = _ => NonEmptySet one eid
      )
  }

  /**
    * `Vault` is a sum type used in its capacity as an obfuscator ;)
    * TODO: implement recursive traversal of a `Vault` as a `Foldable`
    * so you can treat as a container of `Folio.Id`s, basically.
    */
  sealed trait Vault
  object Vault {

    case class SubAccounts(subs: Set[Account.Id]) extends Vault
    case class Folio(fid: self.Folio.Id)          extends Vault

    def empty: Vault = SubAccounts(Set.empty) // idiom
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
  case class Account(roster: Roster, vault: Vault)
  object Account {
    import refinements.ValidRange
    type Id = Long Refined ValidRange
    object Id {
      implicit def orderAccountId: cats.Order[Id] = cats.Order by { _.value }
      implicit lazy val fresh: Fresh[Id]          = ???
      // FIXME need to revisit fresh anyway
      // Fresh(100000100100L, id => refined.refineV[ValidRange](id + 1L).fold(_ => ???, identity))
    }

    def empty(eid: Entity.Id) = Account(Roster single eid, Vault.empty)

    def simple(eid: Entity.Id, fid: Folio.Id) = Account(Roster single eid, Vault.Folio(fid))

    implicit def eq = Eq.fromUniversalEquals[Account]
  }

  import Account.Id._ // for implicits

  object Accounts extends SimplePointInTimeRepository[cats.Id, Account.Id, Account]
  type Accounts = Accounts.Table

  /**
    * TODO: this scheme is adequate for my purposes, but a ZK validation scheme which didn't expose
    * the `Account.Id`'s would be ideal.
    * another approoach would be to merkelize the `Roster` to expose only the relevant bits.
    */
  type AccountAuth           = (Account.Id, Signature)
  type FolioAuth             = (Folio.Id, AccountAuth)
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
      Validate.fromPredicate(predicate, t => s"$t is mos def NOT a valid SSN", SSN())

    private val regex = "^(\\d{3})-(\\d{2})-(\\d{4})$".r.pattern
    private val predicate: String => Boolean = s => {
      val matcher = regex matcher s
      matcher.find() && matcher.matches() && {
        import matcher.group
        val an      = group(1).toInt
        val gn      = group(2).toInt
        val sn      = group(3).toInt
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
