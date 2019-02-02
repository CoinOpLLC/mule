package io.deftrade
package model

import time._
import time.implicits._

import money._
import Currency.USD

import opaqueid._
import OpaqueId.Fresh

import repos._

import reference.InstrumentIdentifier

import enumeratum._

import cats.{ Eq, Foldable, Hash, Invariant, Monad, Monoid, MonoidK, Order }
import cats.kernel.CommutativeGroup
import cats.data.Kleisli
import cats.implicits._
import feralcats.instances._

import eu.timepit.refined
import refined.{ cats => refinedCats, _ }
import refined.api.Refined
import refined.numeric._
import refined.string._
import refined.auto._

import io.circe.Json

import scala.language.higherKinds


sealed trait Role extends EnumEntry
sealed trait NonPrinciple extends Role
object Role extends Enum[Role] {

  /**
    * The `Entity` which is the economic actor responsible for establishing the `Account`.
    *
    * Semantics for `Principle` are conditioned on the status of account, for examples:
    * - beneficial owner for an asset
    * - responsible party for a liability
    * - shareholder for equity
    * - business unit chief for revenue and expenses
    */
  case object Principle extends Role

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
    * `Regulator`s are first class entities, each with a package of rights and responsibilities
    * which is situation and juristiction specific.
    *
    * Practically, what this means is that `Regulator`s will have a (possibly limited) view
    * into the state of the `Ledger`,
    * and (possibly) the ability to block the settlement of `Transaction`s to the `Ledger`
    * or even intitiate `Transaction`s.
    *
    * Actions of the `Regulator` may include the publishing of specific summaries of its views
    * into the `Ledger` to establish common knowledge for participants in `Ledger` `Transaction`s.
    *
    * N.B.: the `Regulator` need not be a governmental entity; in particular this role might
    * be suited to a risk manager function.
    */
  case object Regulator extends NonPrinciple

  /** The `findValues` macro collects all `value`s in the order written. */
  lazy val values: IndexedSeq[Role] = findValues

  lazy val nonPrinciples: Set[NonPrinciple] =
    (Set.empty[Role] ++ values - Principle).asInstanceOf[Set[NonPriniple]]

  implicit lazy val eq = Eq.fromUniversalEquals[Role]
}

/** `Entities`: People, Corporations */
sealed trait Entity extends Product with Serializable { def meta: Json }
object Entity extends IdPC[Long, Entity] {

  // FIXME: check out the existing `Refine`ments in the library.
  object SSN {
    type Pattern = W.`"[0-9]{3}-[0-9]{2}-[0-9]{4}"`.T
    type Regex   = MatchesRegex[Pattern]
  }
  type SSN = String Refined SSN.Regex

  object EIN {
    type Pattern = W.`"[0-9]{2}-[0-9]{7}"`.T
    type Regex   = MatchesRegex[Pattern]
  }
  type EIN = String Refined EIN.Regex

  final case class Person(val ssn: SSN, val meta: Json)      extends Entity
  final case class Corporation(val ein: EIN, val meta: Json) extends Entity

  implicit def eq = Eq.fromUniversalEquals[Entity]
}

object Entities extends SimplePointInTimeRepository[cats.Id, Entity.Id, Entity]

/** package level API */
abstract class EntityAccountMapping[MA: Financial, Q: Financial] extends Ledger[MA, Q] { self =>


    /**  */
    type Entities = Entities.Table

    /**
      * Note that Roster encodes the notion of a `CapitalKey` _per `Role`_!
      * The meaning of this `Partition` will depend upon the `Role`.
      * For the beneficial owner(s) of the assets in question the meaning is obvious.
      * For a `Manager`, the `Partition` could encode their voting rights – which may differ from
      * their ownership rights.
      */
    final case class Partition[K] private (val kvs: Map[K, Quantity]) extends AnyVal
    object Partition {

      import QF._

      def apply[K: Order](shares: (K, Quantity)*): Result[Partition[K]] = {
        val s = shares.toList
        val x = s.map(_._2).fold(zero)(plus(_, _)
        if ( x == one) && (s forall { case (_, q) => validate(q) })) s.toMap.asRight
        else Fail(s"Partition: invalid creation parms: $x != 1: $shares")
      }

      def apply[K: Order](totalShares: Long)(ps: (K, Long)*): Partition[K] = ps.toList.toMap map {
        case (k, l) => (k, div(one, fromLong(totalShares)))
      }

      /**
        * The portion of the total accorded to the identified `Entity`.
        */
      def validate(q: Quantity): Boolean          = zero <= q && q <= one
      def validate(total: Long)(n: Long): Boolean = 0L <= n && n <= total
      // def apply[K](shares: Share[K]*): Either[String, Partition[K]] = {
      //   val p = accumulate(shares.toList)
      //   p.values.toList foldMap identity match {
      //     case sum if sum === QF.one => p.asRight
      //     case badsum                => s"Partition doesn't add up: $badsum != 1.0".asLeft
      //   }
      // }
      // def single[K: Hash](k: K): Partition[K] = Map.empty + Share.single[K](k)

      def proRata[K: Order](capTable: Map[K, Quantity]): Partition[K] = {
        val totalShares = (capTable map { case (_, shares) => shares }).toList |> QF.sum
        capTable map { case (k, mySlice) => (k, div(mySlice, totalShares)) }
      }

      def pariPassu[K: Hash](ks: Set[K]): Partition[K] = {
        val n = ks.size
        (ks.toList zip List.fill(n)(div(one, fromInt(n)))).toMap
      }

      object Single {

        def unapply[K](p: Partition[K]): Option[K] = p.toList match {
          case (k, Quantity.One) :: Nil => k.some
          case _                        => none
        }
      }
    }

    /**
      * Who does what. Or should. And shouldn't.
      */
    final case class Roster(principles: Partition[Entity.Id], roles: NonPrinciple => Set[Entity.Id])

    object Roster {
      def apply(eid: Entity.Id): Roster =
        Map.empty + (Role.Principle -> Set(eid))
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
      type ValidRange = Interval.Closed[W.`100000100100L`.T, W.`999999999999L`.T]
      type Id         = Long Refined ValidRange
      object Id {
        implicit def orderAccountId: cats.Order[Id] = cats.Order by { _.value }
        implicit lazy val fresh: Fresh[Id] =
          Fresh(100000100100L, id => refineV[ValidRange](id + 1L).fold(_ => ???, identity))
      }

      def empty(eid: Entity.Id) = Account(Roster(eid), Vault.empty)

      def simple(eid: Entity.Id, fid: Folio.Id) = Account(Roster(eid), Vault.Folio(fid))

      implicit def eq = Eq.fromUniversalEquals[Account]
    }

    import Account.Id._ // for implicits

    object Accounts extends SimplePointInTimeRepository[cats.Id, Account.Id, Account]
    type Accounts = Accounts.Table

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
      * TODO: this scheme is adequate for my purposes, but a ZK validation scheme which didn't expose
      * the `Account.Id`'s would be ideal.
      * another approoach would be to merkelize the `Roster` to expose only the relevant bits.
      */
    type AccountAuth           = (Account.Id, Signature)
    type FolioAuth             = (Folio.Id, AccountAuth)
    type FolioAuths            = (FolioAuth, FolioAuth)
    type AuthorizedTransaction = (Transaction, FolioAuths)
}
