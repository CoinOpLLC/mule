package io.deftrade
package model

import money._, time._, keyval._, refinements._

import enumeratum._

import cats._
import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.boolean.Or

import io.circe.Json

/**
  * Models real world actors under the aegis of, and registered with, real world
  * justistictions.
  *
  * Privacy by design: [[LegalEntity.TaxId]]'s are not used as keys.
  * See Also: `model.Role`s.
  */
sealed trait LegalEntity extends Serializable {
  def name: VarChar
  def taxId: LegalEntity.TaxId
  def meta: Json
}

/**
  * `LegalEntity`s recognized by the system.
  */
object LegalEntity extends WithOpaqueKey[Int, LegalEntity] {

  type IsTaxId = IsSsn Or IsEin
  type TaxId   = String Refined IsTaxId

  import refined.auto._

  /**
    *`NaturalPerson`s are people. Also, `NaturalPerson`s are `LegalEntity`s.
    */
  final case class NaturalPerson(
      name: VarChar,
      ssn: Ssn,
      dob: LocalDate,
      meta: Json
  ) extends LegalEntity {
    def taxId = ssn
  }

  /**
    * `Corporation`s are `LegalEntity`s too!
    */
  final case class Corporation(name: VarChar, ein: Ein, meta: Json) extends LegalEntity {
    def taxId = ein
  }

  implicit def eqEntity = Eq.fromUniversalEquals[LegalEntity]

  implicit def showEntity = Show.show[LegalEntity] {
    _.toString // this can evolve!
  }

  /**
    * There are a finite enumeration of roles which [[LegalEntity]]s may take on with respect to
    * [[Account]]s.
    *
    * Every `Role` is mapped to a [[LegalEntity]] via a [[Roster]].
    */
  sealed trait Role extends EnumEntry

  /**
    * Enumerated `Role`s.
    * Note: this enum is not dependant on the type parameter and so may be hoisted up.
    * Actually a primitive, in other words.
    */
  object Role extends Enum[Role] with CatsEnum[Role] {

    /** */
    object NonPrincipal {

      /**
        * A test for all `Role`s _other_ than `Princple`.
        */
      def unapply(role: Role): Option[Role] = role match {
        case Principal                        => none
        case np @ (Agent | Manager | Auditor) => np.some
      }
    }

    /**
      *
      * There is _always_ a distinguished [[Role]], the `Principal`.
      *
      * The [[LegalEntity]] which is the market participant
      * responsible for establishing the [[Account]].
      *
      * Semantics for `Principal` are conditioned on the status of account, for examples:
      * - beneficial owner for an asset
      * - responsible party for a liability
      * - shareholder for equity
      * - business unit chief for revenue and expenses
      */
    case object Principal extends Role

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
    case object Agent extends Role

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
    case object Manager extends Role

    /**
      * `Auditor`s are first class participants, with a package of rights and responsibilities.
      *
      * There are a finite enumeration of [[Role]]s.
      * Every `Role` is mapped to a [[LegalEntity]] via a [[Roster]]
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
    case object Auditor extends Role

    /** The `findValues` macro collects all `value`s in the order written. */
    lazy val values: IndexedSeq[Role] = findValues

    /** */
    lazy val nonPrincipals: IndexedSeq[Role] = values collect { case NonPrincipal(np) => np }
  }

}
