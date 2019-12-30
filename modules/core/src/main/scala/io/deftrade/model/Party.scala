/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.deftrade
package model

import time._, keyval._, refinements._

import enumeratum.{ CatsEnum, Enum, EnumEntry }

import cats.implicits._
import cats.{ Eq, Show }

import eu.timepit.refined
import refined.api.Refined
import refined.api.Validate
import refined.boolean.{ And, Or }
import refined.string.{ MatchesRegex }
import refined.collection.{ NonEmpty }

import shapeless.syntax.singleton._

import io.circe.Json

/**
  * Models real world actors under the aegis of, and registered with, real world
  * justistictions.
  *
  * Small step towards privacy by design: [[Party.TaxId]]'s are not used as keys.
  */
sealed trait Party extends Serializable {
  def name: Label
  def taxId: Party.TaxId
  def meta: Json
}

/**
  * Players that are recognized by the system (ours).
  */
object Party extends WithOpaqueKey[Int, Party] {

  import refined.auto._

  /** */
  final val MatchesRxSsn = """\d{3}-\d{2}-\d{4}""".witness

  /** */
  type MatchesRxSsn = MatchesRxSsn.T

  /** */
  type IsSsn = MatchesRxSsn And CheckedSsn

  /** */
  type Ssn = String Refined IsSsn

  /**
    * Post "Randomization" SSN validation: i.e., cursory only.
    * See also:
    * https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs
    * https://www.ssa.gov/employer/randomization.html
    * https://www.ssa.gov/history/ssn/geocard.html
    */
  sealed abstract case class CheckedSsn private ()

  /** */
  object CheckedSsn {

    /** */
    lazy val instance: CheckedSsn = new CheckedSsn() {}

    /** */
    implicit def ssnValidate: Validate.Plain[String, CheckedSsn] =
      Validate.fromPredicate(predicate, t => s"$t is certainly not a valid IsSsn", instance)

    /** */
    private val predicate: String => Boolean = s => {
      scala.util.Try {
        val an :: gn :: sn :: Nil = (s split '-' map (_.toInt)).toList
        def checkAn               = 0 < an && an != 666 /* sic */ && an < 900
        checkAn && 0 < gn && 0 < sn
      } getOrElse false
    }
  }

  /**
    * An `Party` represents a legal (eg corporate, or non-profit) body.
    * TODO: refine (pun intended) the requirements on US EINs.
    * TODO: Internationalize with an ADT?!
    * TODO: '''DTC''' ''Legal Entity Identifier '' `LEI` definition (issuing party for public secs)
    */
  val IsEin = """\d{2}-\d{7}""".witness

  /** */
  type IsEin = MatchesRegex[IsEin.T]

  /** */
  type Ein = String Refined IsEin

  /** */
  type IsTaxId = IsSsn Or IsEin

  /** */
  type TaxId = String Refined IsTaxId

  /**
    *`NaturalPerson`s are people. Also, `NaturalPerson`s are `Party`s.
    */
  final case class NaturalPerson(
      name: Label,
      ssn: Ssn,
      dob: LocalDate,
      meta: Json
  ) extends Party {
    def taxId = ssn
  }

  /**
    * `Corporation`s are `Party`s too!
    */
  final case class Corporation(name: Label, ein: Ein, meta: Json) extends Party {
    def taxId = ein
  }

  /** */
  implicit def eqEntity = Eq.fromUniversalEquals[Party]

  /** */
  implicit def showEntity = Show.show[Party] {
    _.toString // this can evolve!
  }

  /**
    * There are a finite enumeration of roles which [[Party]]s may take on with respect to
    * [[layers.Accounts.Account]]s.
    *
    * Every `Role` is mapped to a [[Party]] via a [[layers.Accounts.Roster]].
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
      * The [[Party]] which is the market participant
      * responsible for establishing the [[layers.Accounts.Account]].
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
      * Also, simply, the `Party`(s) whose names are listed on the `Account`,
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
      * `Party`(s) with responsibility for, and authority over,
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
      * Every `Role` is mapped to a [[Party]] via a [[layers.Accounts.Roster]]
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
      * be suited eg to a Risk Manager, operating in the context of a hedge fund.
      */
    case object Auditor extends Role

    /** The `findValues` macro collects all `value`s in the order written. */
    lazy val values = findValues

    /** FIXME this is just a hack to use `SortedSet`s etc
      * it is almost certainly wrong to do this, but why?
      */
    implicit val orderInstance: cats.Order[Role] = cats.Order by (_.entryName)

    /** */
    lazy val nonPrincipals = values collect { case NonPrincipal(np) => np }
  }
}

/** */
sealed abstract case class CountryCode(
    alpha2: Alpha2,
    alpha3: Alpha3,
    countryCode: Num3,
    name: String Refined NonEmpty,
    regionCode: Option[Num3],
    subRegionCode: Option[Num3],
    intermediateRegionCode: Option[Num3],
    region: VarChar,
    subRegion: VarChar,
    intermediateRegion: VarChar,
    // iso_3166_2: NonEmptyVarChar,
)

/** */
object CountryCode extends WithOpaqueKey[Int, CountryCode] {

  def regions: Map[Num3, VarChar]             = ???
  def intermediateRegions: Map[Num3, VarChar] = ???
  def subRegions: Map[Num3, VarChar]          = ???

  def apply(alpha2: Alpha2,
            alpha3: Alpha3,
            countryCode: Num3,
            name: String Refined NonEmpty,
            regionCode: Option[Num3],
            subRegionCode: Option[Num3],
            intermediateRegionCode: Option[Num3]): CountryCode =
    new CountryCode(
      alpha2,
      alpha3,
      countryCode,
      name,
      regionCode,
      subRegionCode,
      intermediateRegionCode,
      region = (regions get countryCode).fold(VarChar.empty)(identity),
      subRegion = (regions get countryCode).fold(VarChar.empty)(identity),
      intermediateRegion = (regions get countryCode).fold(VarChar.empty)(identity),
      // s"ISO 3166-2:$alpha2",
    ) {}
}