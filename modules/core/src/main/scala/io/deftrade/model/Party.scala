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

import keyval._, refinements._

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semi }
import cats.effect.{ Sync }

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined, Validate }
import refined.boolean.{ And, Or }
import refined.string.{ MatchesRegex }

import fs2.Stream

// import shapeless.syntax.singleton._

/**
  */
object Tax {

  /**
    */
  final val MatchesRxSsn = """\d{3}-\d{2}-\d{4}"""

  /**
    */
  type MatchesRxSsn = MatchesRxSsn.type

  /**
    * Post [[https://www.ssa.gov/employer/randomization.html Randomization]]
    * SSN validation: i.e., cursory only.
    *
    * [[https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs SSN validation]]
    * [[https://www.ssa.gov/history/ssn/geocard.html SSN geocard]]
    */
  type IsSsn = MatchesRxSsn And CheckedSsn

  /**
    */
  type Ssn = String Refined IsSsn

  object Ssn {
    def unapply(no: No)(implicit v: Validate[String, IsSsn]): Option[Ssn] =
      refineV[IsSsn](no.value).toOption
  }

  /**
    */
  sealed abstract case class CheckedSsn private ()

  /**
    */
  object CheckedSsn {

    /**
      */
    lazy val instance: CheckedSsn = new CheckedSsn() {}

    /**
      */
    implicit def ssnValidate: Validate.Plain[String, CheckedSsn] =
      Validate.fromPredicate(predicate, t => s"$t is certainly not a valid IsSsn", instance)

    /**
      */
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
    *
    * TODO: '''DTC''' ''Legal Entity Identifier '' `LEI` definition (issuing party for public secs)
    */
  final val IsEin = """\d{2}-\d{7}"""

  /**
    */
  final type IsEin = MatchesRegex[IsEin.type]

  /**
    */
  final type Ein = String Refined IsEin

  /**
    */
  object Ein {
    def unapply(no: No)(implicit v: Validate[String, IsEin]): Option[Ein] =
      refineV[IsEin](no.value).toOption
  }

  /**
    */
  type IsNo = IsSsn Or IsEin

  /**
    */
  type No = String Refined IsNo
}

/**
  * Models financial market participants.
  *
  * Presumed real world actors under the aegis of, and registered with, real world
  * juristictions.
  *
  * Small step towards privacy by design: `Tax.No`'s are not used as `Key`s.
  */
sealed trait Party extends Product with Serializable {
  def name: Label
  def taxNo: Tax.No
  def contact: Contact.Id
}

/**
  * Players that are recognized by the system (ours).
  */
object Party extends WithFuuidKey[Party] {

  /** TODO: this is sketchy and probably not needed */
  def apply(name: Label, taxNo: Tax.No, meta: Meta.Id)(implicit
                                                       vssn: Validate[String, Tax.IsSsn],
                                                       vein: Validate[String, Tax.IsEin]) =
    taxNo match {
      case Tax.Ssn(ssn) => NaturalPerson(name, ssn, meta)
      case Tax.Ein(ein) => LegalEntity(name, ein, meta)
    }

  implicit def partyEq: Eq[Party]     = { import auto.eq._; semi.eq }
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
  final def taxNo: Tax.No = {
    import refined.auto._
    ssn
  }
}

/**
  */
object NaturalPerson extends WithFuuidKey[NaturalPerson] {

  /**
    */
  def apply(name: Label, ssn: Tax.Ssn, contact: Contact.Id): NaturalPerson =
    new NaturalPerson(name, ssn, contact) {}

  /** FIXME: `Accounts.contacts` dependency is unmapped */
  def mk[F[_]: Sync](ssn: Tax.Ssn, contact: Contact): Stream[F, Result[NaturalPerson]] =
    // extract name from contact before persisting it
    ???

  import refined.cats._

  implicit def naturalPersonEq: Eq[NaturalPerson]     = { import auto.eq._; semi.eq }
  implicit def naturalPersonShow: Show[NaturalPerson] = { import auto.show._; semi.show }
}

/**
  */
sealed abstract case class LegalEntity private (
    name: Label,
    ein: Tax.Ein,
    contact: Contact.Id
) extends Party {

  /**
    */
  final def taxNo: Tax.No = {
    import refined.auto._
    ein
  }
}

/**
  */
object LegalEntity extends WithFuuidKey[LegalEntity] {

  /**
    */
  def apply(name: Label, ein: Tax.Ein, contact: Contact.Id): LegalEntity =
    new LegalEntity(name, ein, contact) {}

  import refined.cats._

  implicit def legalEntityEq: Eq[LegalEntity]     = { import auto.eq._; semi.eq }
  implicit def legalEntityShow: Show[LegalEntity] = { import auto.show._; semi.show }
}
