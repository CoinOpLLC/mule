package io.deftrade
package model

import money._, time._, keyval._, refinements._

import cats._
import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.boolean.Or

import io.circe.Json

/**
  * This value class models real world actors under the aegis of, and registered with, real world
  * justistictions.
  *
  * Privacy by design: [[TaxId]]'s are not used as keys.
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

}
