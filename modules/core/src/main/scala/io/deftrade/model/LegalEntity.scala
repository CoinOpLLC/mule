package io.deftrade
package model

import money._, time._, keyval._, refinements._

import cats._
import cats.implicits._

import eu.timepit.refined
import refined.boolean.Or
// import refined.{ cats => refinedCats, _ }

import io.circe.Json

/**
  * `LegalEntities` model real world actors.
  * See Also: `model.Role`s.
  */
sealed trait LegalEntity extends Serializable {
  def name: VarChar
  def key: LegalEntity.Key
  def meta: Json
}

/** `LegalEntity`s recognized by the system. */
object LegalEntity extends WithRefinedKey[String, IsSsn Or IsEin, LegalEntity] {

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
    def key = ssn
  }

  /**
    * `Corporation`s are `LegalEntity`s too!
    */
  final case class Corporation(name: VarChar, ein: Ein, meta: Json) extends LegalEntity {
    def key = ein
  }

  implicit def eqEntity = Eq.fromUniversalEquals[LegalEntity]

  implicit def showEntity = Show.show[LegalEntity] {
    _.toString // this can evolve!
  }

}
