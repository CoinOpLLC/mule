package io.deftade
package model

import io.deftrade.time.LocalDate
import io.deftrade.opaqueid.IdPC

import cats._
import cats.implicits._

import eu.timepit.refined
import io.circe.Json

import refined.W
import refined.api.Refined
import refined.boolean.{ And, Or }
import refined.collection.{ MaxSize, NonEmpty }
import refined.string.{ MatchesRegex, Trimmed }
import refined.auto._

/** `Entities` model real world actors.
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
  type VarChar126 = String Refined NonEmpty And Trimmed And MaxSize[`W`.`126`.T]

  /** Typical SQL */
  type VarChar255 = String Refined NonEmpty And Trimmed And MaxSize[`W`.`255`.T]

  /**
    * Post Randomization SSN validation. I.e., cursory only.
    * https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs
    * https://www.ssa.gov/employer/randomization.html
    * https://www.ssa.gov/history/ssn/geocard.html
    */
  type SSN = String Refined refinements.SSN

  type EIN = String Refined refinements.EIN

  /** TODO fill in the placeholder... also: what else? */
  // type AIN = String // Refined refinements.AIN

  // type DTID = String Refined Or[refinements.SSN, Or[refinements.EIN, refinements.AIN]]
  type DTID = String Refined Or[refinements.SSN, refinements.EIN]

  /** `People` are people. Also, `People` are `Entity`s.  */
  final case class Person(name: VarChar, ssn: SSN, dob: LocalDate, meta: Json) extends Entity {
    def dtid = ssn
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

object refinements {

  import refined.api.Validate

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
}
