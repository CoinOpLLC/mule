package io.deftrade
package model

import money._
import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.W
import refined.numeric._
import refined.boolean.{ And }
import refined.collection.{ MaxSize }
import refined.string.{ MatchesRegex, Trimmed }
import refined.api.Validate

import shapeless.syntax.singleton._

/**
  * A palette of domain specific refined types. Prêt à porter, as it were.
  */
object refinements {

  /**
    * RDB friendly `String`s that are born usable as is.
    * Defaults to Postgres, which is the shorter limit (126)
    */
  type IsVarChar = IsVarChar126
  type VarChar   = String Refined IsVarChar

  /** Postgres optimizes strings less than this. */
  type IsVarChar126 = Trimmed And MaxSize[W.`126`.T]
  type VarChar126   = String Refined IsVarChar126

  /** Typical SQL */
  type IsVarChar255 = Trimmed And MaxSize[W.`255`.T]
  type VarChar255   = String Refined IsVarChar255

  final val MatchesRxSsn = """\d{3}-\d{2}-\d{4}""".witness
  type MatchesRxSsn = MatchesRxSsn.T

  /**
    * Post "Randomization" SSN validation: i.e., cursory only.
    * See also:
    * https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs
    * https://www.ssa.gov/employer/randomization.html
    * https://www.ssa.gov/history/ssn/geocard.html
    */
  sealed abstract case class CheckedSsn private ()
  object CheckedSsn {

    lazy val instance: CheckedSsn = new CheckedSsn() {}

    implicit def ssnValidate: Validate.Plain[String, CheckedSsn] =
      Validate.fromPredicate(predicate, t => s"$t is certainly not a valid IsSsn", instance)

    private val predicate: String => Boolean = s => {
      scala.util.Try {
        val an :: gn :: sn :: Nil = (s split '-' map (_.toInt)).toList
        def checkAn               = 0 < an && an != 666 /* sic */ && an < 900
        checkAn && 0 < gn && 0 < sn
      } getOrElse false
    }
  }

  type IsSsn = MatchesRxSsn And CheckedSsn
  type Ssn   = String Refined IsSsn

  /**
    * An `LegalEntity` represents a legal (e.g. corporate, or non-profit) body.
    * TODO: refine (pun intended) the requirements on US EINs.
    * TODO: Internationalize with an ADT.
    */
  val IsEin = "[0-9]{2}-[0-9]{7}".witness
  type IsEin = MatchesRegex[IsEin.T]
  type Ein   = String Refined IsEin

  /**
    * An ISIN is a twelve character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  val MatchesRxIsin = """[A-Z]{2}[A-Z0-9]{9}[0-9]""".witness
  type MatchesRxIsin = MatchesRegex[MatchesRxIsin.T]

  sealed abstract case class CheckedIsin()
  object CheckedIsin {

    lazy val instance: CheckedIsin = new CheckedIsin() {}

    implicit def isinValidate: Validate.Plain[String, CheckedIsin] =
      Validate fromPredicate (predicate, t => s"$t is not Luhny", instance)

    private def predicate(isin: String): Boolean =
      scala.util.Try {

        val digits = for {
          c <- isin
          d <- Character.digit(c, 36).toString
        } yield d.asDigit

        def luhn(digit: Int, idx: Int): Int =
          if (idx % 2 === 0) digit else (digit * 2) / 10 + (digit * 2) % 10

        val check = for ((d, i) <- digits.reverse.zipWithIndex) yield luhn(d, i)

        check.sum % 10 === 0

      } getOrElse false
  }

  type IsIsin = MatchesRxIsin And CheckedIsin
  type Isin   = String Refined IsIsin

  /**
    * A CUSIP is a nine character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  sealed abstract case class CheckedCusip()
  object CheckedCusip {

    lazy val instance: CheckedCusip = new CheckedCusip() {}

    implicit def cusipValidate: Validate.Plain[String, CheckedCusip] =
      Validate fromPredicate (predicate, t => s"$t is not legit", instance)

    private def predicate(isin: String): Boolean =
      scala.util.Try {

        /** FIXME flesh this out */
        true

      } getOrElse false
  }

  val MatchesRxCusip = """[0-9]{3}[0-9A-Z]{3}[0-9]{3}""".witness
  type MatchesRxCusip = MatchesRegex[MatchesRxCusip.T]

  type IsCusip = MatchesRxCusip And CheckedCusip
  type Cusip   = String Refined IsCusip
}
