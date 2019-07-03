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

  /**
    * Post Randomization IsSSN validation: i.e., cursory only.
    * See also:
    * https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs
    * https://www.ssa.gov/employer/randomization.html
    * https://www.ssa.gov/history/ssn/geocard.html
    */
  final case class IsSsn private ()
  object IsSsn {

    implicit def ssnValidate: Validate.Plain[String, IsSsn] =
      Validate.fromPredicate(predicate, t => s"$t is certainly not a valid IsSsn", IsSsn())

    private val regex = """^(\d{3})-(\d{2})-(\d{4})$""".r.pattern
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
  type Ssn = String Refined IsSsn

  /**
    * An `LegalEntity` represents a legal (e.g. corporate, or non-profit) body.
    * TODO: refine (no pun intended) the requirements on US EINs.
    * TODO: Internationalize with an ADT.
    */
  object IsEin extends RxPredicate("[0-9]{2}-[0-9]{7}".narrow)
  type IsEin = IsEin.MRx
  type Ein   = String Refined IsEin

  /**
    * All algos carry the mark of the beast. (It is known.)
    */
  type IsAin = IsAin.MRx
  object IsAin extends RxPredicate("666-[A-F]{2}-[0-9]{6}".narrow)
  type Ain = String Refined IsAin

  private[model] sealed abstract class RxPredicate[P](p: P) {
    type Pattern = P // =:= p.T
    val Pattern = p
    final type MRx = MatchesRegex[Pattern]
  }

  /**
    * An ISIN is a twelve character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  type IsIsin = MatchesRxIsin And LuhnyIsin
  type Isin   = String Refined IsIsin

  val isinRx = """[A-Z]{2}[A-Z0-9]{9}\d""".witness
  type MatchesRxIsin = MatchesRegex[isinRx.T]

  sealed abstract case class LuhnyIsin()
  object LuhnyIsin {

    implicit def isinValidate: Validate.Plain[String, LuhnyIsin] =
      Validate fromPredicate (predicate, t => s"$t is not Luhny", new LuhnyIsin() {})

    private def predicate(isin: String): Boolean =
      scala.util.Try {

        val digits = for {
          c <- isin.trim.toUpperCase
          d <- Character.digit(c, 36).toString
        } yield d.asDigit

        val checksum = digits.reverse.zipWithIndex.foldLeft(0) {
          case (sum, (digit, i)) =>
            if (i % 2 == 0)
              sum + digit
            else
              sum + (digit * 2) / 10 + (digit * 2) % 10
        }
        checksum % 10 === 0

      } getOrElse false
  }
}
object IsinScratch {
  val isins = List(
    "US0378331005",
    "US0373831005",
    "U50378331005",
    "US03378331005",
    "AU0000XVGZA3",
    "AU0000VXGZA3",
    "FR0000988040"
  )
}
