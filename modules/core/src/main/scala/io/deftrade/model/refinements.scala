package io.deftrade
package model

import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.W
import refined.boolean.{ And, Or }
import refined.collection.{ MaxSize, NonEmpty }
import refined.numeric.Positive
import refined.string.{ MatchesRegex, Trimmed }
import refined.api.Validate

import shapeless.syntax.singleton._

import scala.util.Try
import scala.language.existentials

/**
  * A palette of domain specific refined types.
  *
  * Prêt à porter, batteries included, your metaphore here (civil pull requests considered).
  */
object refinements {

  import refined.auto._

  private def alpha = "[A-Z]"
  private def num   = "[0-9]"

  private def witnesseth[T](t: T)                          = { val tv = t; tv.witness }
  private def patRep(pat: String)(n: Int Refined Positive) = s"""$pat{$n}""" |> witnesseth

  val Alpha2 = patRep(alpha)(2)
  type Alpha2 = String Refined MatchesRegex[Alpha2.T]

  val Alpha3 = patRep(alpha)(3)
  type Alpha3 = String Refined MatchesRegex[Alpha3.T]

  val Num3 = patRep(num)(3)
  type Num3 = String Refined MatchesRegex[Num3.T]

  val Mic = """[A-Z]{3,4}""".witness
  type Mic = String Refined MatchesRegex[Mic.T] // market venue

  /**
    * RDB friendly `String`s that are born usable as is.
    * Defaults to Postgres, which is the shorter limit (126)
    */
  type IsVarChar = IsVarChar126
  type VarChar   = String Refined IsVarChar
  object VarChar {
    import refined.auto._

    val empty: VarChar = ""

  }
  type NonEmptyVarChar = String Refined (IsVarChar And NonEmpty)

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

  /** */
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
    * An `LegalEntity` represents a legal (eg corporate, or non-profit) body.
    * TODO: refine (pun intended) the requirements on US EINs.
    * TODO: Internationalize with an ADT.
    */
  val IsEin = """\d{2}-\d{7}""".witness
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

    /**
      * * TODO need to add country checks,
      * and break them out into a separate function
      *
      *   - green-light only a predefined list of juristictions for registered securities
      *   - two-letter code mappings reserved for "users" are adopted by deftrade:
      *   - ZZ: unregistered securities with house-issued numbers.
      *   - XB: Interactive Brokers `ConId` number
      *   - the other 25 mappings in X[A-Z] are reserved for use facing other brokers' apis.
      */
    private def predicate(isin: String): Boolean = failsafe {

      val digits = for {
        c <- isin
        d <- Character.digit(c, 36).toString
      } yield d.asDigit

      val check = for ((d, i) <- digits.reverse.zipWithIndex) yield luhn(d, i)

      check.sum % 10 === 0

    }
  }

  /** Psin: Pseudo Isin: matches regex, but uses the 9 digit body for proprietary mappings. */
  sealed abstract case class CheckedPsin()

  /** */
  object CheckedPsin {

    lazy val instance: CheckedPsin = new CheckedPsin() {}

    implicit def isinValidate: Validate.Plain[String, CheckedPsin] =
      Validate fromPredicate (predicate, t => s"$t is not Luhny", instance)

    private def predicate(isin: String): Boolean = failsafe {

      val digits = for {
        c <- isin
        d <- Character.digit(c, 36).toString
      } yield d.asDigit

      val check = for ((d, i) <- digits.reverse.zipWithIndex) yield luhn(d, i)

      check.sum % 10 === 0
    }
  }

  type IsIsin = MatchesRxIsin And CheckedIsin
  type Isin   = String Refined IsIsin

  type IsPsin = MatchesRxIsin And CheckedPsin // sic
  type Psin   = String Refined IsPsin

  /**
    * How deftrade canonicalizes securities identifiers:
    * Use `Isin`s where non-isin uses use reserved country codes: {X*, ZZ}
    *
    * Universal Security Identifying Number: Usin
    */
  type IsUsin = IsIsin Or IsPsin
  type Usin   = String Refined IsUsin

  object Usin {

    /**
      * the least we can do
      */
    def from(s: String): Result[Usin] = ???

    def fromIsin(isin: Isin): Isin    = ???
    def fromCusip(cusip: Cusip): Isin = ???
    // def fromSedol(sedol: Sedol): Isin = ???

    def fromUnreg(unreg: Unreg): Psin = ???
    def fromIbrk(ibrk: Ibrk): Psin    = ???

    def toIsin(usin: Usin): Result[Isin]   = ???
    def toCusip(usin: Usin): Result[Cusip] = ???
    // def toSedol(usin: Usin): Result[Sedol] = ???

    def toUnreg(usin: Usin): Result[Unreg] = ???
    def toIbrk(usin: Usin): Result[Ibrk]   = ???
  }

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
      failsafe {

        // FIXME flesh this out

        true

      }
  }

  val MatchesRxCusip = """[0-9]{3}[0-9A-Z]{3}[0-9]{3}""".witness
  type MatchesRxCusip = MatchesRegex[MatchesRxCusip.T]

  type IsCusip = MatchesRxCusip And CheckedCusip
  type Cusip   = String Refined IsCusip

  /**
    * `Ibrk` identifiers represent **Interactive Brokers**
    * `ConId`'s
    */
  val MatchesRxIbrk = """\d{8}\d?""".witness // 8 or 9 char, all numbers (evidently)
  type MatchesRxIbrk = MatchesRegex[MatchesRxIbrk.T]
  type IsIbrk        = MatchesRxIbrk
  type Ibrk          = String Refined IsIbrk

  /**
    * `Unreg` identifiers represent unregistered securities, numbered by the firm.
    */
  val MatchesRxUnreg = """\d{8}\d?""".witness // 8 or 9 char, all numbers (evidently)
  type MatchesRxUnreg = MatchesRegex[MatchesRxUnreg.T]
  type IsUnreg        = MatchesRxUnreg
  type Unreg          = String Refined IsUnreg

  //------

  // https://en.wikipedia.org/wiki/Luhn_algorithm
  private def luhn(digit: Int, idx: Int): Int =
    if (idx % 2 === 0) digit else (digit * 2) / 10 + (digit * 2) % 10

  private def failsafe(predcomp: => Boolean): Boolean = Try(predcomp).fold(_ => false, identity)

}
