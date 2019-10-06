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

// import cats._
import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.api.Validate
import refined.boolean.{ And, Or }
import refined.collection.NonEmpty
import refined.string.{ MatchesRegex }

import shapeless.syntax.singleton._

import scala.util.Try

/** */
package object reference {

  /** */
  final val MatchesRxSsn = """\d{3}-\d{2}-\d{4}""".witness

  /** */
  type MatchesRxSsn = MatchesRxSsn.T

  /** */
  type IsSsn = MatchesRxSsn And CheckedSsn

  /** */
  type Ssn = String Refined IsSsn

  /**
    * An `LegalEntity` represents a legal (eg corporate, or non-profit) body.
    * TODO: refine (pun intended) the requirements on US EINs.
    * TODO: Internationalize with an ADT.
    */
  val IsEin = """\d{2}-\d{7}""".witness

  /** */
  type IsEin = MatchesRegex[IsEin.T]

  /** */
  type Ein = String Refined IsEin

  /** */
  val Mic = """[A-Z]{3,4}""".witness

  /** */
  type Mic = String Refined MatchesRegex[Mic.T] // market venue

  /**
    * An ISIN is a twelve character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  val MatchesRxIsin = """[A-Z]{2}[A-Z0-9]{9}[0-9]""".witness

  /** */
  type MatchesRxIsin = MatchesRegex[MatchesRxIsin.T]

  /** */
  type IsIsin = MatchesRxIsin And CheckedIsin

  /** */
  type Isin = String Refined IsIsin

  /** */
  type IsPsin = MatchesRxIsin And CheckedPsin // sic
  /** */
  type Psin = String Refined IsPsin

  /**
    * How deftrade canonicalizes securities identifiers:
    * Use `Isin`s where non-isin uses use reserved country codes: {X*, ZZ}
    *
    * Universal Security Identifying Number: Usin
    */
  type IsUsin = IsIsin Or IsPsin

  /** */
  type Usin = String Refined IsUsin

  /** */
  object Usin {

    /**
      * the least we can do
      */
    def from(s: String): Result[Usin] = ???

    /** */
    def fromIsin(isin: Isin): Isin = ???

    /** */
    def fromCusip(cusip: Cusip): Isin = ???
    // def fromSedol(sedol: Sedol): Isin = ???

    /** */
    def fromUnreg(unreg: Unreg): Psin = ???

    /** */
    def fromIbrk(ibrk: Ibrk): Psin = ???

    /** */
    def toIsin(usin: Usin): Result[Isin] = ???

    /** */
    def toCusip(usin: Usin): Result[Cusip] = ???
    // def toSedol(usin: Usin): Result[Sedol] = ???

    /** */
    def toUnreg(usin: Usin): Result[Unreg] = ???

    /** */
    def toIbrk(usin: Usin): Result[Ibrk] = ???
  }

  /** */
  val MatchesRxCusip = """[0-9]{3}[0-9A-Z]{3}[0-9]{3}""".witness

  /** */
  type MatchesRxCusip = MatchesRegex[MatchesRxCusip.T]

  /** */
  type IsCusip = MatchesRxCusip And CheckedCusip

  /** */
  type Cusip = String Refined IsCusip

  /**
    * `Ibrk` identifiers represent [[https://interactivebrokers.com Interactive Brokers]]
    * `ConId`'s
    */
  val MatchesRxIbrk = """\d{8}\d?""".witness // 8 or 9 char, all numbers (evidently)

  /** */
  type MatchesRxIbrk = MatchesRegex[MatchesRxIbrk.T]

  /** */
  type IsIbrk = MatchesRxIbrk

  /** */
  type Ibrk = String Refined IsIbrk

  /**
    * `Unreg` identifiers represent unregistered securities, numbered by the firm.
    */
  val MatchesRxUnreg = """\d{8}\d?""".witness // 8 or 9 char, all numbers (evidently)

  /** */
  type MatchesRxUnreg = MatchesRegex[MatchesRxUnreg.T]

  /** */
  type IsUnreg = MatchesRxUnreg

  /** */
  type Unreg = String Refined IsUnreg

  // https://en.wikipedia.org/wiki/Luhn_algorithm
  private[model] def luhn(digit: Int, idx: Int): Int =
    if (idx % 2 === 0) digit else (digit * 2) / 10 + (digit * 2) % 10

  private[model] def failsafe(thunk: => Boolean): Boolean = Try(thunk).fold(_ => false, identity)
}

/**
  * Types and methods for financial reference data.
  */
package reference {

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

  /** */
  sealed abstract case class CheckedIsin()

  /** */
  object CheckedIsin {

    /** */
    lazy val instance: CheckedIsin = new CheckedIsin() {}

    /** */
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

  /** Pseudo `Isin` matches `Isin` regex, but uses the 9 digit body for proprietary mappings. */
  sealed abstract case class CheckedPsin()

  /** */
  object CheckedPsin {

    /** */
    lazy val instance: CheckedPsin = new CheckedPsin() {}

    /** */
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

  /**
    * A CUSIP is a nine character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  sealed abstract case class CheckedCusip()

  /** */
  object CheckedCusip {

    /** */
    lazy val instance: CheckedCusip = new CheckedCusip() {}

    /** */
    implicit def cusipValidate: Validate.Plain[String, CheckedCusip] =
      Validate fromPredicate (predicate, t => s"$t is not legit", instance)

    private def predicate(isin: String): Boolean =
      failsafe {

        // FIXME flesh this out

        true

      }
  }

  /**
    * transformations from camelCase to other case conventions
    */
  object camelsnake { outer =>

    def stripAsciiWs(s: String) = s filterNot (" \n\r\t" contains _)

    def camelToSnake(camelName: String): String  = CamelTo("_")(camelName)
    def camelToHyphen(camelName: String): String = CamelTo("-")(camelName)
    def camelToDot(camelName: String): String    = CamelTo(".")(camelName)
    def camelToWord(camelName: String): String   = CamelTo(" ")(camelName)

    implicit class CamelOps(val camelName: String) extends AnyVal {
      def camelToSnake: String  = outer.camelToSnake(camelName)
      def camelToHyphen: String = outer.camelToHyphen(camelName)
      def camelToDot: String    = outer.camelToDot(camelName)
      def camelToWord: String   = outer.camelToWord(camelName)
    }

    object CamelTo {

      val uppers    = 'A' to 'Z'
      val nonUppers = ('a' to 'z') ++ ('0' to '9') :+ '_' :+ '$'

      def apply(sep: String)(name: String): String = {
        val osc                        = maybeSepFrom(sep)
        val bh: Seq[Char] => Seq[Char] = bustHumps(osc)(_)
        val sc: String => Seq[Char]    = splitCaps(osc)(_)
        (sc andThen bh)(name).mkString
      }

      protected def splitCaps(sep: Option[Char])(name: String): Seq[Char] =
        name
          .foldLeft(Seq.empty[Char]) { (b, a) =>
            (a, b) match { // yeah just flip your head around, it's easier, trust self
              case (c, h +: g +: t)
                  if (uppers contains g) &&
                    (uppers contains h) &&
                    (nonUppers contains c) => // sep between g and h
                sep.fold(c +: h +: g +: t)(c +: h +: _ +: g +: t)
              case _ => a +: b
            }
          }
          .reverse

      protected def bustHumps(sep: Option[Char])(name: Seq[Char]): Seq[Char] =
        name.foldRight(Seq.empty[Char]) { (a, b) =>
          (a, b) match {
            case (c, h +: _) if (nonUppers contains c) && (uppers contains h) =>
              sep.fold(a +: b)(a +: _ +: b)
            case _ =>
              a +: b
          }
        }

      protected def maybeSepFrom(s: String): Option[Char] = s match {
        case "_" => Some('_')
        case "-" => Some('-')
        case _   => None
      }
    }
  }
}
