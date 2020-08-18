package io.deftrade
package model
package capital

import refinements.failsafe

import cats.implicits._

import shapeless.syntax.singleton._

import eu.timepit.refined
import refined.api.Refined
import refined.api.Validate
import refined.boolean.{ And, Or }
import refined.string.{ MatchesRegex }

/**
  * Security Idenification Numbers (of any kind), modelled as `String Refined IsXsin`.
  */
object keys {

  /**
    * An ISIN is a twelve character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  val MatchesRxIsin = """[A-Z]{2}[A-Z0-9]{9}[0-9]""".witness
  type MatchesRxIsin = MatchesRegex[MatchesRxIsin.T]
  type IsIsin        = MatchesRxIsin And CheckedIsin
  type Isin          = String Refined IsIsin

  /**
    */
  sealed abstract case class CheckedIsin()

  /**
    */
  object CheckedIsin {

    /**
      */
    lazy val instance: CheckedIsin = new CheckedIsin() {}

    /**
      */
    implicit def isinValidate: Validate.Plain[String, CheckedIsin] =
      Validate.fromPredicate(predicate, t => s"$t is not Luhny", instance)

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
    private def predicate(isin: String): Boolean =
      failsafe {

        val digits = for {
          c <- isin
          d <- Character.digit(c, 36).toString
        } yield d.asDigit

        val check = for ((d, i) <- digits.reverse.zipWithIndex) yield luhn(d, i)

        check.sum % 10 === 0

      }
  }

  /** Pseudo `Isin` matches `Isin` regex, but uses the 9 digit body for proprietary mappings. */
  type IsPsin = MatchesRxIsin And CheckedPsin // sic
  type Psin   = String Refined IsPsin

  /**
    */
  sealed abstract case class CheckedPsin()

  /**
    */
  object CheckedPsin {

    /**
      */
    lazy val instance: CheckedPsin = new CheckedPsin() {}

    /**
      */
    implicit def isinValidate: Validate.Plain[String, CheckedPsin] =
      Validate.fromPredicate(predicate, t => s"$t is not Luhny", instance)

    private def predicate(isin: String): Boolean =
      failsafe {

        val digits = for {
          c <- isin
          d <- Character.digit(c, 36).toString
        } yield d.asDigit

        val check = for ((d, i) <- digits.reverse.zipWithIndex) yield luhn(d, i)

        check.sum % 10 === 0
      }
  }

  /**
    * How deftrade canonicalizes securities identifiers:
    * Use `Isin`s where non-isin uses use reserved country codes: {X*, ZZ}
    *
    * Universal Security Identifying Number: Usin
    */
  type IsUsin = IsIsin Or IsPsin
  type Usin   = String Refined IsUsin

  /**
    */
  object Usin {

    /**
      * the least we can do
      */
    def from(s: String): Result[Usin] = ???

    /**
      */
    def fromIsin(isin: Isin): Isin = ???

    /**
      */
    def fromCusip(cusip: Cusip): Isin = ???
    // def fromSedol(sedol: Sedol): Isin = ???

    /**
      */
    def fromUnreg(unreg: Unreg): Psin = ???

    /**
      */
    def fromIbrk(ibrk: Ibrk): Psin = ???

    /**
      */
    def toIsin(usin: Usin): Result[Isin] = ???

    /**
      */
    def toCusip(usin: Usin): Result[Cusip] = ???
    // def toSedol(usin: Usin): Result[Sedol] = ???

    /**
      */
    def toUnreg(usin: Usin): Result[Unreg] = ???

    /**
      */
    def toIbrk(usin: Usin): Result[Ibrk] = ???
  }

  /**
    */
  val MatchesRxCusip = """[0-9]{3}[0-9A-Z]{3}[0-9]{3}""".witness
  type MatchesRxCusip = MatchesRegex[MatchesRxCusip.T]
  type IsCusip        = MatchesRxCusip And CheckedCusip
  type Cusip          = String Refined IsCusip

  /**
    * A CUSIP is a nine character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  sealed abstract case class CheckedCusip()

  /**
    */
  object CheckedCusip {

    /**
      */
    lazy val instance: CheckedCusip = new CheckedCusip() {}

    /**
      */
    implicit def cusipValidate: Validate.Plain[String, CheckedCusip] =
      Validate.fromPredicate(predicate, t => s"$t is not legit", instance)

    private def predicate(isin: String): Boolean =
      failsafe {

        // FIXME flesh this out

        true

      }
  }

  /**
    * `Ibrk` identifiers represent [[https://interactivebrokers.com Interactive Brokers]]
    * `ConId`'s
    */
  val MatchesRxIbrk = """\d{8}\d?""".witness // 8 or 9 char, all numbers (evidently)
  type MatchesRxIbrk = MatchesRegex[MatchesRxIbrk.T]
  type IsIbrk        = MatchesRxIbrk
  type Ibrk          = String Refined IsIbrk

  /**
    * `Unreg` identifiers represent unregistered securities, numbered by the firm.
    * TODO: revisit how these are defined.
    */
  val MatchesRxUnreg = """\d{8}\d?""".witness
  type MatchesRxUnreg = MatchesRegex[MatchesRxUnreg.T]
  type IsUnreg        = MatchesRxUnreg
  type Unreg          = String Refined IsUnreg

  /**
    * `UsBan` identifiers represent bank account numbers in the US
    * TODO: Next up is IBAN
    */
  val MatchesRxUsBan = """\d{8,10}""".witness
  type MatchesRxUsBan = MatchesRegex[MatchesRxUsBan.T]
  type IsUsBan        = MatchesRxUnreg // And CheckedUsBan
  type UsBan          = String Refined IsUsBan

  ////////////////////////////////////////////////

  // https://en.wikipedia.org/wiki/Luhn_algorithm
  private[deftrade] def luhn(digit: Int, idx: Int): Int =
    if (idx % 2 === 0) digit else (digit * 2) / 10 + (digit * 2) % 10
}
