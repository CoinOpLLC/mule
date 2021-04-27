package io.deftrade
package model.slices

import refinements.failsafe, keyval.Result

import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.api.Validate
import refined.boolean.{ And, Or }
import refined.string.{ MatchesRegex }

/** Security Idenification Numbers (of any kind), modelled as `String Refined IsXsin`.
  */
object keys {

  /** An ISIN is a twelve character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  type MatchesRxIsin = MatchesRegex["""[A-Z]{2}[A-Z0-9]{9}[0-9]"""] //""" ide syntax color hack
  type IsISIN        = MatchesRxIsin And CheckedISIN
  type ISIN          = String Refined IsISIN

  object ISIN {

    /** May fail, so return a result.
      */
    def fromUSIN(isin: USIN): Result[ISIN] = ???

    /** Note these transforms should not fail.
      */
    def fromCUSIP(cusip: CUSIP): ISIN = ???
    // def fromSEDOL(sedol: SEDOL): ISIN = ???
  }

  /**
    */
  sealed abstract case class CheckedISIN private ()

  /**
    */
  object CheckedISIN {

    /**
      */
    object instance extends CheckedISIN

    /**
      */
    implicit def isinValidate: Validate.Plain[String, CheckedISIN] =
      Validate.fromPredicate(predicate, t => s"$t is not Luhny", instance)

    /** * TODO need to add country checks,
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

  /** Pseudo `ISIN` matches `ISIN` regex, but uses the 9 digit body for proprietary mappings. */
  type IsPSIN = MatchesRxIsin And CheckedPSIN // sic
  type PSIN   = String Refined IsPSIN

  object PSIN {

    /**
      */
    def fromUnregistered(unreg: Unregistered): PSIN = ???

    def toUnregistered(psin: PSIN): Result[Unregistered] = ???

    /** TODO: how does option sybology jibe with ISINs?
      */
    def fromIBRK(ibrk: IBRK): PSIN = ???
  }

  /**
    */
  sealed abstract case class CheckedPSIN private ()

  /**
    */
  object CheckedPSIN {

    /**
      */
    object instance extends CheckedPSIN

    /** TODO: this needs to be specified.
      * For now, whatever passes regex is OK.
      */
    implicit def isinValidate: Validate.Plain[String, CheckedPSIN] =
      Validate.fromPredicate(predicate, t => s"$t is not Luhny", instance)

    private def predicate(isin: String): Boolean =
      true
  }
  // ISIN entails USIN
  // PSIN entails USIN

  /** How deftrade canonicalizes securities identifiers:
    * Use `ISIN`s where non-isin uses use reserved country codes: {X*, ZZ}
    *
    * Universal Security Identifying Number: USIN
    */
  type IsUSIN = IsISIN Or IsPSIN
  type USIN   = String Refined IsUSIN

  /**
    */
  type MatchesRxCUSIP = MatchesRegex["""[0-9]{3}[0-9A-Z]{3}[0-9]{3}"""] //"""
  type IsCUSIP        = MatchesRxCUSIP And CheckedCUSIP
  type CUSIP          = String Refined IsCUSIP
  object CUSIP {

    def from(s: String): Result[CUSIP]      = ???
    def fromISIN(isin: ISIN): Result[CUSIP] = ???
  }

  /** A CUSIP is a nine character string that must match a certain regex, and whose characters
    * must pass a certain (Luhn) checksum.
    */
  sealed abstract case class CheckedCUSIP private ()

  /**
    */
  object CheckedCUSIP {

    /**
      */
    object instance extends CheckedCUSIP

    /**
      */
    implicit def cusipValidate: Validate.Plain[String, CheckedCUSIP] =
      Validate.fromPredicate(predicate, t => s"$t is not legit", instance)

    private def predicate(isin: String): Boolean =
      failsafe {

        // FIXME flesh this out

        true

      }
  }

  /** `IBRK` identifiers represent [[https://interactivebrokers.com Interactive Brokers]]
    * `ConId`'s
    */
  type MatchesRxIbrk = MatchesRegex["""\d{8}\d?"""] // 8 or 9 numbers
  type IsIBRK        = MatchesRxIbrk
  type IBRK          = String Refined IsIBRK
  object IBRK {
    def from(s: String): Result[IBRK] = ???
  }

  /** `Unregistered` identifiers represent unregistered securities, numbered by the firm.
    * TODO: revisit how these are defined.
    */
  type MatchesRxUnreg = MatchesRegex["""\d{8,9}"""]
  type IsUnregistered = MatchesRxUnreg
  type Unregistered   = String Refined IsUnregistered
  object Unregistered {
    def from(s: String): Result[Unregistered] = ???
  }

  /** `UsBAN` identifiers represent bank account numbers in the US
    * TODO: Next up is IBAN
    */
  type MatchesRxUsBan = MatchesRegex["""\d{8,10}\"""]
  type IsUsBan        = MatchesRxUnreg // And CheckedUsBan
  type UsBAN          = String Refined IsUsBan
  object UsBAN {
    def from(s: String): Result[UsBAN] = ???
  }

  ////////////////////////////////////////////////

  // https://en.wikipedia.org/wiki/Luhn_algorithm
  private[deftrade] def luhn(digit: Int, idx: Int): Int =
    if (idx % 2 === 0)
      digit
    else {
      val d2 = digit * 2
      d2 / 10 + d2 % 10
    }
}
