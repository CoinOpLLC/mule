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

import cats.implicits._

import eu.timepit.refined
import refined.refineV
import refined.api.{ Refined, Validate }
import refined.boolean.{ And, Or }
import refined.string.{ MatchesRegex }

// import shapeless.syntax.singleton._

/**
  */
object Tax {

  /**
    */
  final val IsSsn = """\d{3}-\d{2}-\d{4}"""

  /**
    * Post [[https://www.ssa.gov/employer/randomization.html Randomization]]
    * SSN validation: i.e., cursory only.
    *
    * [[https://en.wikipedia.org/wiki/Social_Security_number#Valid_SSNs SSN validation]]
    * [[https://www.ssa.gov/history/ssn/geocard.html SSN geocard]]
    */
  final type IsSsn = MatchesRegex[IsSsn.type] And CheckedSsn

  /**
    */
  final type SSN = String Refined IsSsn

  object SSN {

    /**
      */
    def unapply(no: No)(implicit v: Validate[String, IsSsn]): Option[SSN] =
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
    * TaxId for a legal (eg corporate, or non-profit) body.
    *
    * TODO: '''DTC''' ''Legal Entity Identifier '' `LEI` definition (issuing party for public secs)
    */
  final val IsEin = """\d{2}-\d{7}"""

  /**
    */
  final type IsEin = MatchesRegex[IsEin.type]

  /**
    */
  final type EIN = String Refined IsEin

  /**
    */
  object EIN {
    def unapply(no: No)(implicit v: Validate[String, IsEin]): Option[EIN] =
      refineV[IsEin](no.value).toOption
  }

  /**
    */
  type IsNo = IsSsn Or IsEin

  /**
    */
  type No = String Refined IsNo
}
