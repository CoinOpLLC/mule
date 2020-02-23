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

import implicits._

import cats.implicits._

import eu.timepit.refined
import refined.api.Refined
import refined.{ refineV, W }
import refined.boolean.And
import refined.generic.Equal
import refined.collection.{ MaxSize, NonEmpty, Size }
import refined.numeric.{ Interval, Positive }
import refined.string.{ MatchesRegex, Trimmed }
// import refined.api.Validate

import shapeless.syntax.singleton._

import scala.language.existentials

/**
  * A palette of domain specific refined types.
  *
  * Prêt à porter, batteries included, your metaphore here (civil pull requests considered).
  */
object refinements {

  // power tools on
  import refined.auto._

  private def alpha = "[A-Z]"
  private def num   = "[0-9]"

  private def patRep(pat: String)(n: Int Refined Positive) = {
    val r = s"""$pat{${n.toString}}"""
    r.witness
  }

  /** */
  val Alpha2 = patRep(alpha)(2)

  /** */
  type Alpha2 = String Refined MatchesRegex[Alpha2.T]

  /** */
  val Alpha3 = patRep(alpha)(3)

  /** */
  type Alpha3 = String Refined MatchesRegex[Alpha3.T]

  /** */
  val Alpha4 = patRep(alpha)(4)

  /** */
  type Alpha4 = String Refined MatchesRegex[Alpha4.T]

  /** */
  val Num3 = patRep(num)(3)

  /** */
  type Num3 = String Refined MatchesRegex[Num3.T]

  /**
    * RDB friendly `String`s that are born usable as is.
    * Defaults to Postgres, which is the shorter limit (126)
    */
  type IsVarChar = IsVarChar126

  /** */
  type VarChar = String Refined IsVarChar

  /** */
  object VarChar {
    import refined.auto._
    val empty: VarChar = ""
  }

  /** Our `Refined` type naming convention: {{{type XYZ = T Refined IsXYZ}}} */
  type IsLabel = IsVarChar And Trimmed And NonEmpty

  /** A saner `String` (non-empty, bounded, trimmed). */
  type Label = String Refined IsLabel

  /** Postgres optimizes strings of this length (and shorter). */
  type IsVarChar126 = MaxSize[W.`126`.T]

  /** */
  type VarChar126 = String Refined IsVarChar126

  /** Typical SQL */
  type IsVarChar255 = MaxSize[W.`255`.T]

  /** */
  type VarChar255 = String Refined IsVarChar255

  /** */
  type IsSha256 = Size[Equal[W.`32`.T]]

  /** */
  type Sha256 = Array[Byte] Refined IsSha256

  /** */
  object Sha256 {
    import java.util.Base64
    def toBase64(sha: Sha256): String = Base64.getUrlEncoder encodeToString sha.value
    def fromBase64(b64: String): Result[Sha256] =
      refineV[IsSha256](Base64.getUrlDecoder decode b64) leftMap Fail.fromString
  }

  /** */
  implicit class ShaOps(val sha: Sha256) {
    def toBase64: String = Sha256 toBase64 sha
  }

  /**  */
  object IsUnitInterval {
    import _root_.shapeless.nat.{ _0, _1 }
    type `(0,1)` = Interval.Open[_0, _1]
    type `[0,1)` = Interval.ClosedOpen[_0, _1]
    type `(0,1]` = Interval.OpenClosed[_0, _1]
    type `[0,1]` = Interval.Closed[_0, _1]
  }
}
