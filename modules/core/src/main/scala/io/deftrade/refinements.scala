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

import shapeless.nat.{ _0, _1 }

import eu.timepit.refined
import eu.timepit.refined.boolean._
import refined.collection.{ Forall, Size }
import refined.numeric.{ Interval, LessEqual, Positive }
import refined.string.{ MatchesRegex, Trimmed }
import refined.api.{ Refined }
// import Inference.==>
import Interval.{ Closed => To }

/** A palette of domain specific refined types.
  */
object refinements {

  // power tools on
  import refined.auto._

  private def alpha = "[A-Z]"
  private def num   = "[0-9]"

  private def patRep(pat: String)(n: Int Refined Positive) =
    s"""$pat{${n.toString}}"""

  /**
    */
  final val Alpha2 = patRep(alpha)(2)

  /**
    */
  type Alpha2 = String Refined MatchesRegex[Alpha2.type]

  /**
    */
  final val Alpha3 = patRep(alpha)(3)

  /**
    */
  type Alpha3 = String Refined MatchesRegex[Alpha3.type]

  /**
    */
  final val Alpha4 = patRep(alpha)(4)

  /**
    */
  type Alpha4 = String Refined MatchesRegex[Alpha4.type]

  /**
    */
  final val Num3 = patRep(num)(3)

  /**
    */
  type Num3 = String Refined MatchesRegex[Num3.type]

  /** RDB friendly `String`s that are born usable as is.
    * Postgres optimizes strings of length 126 (and shorter).
    * FIXME: account for `UTF-8` encoding
    */
  type IsVarChar = Size[LessEqual[126]]

  /**
    */
  type VarChar = String Refined IsVarChar

  /**
    */
  object VarChar {
    import refined.auto._
    val empty: VarChar = ""
  }

  /** Our `Refined` type naming convention: {{{type XYZ = T Refined IsXYZ}}} */
  // type IsLabel = Size[GreaterEqual[3] And LessEqual[100]] And Trimmed
  type IsLabel = Size[Interval.Closed[3, 100]] And Trimmed
  // type IsLabel = Size[LessEqual[100]]

  /** A saner `String` (non-empty, bounded, trimmed). */
  type Label = String Refined IsLabel

  /** Non-whitespace, non control block, non-empty ASCII string of bounded length. */
  type IsAscii24 = Size[3 To 24] And Forall['!' To '~']

  /** A short, pure ASCII, all printable, no whitespace `Label`.
    */
  type Ascii24 = String Refined IsAscii24

  /**
    */
  object IsUnitInterval {
    type `(0,1)` = Interval.Open[_0, _1]
    type `[0,1)` = Interval.ClosedOpen[_0, _1]
    type `(0,1]` = Interval.OpenClosed[_0, _1]
    type `[0,1]` = Interval.Closed[_0, _1]
  }

  /**
    */
  private[deftrade] def failsafe(thunk: => Boolean): Boolean =
    scala.util.Try(thunk).fold(_ => false, identity)
}
