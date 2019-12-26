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

import eu.timepit.refined
import refined.api.Refined
import refined.W
import refined.boolean.And
import refined.collection.{ MaxSize, NonEmpty, Size }
import refined.numeric.Positive
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

  private def witnesseth[T](t: T) = { val tv = t; tv.witness }
  private def patRep(pat: String)(n: Int Refined Positive) =
    s"""$pat{${n.toString}}""" |> witnesseth

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

  /** */
  type Label = String Refined (IsVarChar And Trimmed And NonEmpty)

  /** Postgres optimizes strings less than this. */
  type IsVarChar126 = MaxSize[W.`126`.T]

  /** */
  type VarChar126 = String Refined IsVarChar126

  /** Typical SQL */
  type IsVarChar255 = MaxSize[W.`255`.T]

  /** */
  type VarChar255 = String Refined IsVarChar255

  /** */
  type IsSha256 = Size[W.`32`.T]

  /** */
  type Sha256 = Array[Byte] Refined IsSha256
}
