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

import cats.implicits._

import eu.timepit.refined
import refined.api.{ Refined, Validate }
import refined.{ refineV, W }
import refined.boolean.And
import refined.generic.Equal
import refined.collection.{ MaxSize, NonEmpty, Size }
import refined.numeric.{ Interval, Positive }
import refined.string.{ MatchesRegex, Trimmed }
// import refined.api.Validate

import scodec.bits.ByteVector

import shapeless.syntax.singleton._

import scala.language.existentials

/**
  * A palette of domain specific refined types.
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
  sealed abstract case class IsSha256()

  object IsSha256 {

    lazy val instance: IsSha256 = new IsSha256() {}

    implicit def isSha256Validate: Validate.Plain[String, IsSha256] =
      Validate fromPredicate (predicate, t => s"$t is not a Base58 encoded 256 bit value", instance)

    def predicate(s: String): Boolean = failsafe {
      val Some(bs) = ByteVector fromBase58 s
      bs.size === 32
    }
  }

  /** */
  type Sha256 = String Refined IsSha256

  /** FIXME */
  /** */
  object Sha256 {
    def toByteVector(sha: Sha256)             = ByteVector fromValidBase58 sha.value
    def toByteArray(sha: Sha256): Array[Byte] = toByteVector(sha).toArray
  }

  /**  */
  object IsUnitInterval {
    import _root_.shapeless.nat.{ _0, _1 }
    type `(0,1)` = Interval.Open[_0, _1]
    type `[0,1)` = Interval.ClosedOpen[_0, _1]
    type `(0,1]` = Interval.OpenClosed[_0, _1]
    type `[0,1]` = Interval.Closed[_0, _1]
  }

  /**  */
  private[deftrade] def failsafe(thunk: => Boolean): Boolean =
    scala.util.Try apply thunk fold (_ => false, identity)
}
