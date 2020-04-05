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
import refined.boolean.And
import refined.collection.{ Forall, MaxSize, MinSize }
import refined.numeric.{ Interval, Positive }
import refined.string.{ MatchesRegex, Trimmed }
// import refined.api.Validate

import scodec.bits.ByteVector

// import shapeless.syntax.singleton._
// import scala.language.existentials

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
    r
  }

  /** */
  final val Alpha2 = patRep(alpha)(2)

  /** */
  type Alpha2 = String Refined MatchesRegex[Alpha2.type]

  /** */
  final val Alpha3 = patRep(alpha)(3)

  /** */
  type Alpha3 = String Refined MatchesRegex[Alpha3.type]

  /** */
  final val Alpha4 = patRep(alpha)(4)

  /** */
  type Alpha4 = String Refined MatchesRegex[Alpha4.type]

  /** */
  final val Num3 = patRep(num)(3)

  /** */
  type Num3 = String Refined MatchesRegex[Num3.type]

  /**
    * RDB friendly `String`s that are born usable as is.
    * Postgres optimizes strings of length 126 (and shorter).
    * FIXME: account for `UTF-8` encoding
    */
  type IsVarChar = MaxSize[126]

  /** */
  type VarChar = String Refined IsVarChar

  /** */
  object VarChar {
    import refined.auto._
    val empty: VarChar = ""
  }

  /** Our `Refined` type naming convention: {{{type XYZ = T Refined IsXYZ}}} */
  type IsLabel = MinSize[3] And MaxSize[100] And Trimmed

  /** A saner `String` (non-empty, bounded, trimmed). */
  type Label = String Refined IsLabel

  /** Non-whitespace, non control block, non-empty ASCII string of bounded length. */
  type IsAscii24 = MinSize[3] And MaxSize[24] And Forall[Interval.Closed['!', '~']]

  /**
    * A short, pure ASCII, all printable, no whitespace `Label`.
    */
  type Ascii24 = String Refined IsAscii24

  /** */
  sealed abstract case class IsSha()

  object IsSha {

    lazy val instance: IsSha = new IsSha() {}

    implicit def isSha256Validate: Validate.Plain[String, IsSha] =
      Validate fromPredicate (predicate, t => s"$t is not a Base58 encoded 256 bit value", instance)

    def predicate(s: String): Boolean = failsafe {
      val Some(bs) = ByteVector fromBase58 s
      bs.size === 32
    }
  }

  /** */
  type Sha = String Refined IsSha

  /** */
  object Sha {

    /** Chosen project-wide (for now) */
    val Algo = "SHA-256"

    /** */
    def toByteVector(sha: Sha) = ByteVector fromValidBase58 sha.value
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
