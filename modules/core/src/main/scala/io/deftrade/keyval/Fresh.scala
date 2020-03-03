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
package keyval

import refinements.{ IsSha256, Sha256 }

import spire.math.Integral
import spire.syntax.field._

import java.security.MessageDigest

/** */
sealed abstract case class Fresh[K](init: () => K, next: K => K)

/**
  * Defines how to create a fresh '''globally unique''' key which
  * is suitable to be persisted.
  */
object Fresh {

  /** */
  def apply[K](init: => K, next: K => K): Fresh[K] = new Fresh(() => init, next) {}

  /** */
  def apply[K](implicit K: Fresh[K]) = K

  /**
    * Equivalent to `autoincrement` or `serial` from SQL.
    *
    * TODO: PRNG version.
    */
  def zeroBasedIncr[K: Integral, P]: Fresh[OpaqueKey[K, P]] = {

    val K = Integral[K]; import K._

    Fresh(OpaqueKey(zero), key => OpaqueKey(key.value + one))
  }
}

import eu.timepit.refined
import refined.refineV
import refined.api.Validate

sealed abstract case class Phresh[K, V](next: (K, V) => K)

import scodec.bits.ByteVector
object Phresh {

  /** FIXME need a strategy which comprehends the b58 strings as well */
  implicit def validateSha256: Validate.Plain[ByteVector, IsSha256] = ???

  def apply[K, V](next: (K, V) => K): Phresh[K, V] = new Phresh(next) {}

  /**
    * TODO:
    *   - hybrid (binary key, text value)... is this what we want?
    *   - Sha256 will have to print out as Base64... hash that?
    *   - do we need `String Refined IsSha256AsBase64`
    *   - threading: single threaded per instance - is this ok?
    */
  def sha256[V]: Phresh[Sha256, V] = {

    val md = MessageDigest getInstance "SHA-256"
    new Phresh[Sha256, V](
      (j, v) => {
        md update j.value.toArray
        md update (v.toString getBytes "UTF-8")
        val Right(sha) = refineV[IsSha256](ByteVector(md.digest()))
        sha
      }
    ) {}
  }
}
