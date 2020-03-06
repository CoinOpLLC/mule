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

import refinements.{ Sha256 }

import spire.math.Integral
import spire.syntax.field._

import eu.timepit.refined
import refined.api.{ Refined }
import scodec.bits.ByteVector

import java.security.MessageDigest

/**
  * Defines how to create a fresh '''globally unique''' key which
  * is suitable to be persisted.
  */
sealed abstract case class Fresh[K, V](next: (K, V) => K)

object Fresh {

  /**
    * Equivalent to `autoincrement` or `serial` from SQL.
    *
    * TODO: PRNG version.
    */
  def zeroBasedIncr[K: Integral, P]: Fresh[OpaqueKey[K, P], P] = {

    val K = Integral[K]; import K._

    apply { (key, ph) =>
      OpaqueKey(key.value + one)
    }
  }

  def apply[K, V](next: (K, V) => K): Fresh[K, V] = new Fresh(next) {}

  /**
    * The use of unsafeApply here is canonical: this is literally the definition of the type.
    *
    * TODO:
    *   - threading: single threaded per instance - is this ok?
    */
  def sha256[V]: Fresh[Sha256, V] = {

    val md = MessageDigest getInstance "SHA-256"
    new Fresh[Sha256, V](
      (j, v) => {
        md update (Sha256 toByteArray j)
        md update (v.toString getBytes "UTF-8")
        Refined unsafeApply ByteVector(md.digest()).toBase58
      }
    ) {}
  }
}
