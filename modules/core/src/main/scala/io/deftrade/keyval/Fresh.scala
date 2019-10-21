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

import spire.math.Integral
import spire.syntax.field._

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
