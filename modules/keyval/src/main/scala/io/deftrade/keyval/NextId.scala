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

import cats.implicits._
import cats.Show

import spire.math.Integral
import spire.syntax.field._

import eu.timepit.refined
import refined.api.{ Refined }
import scodec.bits.ByteVector

import java.security.MessageDigest

/** Defines how to create a fresh '''globally unique''' key which
  * is suitable to be persisted.
  */
sealed abstract case class NextId[K, V](final val next: (K, V) => K) {

  /**
    */
  def nextAll(j: K, v: V, vs: V*): K =
    vs.foldLeft(next(j, v))(next)
}

/**
  */
object NextId {

  /**
    */
  def apply[K, V](next: (K, V) => K): NextId[K, V] = new NextId(next) {}

  /** Equivalent to `autoincrement` or `serial` from SQL.
    */
  def zeroBasedIncr[K: Integral: Show, P]: NextId[OpaqueKey[K, P], P] = {

    val K = Integral[K]; import K._

    apply { (key, ph) =>
      OpaqueKey(key.value + one)
    }
  }

  /** Simple content-addressed `Id` generation using secure hash (`SHA`)
    */
  def shaContent[V: Show]: NextId[SHA, V] = {
    val md = MessageDigest getInstance SHA.Algo

    new NextId[SHA, V]((_, v) => {
      md update (v.show getBytes "UTF-8")
      Refined unsafeApply ByteVector(md.digest).toBase58
    }) {}

  }

  /** Stitch the previous `Id` into the `sha` for the next `Id`.
    *
    * FIXME: the `Show` thing is just a hack; use scodec and CBOR
    * and pay attention to canonicalization
    */
  def shaChain[V: Show]: NextId[SHA, V] = {

    val md = MessageDigest getInstance SHA.Algo

    new NextId[SHA, V]((j, v) => {
      md update (SHA toByteVector j).toArray
      md update (v.show getBytes "UTF-8")
      Refined unsafeApply ByteVector(md.digest).toBase58
    }) {}
  }
}
