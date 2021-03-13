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

import shapeless.syntax.singleton._

import cats.implicits._
import cats.{ Order, Show }

import eu.timepit.refined
import refined.api.{ Refined, Validate }

import scodec.bits.ByteVector

/** Key value store algebras and implementations for persistence and caching of
  * domain value types (typically case classes).
  */
package object keyval extends results.mixin {

  /** Just an alias.
    */
  type OpaqueKey[K, V] = Refined[K, V]

  /**
    */
  type SHA = String Refined IsSHA

  /** The [[Id]] column is by convention assigned a key column label: `'id: Symbol`.
    *
    * The `id` member is a `shapeless.Aux[Symbol @@ String(id)]` instance,
    * useful for type member `T`, which is the (singleton) type of the id column label.
    */
  private[keyval] final val id = Symbol("id").witness

  /** [[Key]] column type literal witness - same purpose as [[id]].
    */
  private[keyval] final val key = Symbol("key").witness
}

package keyval {

  /**
    */
  sealed abstract case class IsSHA()

  /**
    */
  object IsSHA {

    lazy val instance: IsSHA = new IsSHA() {}

    implicit def isSha256Validate: Validate.Plain[String, IsSHA] =
      Validate.fromPredicate(predicate, t => s"$t is not a Base58 encoded 256 bit value", instance)

    def predicate(s: String): Boolean =
      scala.util
        .Try {
          val Some(bs) = ByteVector fromBase58 s
          bs.size === 32
        }
        .fold(_ => false, identity)
  }

  /**
    */
  object SHA {

    /** Chosen project-wide (for now) */
    val Algo = "SHA-256"

    /**
      */
    def toByteVector(sha: SHA) = ByteVector fromValidBase58 sha.value
  }

  /**
    */
  object OpaqueKey {

    /**
      */
    private[keyval] def apply[K: Order: Show, V](k: K): OpaqueKey[K, V] = Refined unsafeApply k

    /**
      */
    def unsafe[K: Order: Show, V](k: K): OpaqueKey[K, V] = apply(k)

    /** TODO: review; feels questionably permissive
      */
    implicit def validate[K: Order: Show, V]: Validate[K, V] = Validate alwaysPassed (())
  }
}
