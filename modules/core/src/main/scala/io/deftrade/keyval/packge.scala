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

import cats.{ Order, Show }

import eu.timepit.refined
import refined.api.{ Refined, Validate }

/** Key value store algebras and implementations for persistence and caching of
  * domain value types (typically case classes).
  *
  * TODO: Postgres / Mongo / Kafka integration
  */
package object keyval {
// package object keyval extends keyval.dsl {

  import shapeless.syntax.singleton._

  /** The [[Id]] column is by convention assigned a key column label: `'id: Symbol`.
    *
    * The `id` member is a `shapeless.Aux[Symbol @@ String(id)]` instance,
    * useful for type member `T`, which is the (singleton) type of the id column label.
    */
  private[keyval] final implicit val id = Symbol("id").witness

  /** [[Key]] column type literal witness - same purpose as [[id]].
    */
  private[keyval] final implicit val key = Symbol("key").witness

  /** Just an alias. */
  type OpaqueKey[K, V] = Refined[K, V]
}

package keyval {

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
