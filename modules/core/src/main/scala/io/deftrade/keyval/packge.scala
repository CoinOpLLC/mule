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
import cats.{ Show }
import keyval.layers._

import shapeless.syntax.singleton._

import eu.timepit.refined
import refined.api.Refined

/**
  * Derived types and implicit methods for the persistence and caching of
  * domain `value types` (typically case classes),
  * with complementary key value store algebras and implementations.
  *
  * Defines a
  * [[https://en.wikipedia.org/wiki/Convention_over_configuration convention over configuration]]
  * system for:
  *   - `id`s: opaque Long based `id` (with `Order` instances)
  *   - `key`s: identifiers (including opaque identifiers)
  * with `Order`, and `Show` typeclass instances
  *   - `value`s: value class typeclass instances (`Eq`, `Hash` and `Show`).
  *   - etc.
  *
  * ''Convention'': A `type Foo` may not depend upon the type of the `key` for `Foo`s.
  *
  * Point being: there will be no `id: Id` fields within domain objects; these will be carried
  * separately (e.g. `key`s in an in-memory [[scala.collection.Map]] and will not depend in any way on the domain
  * value objects.
  *
  * However, foreign keys which reference other domain value types are permitted.
  *
  * This package provides `key` and `id` implementations which '''enforce''' the
  * convention given above.
  *   - aliasing `Refined` as an opaque key for a collection of a given type of values
  *   - assinging the `Value` type to be the `phantom type` parameter
  * for the `Refined` type constructor
  *   - providing the `Key` types and instances as companion base classes.
  *
  * Further, the package supports the instantiaton of the scheme by
  *   - providing a `Row` type `(Key, Value)`
  *   - providing a `Table` type (`Map[Key, Value]`) (see [[layers.stores]])
  *   - providing implicit derivations for [[layers.csv]] file readers and writers of `Row`s and `Table`s,
  * enabling spreadsheet integration.
  *
  * TODO: consider explicitly separating the structural items (keys and links between keys)
  * from the descriptive attributes, as with
  * [[https://en.wikipedia.org/wiki/Data_vault_modeling Data Vault]] style modelling.
  *
  *   - Q: What is a "business key?"
  *   - A: "Real business keys only change when the business changes!"
  *   - same goes for those essential, universal, canonical attributes
  *   - everything else is `meta: Json`
  *      - which can be stored / indexed as binary in e.g. Mongo and Postgres
  *      - which can be projected to create Satellite views.
  */
package object keyval extends stores with csv {

  /** Just an alias.  */
  type OpaqueKey[K, V] = Refined[K, V]

  // /** nb `Order` is inferred for _all_ `OpaqueKey[K: Order, V]` (unquallified for V) */
  // implicit def orderOpaqueKey[K: Order, V]: Order[OpaqueKey[K, V]] = Order by (_.value)

  /** nb `Show` is inferred for _all_ `OpaqueKey[K: Show, V]` (unquallified for V) */
  implicit def showOpaqueKey[K: Show, V]: Show[OpaqueKey[K, V]] =
    Show show (k => s"k=${k.value.show}")

  // FIXME: hash seems broken for even the simplest cases... doing someghing wrong? ;)
  // implicit def hashOpaqueKey[K, V]: Hash[OpaqueKey[K, V]] = cats.derived.semi.hash

  /**
    * The [[Id]] column is by convention assigned a key column label: `'id: Symbol`.
    *
    * The `id` member is a `shapeless.Aux[Symbol @@ String(id)]` instance,
    * useful for type member `T`, which is the (singleton) type of the id column label.
    */
  private[keyval] final implicit val id = Symbol("id").witness

  /**
    * [[Key]] column type literal witness - same purpose as [[id]].
    */
  private[keyval] final implicit val key = Symbol("key").witness
}
