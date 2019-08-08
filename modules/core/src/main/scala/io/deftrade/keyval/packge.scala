package io.deftrade

import cats.implicits._
import cats.{ Order, Show }

import eu.timepit.refined
import refined.api.Refined

import shapeless.syntax.singleton._

// import io.chrisdavenport.cormorant.implicits._
// import io.chrisdavenport.cormorant.generic.semiauto._
// import io.chrisdavenport.cormorant.parser._
// import io.chrisdavenport.cormorant.refined._

/**
  * Defines a scheme* for enriching domain value types (typically case classes) with additional
  * types and implicit methods useful for persistence and caching.
  *
  *   - `id`s: opaque Long based `id` (with `Order` instances)
  *   - `key`s: opaque identifiers with `Order`, `Hash` and `Show` typeclass instances
  *   - `value`s: value class typeclass instances (`Eq`, `Hash` and `Show`).
  *   - etc.
  *
  * This shall be the law: A `type Foo` may not depend upon the type of the `key` for `Foo`s.
  *
  * This package provides `key` and `id` implementations which abide the law.
  *   - aliasing `Refined` as an opaque key for a collection of a given type of values
  *   - assinging the `Value` type to be the phantom type parameter for the Refined type constructor
  *   - providing the `Key` types and instances as companion base classes.
  *
  * Further, the package supports the instantiaton of the scheme by
  *   - providing a `Row` type (`Key` -> `Value`)
  *   - providing a `Table` type (Map[Key, Value])
  *   - providing implicit derivations for CSV file readers and writers of `Row`s and `Tables`s.
  *
  * *,, it's just a scheme because calling it a "schema" is too grand,,
  */
package object keyval extends csv {

  /** Just an alias, bssically.  */
  type OpaqueKey[K, V] = Refined[K, V]

  /** nb `Order` is inferred for _all_ `OpaqueKey[K: Order, V]` (unquallified for V) */
  implicit def orderOpaqueKey[K: Order, V]: Order[OpaqueKey[K, V]] = Order by (_.value)

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
  private[keyval] final val id = Symbol("id").witness

  /**
    * [[Key]] column type literal witness - same purpose as [[id]].
    */
  private[keyval] final val key = Symbol("key").witness

}
