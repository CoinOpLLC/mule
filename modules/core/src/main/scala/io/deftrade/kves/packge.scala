package io.deftrade

import cats.{ Eq, Order, Show }
import cats.implicits._

import eu.timepit.refined
import refined.api.{ Min, Refined }

import spire.math.Integral
import spire.implicits._

/**
  * kvse: Key Value Entity Scheme*:
  *
  * - keys: opaque identifiers with `Order`, `Eq`, `Hash` and `Show` (uses `refined`).
  * - values: value objects (case classes) with `Eq`, `Hash` and `Show`
  * - entities: ("aggregate roots") `Map`s of Key -> Value entries: repos, logs...
  *
  * This shall be the law: A `type Foo` may not depend on any type used as a key for `Foo`s.
  *
  * This package will enforce the law by
  * - aliasing `Refined` as an opaque key for a collection of a given type of values
  * - using the type of the `Value` to be the phantom types
  * - providing the `Key` types and instances as companion base classes.
  *
  * *It's a scheme because calling it a "schema" is too grand.
  */
package object kves {

  /**
    * Our keys will be embedded with phantom types.
    * By convention, we will use as the phantom type the type of the `Value`
    * which is indexed by this `Key`.
    */
  type OpaqueKey[K, P] = Refined[K, P]

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, P] (unquallified for P) */
  implicit def orderOpaqueKey[K: Order, P]: Order[OpaqueKey[K, P]] =
    Order by (_.value)

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, P] (unquallified for P) */
  implicit def showOpaqueKey[K: Show: Order, P]: Show[OpaqueKey[K, P]] =
    Show show (key => s"k: ${key.value.show}")
}

package kves {

  object OpaqueKey {

    def apply[K: Order, P: Eq](k: K): OpaqueKey[K, P] = Refined unsafeApply k

  }

  /**
    * Make a fresh *globally unique* key, suitable to be persisted.
    * TODO: consider threading F[_] thru Fresh.
    */
  final case class Fresh[I](init: I, next: I => I)

  /** fresh `Fresh` */
  object Fresh {

    def apply[KY: Fresh] = implicitly[Fresh[KY]]

    def zeroBasedIncr[K: Integral, P: Eq]: Fresh[OpaqueKey[K, P]] = {

      val K = Integral[K]
      import K._

      Fresh(
        OpaqueKey(zero),
        key => OpaqueKey(key.value + one)
      )
    }
  }

  /** Module-level companion base class with default Key type */
  abstract class WithKey[K: Order, P: Eq] {
    type Key = OpaqueKey[K, P]
    object Key extends OpaqueKeyC[K, P]
  }

  abstract class WithKeyAndEq[K: Order, P] {
    type Key = OpaqueKey[K, P]
    object Key extends OpaqueKeyC[K, P]
    implicit lazy val eqP: Eq[P] = Eq.fromUniversalEquals[P]
  }

  /** Key type companion base class. */
  private[kves] abstract class OpaqueKeyC[K: Order, P: Eq] {
    def apply(k: K) = OpaqueKey[K, P](k)

    /** Where the key type is integral, we will reserve the min value. */
    def reserved(implicit K: Min[K]) = OpaqueKey[K, P](K.min)
  }
}
