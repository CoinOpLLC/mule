package io.deftrade

import cats.{ Eq, Order, Show }
import cats.implicits._

import eu.timepit.refined.api.Refined

import spire.math.Integral
import spire.implicits._

/**
  * kvse: Key Value Entity Schema:
  * - keys: opaque identifiers with `Order`, `Eq`, `Hash` and `Show` (uses `refined`).
  * - values: value objects (case classes) with `Eq`, `Hash` and `Show`
  * - entities: ("aggregate roots") `Map`s of Key -> Value entries: repos, logs...
  */
package object kves {

  type OpaqueKey[K, P] = Refined[K, P]

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, P] (unquallified for P) */
  implicit def orderOpaqueKey[K: Order, P]: Order[OpaqueKey[K, P]] =
    Order by (_.value)

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, P] (unquallified for P) */
  implicit def showOpaqueKey[K: Show: Order, P]: Show[OpaqueKey[K, P]] =
    Show show (key => s"key:${key.value.show}")
}

package kves {

  object OpaqueKey {

    def apply[K: Order, P: Eq](id: K): OpaqueKey[K, P] = Refined unsafeApply id

    /**
      * Make a fresh *globally unique* key, suitable to be persisted.
      * TODO: consider threading F[_] thru Fresh. (See above.)
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
          id => OpaqueKey(id.value + one)
        )
      }
    }
  }

  abstract class OpaqueIdC[K: Order, P: Eq] {
    def apply(id: K) = OpaqueKey[K, P](id)
  }

  abstract class IdC[K: Order, P: Eq] {
    type Id = OpaqueKey[K, P]
    object Id extends OpaqueIdC[K, P]
  }

  abstract class IdPC[K: Order, P] {
    type Id = OpaqueKey[K, P]
    object Id extends OpaqueIdC[K, P]
    implicit lazy val eqP: Eq[P] = Eq.fromUniversalEquals[P]
  }
}
