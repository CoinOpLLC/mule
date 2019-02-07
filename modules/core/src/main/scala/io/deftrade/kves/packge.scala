package io.deftrade

import cats.{ Eq, Order }
import spire.math.Integral
import eu.timepit.refined.api.Refined

/**
  * kvse: Key Value Entity Schema:
  * - keys: opaque identifiers with `Order`, `Eq`, `Hash` and `Show` (uses `refined`).
  * - values: value objects (case classes) with `Eq`, `Hash` and `Show`
  * - entities: ("aggregate roots") `Map`s of Key -> Value entries: repos, logs...
  */
package object kves {

  type OpaqueId[K, P] = Refined[K, P]

  implicit def orderId[K: Order, P]: Order[OpaqueId[K, P]] = Order by (_.value)
}

package kves {

  object OpaqueId {

    import spire.implicits._

    def apply[K: Order, P: Eq](id: K): OpaqueId[K, P] = Refined unsafeApply id

    // TODO: consider threading F[_] thru Fresh
    final case class Fresh[I](val init: I, val next: I => I)
    object Fresh {
      def apply[ID: Fresh] = implicitly[Fresh[ID]]
    }

    implicit def freshOpaqueId[K, P](implicit K: Integral[K], P: Eq[P]): Fresh[OpaqueId[K, P]] =
      Fresh(OpaqueId(K.zero), id => { import K._; OpaqueId(id.value + one) })
  }

  abstract class OpaqueIdC[K: Order, P: Eq] {
    def apply(id: K) = OpaqueId[K, P](id)
  }

  abstract class IdC[K: Order, P: Eq] {
    type Id = OpaqueId[K, P]
    object Id extends OpaqueIdC[K, P]
  }

  abstract class IdPC[K: Order, P] {
    type Id = OpaqueId[K, P]
    object Id extends OpaqueIdC[K, P]
    implicit lazy val eqP: Eq[P] = Eq.fromUniversalEquals[P]
  }

  object LongId {
    import cats.instances.long._
    def reserved[P: Eq] = OpaqueId[Long, P](Long.MinValue)
  }

  object IntId {
    import cats.instances.int._
    def reserved[P: Eq] = OpaqueId[Int, P](Int.MinValue)
  }

}
