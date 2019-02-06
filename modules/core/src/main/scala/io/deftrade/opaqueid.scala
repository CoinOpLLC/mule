package io.deftrade

import cats.{ Eq, Order }
import spire.math.Integral
import eu.timepit.refined.api.Refined

/**
  * And by `opaque` we mean opaque.
  */
object opaqueid {

  final type OpaqueId[K, P] = Refined[K, P]

  object OpaqueId {

    import spire.implicits._

    def apply[K: Order, P: Eq](id: K): OpaqueId[K, P] = Refined unsafeApply id

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

  implicit def orderId[K: Order, P]: Order[OpaqueId[K, P]] = Order by (_.value)

  object LongId {
    import cats.instances.long._
    def reserved[P: Eq] = OpaqueId[Long, P](Long.MinValue)
  }

  object IntId {
    import cats.instances.int._
    def reserved[P: Eq] = OpaqueId[Int, P](Int.MinValue)
  }

}
