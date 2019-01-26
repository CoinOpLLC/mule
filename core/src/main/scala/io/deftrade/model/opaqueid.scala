package io.deftrade

import cats.{ Eq, Order }
import spire.math.Integral
import eu.timepit.refined.api.Refined

/**
 And by `opaque` we mean opaque.
  */
object opaqueid {

  final type OpaqueId[K, P] = Refined[K, P]

  object OpaqueId {

    def apply[K: Order, P: Eq](id: K): OpaqueId[K, P] = Refined unsafeApply id

    trait Fresh[I] {
      def init: I
      def next: I => I
    }

    object Fresh {

      def apply[ID: Fresh] = implicitly[Fresh[ID]]

    }

    implicit def freshOpaqueId[K: Integral, P: Eq]: Fresh[OpaqueId[K, P]] = new Fresh[OpaqueId[K, P]] {
      private val K = Integral[K]
      import K._ // { one, plus, zero } // TODO: infix?
      import spire.implicits._
      type Id = OpaqueId[K, P]
      def init: Id       = OpaqueId(zero)
      def next: Id => Id = id => OpaqueId(id.value + one)
    }

  }
  case class StrictFresh[I](val init: I, val next: I => I) extends OpaqueId.Fresh[I]

  abstract class OpaqueIdC[K: Order, P: Eq] {
    def apply(id: K) = OpaqueId[K, P](id)
  }

  abstract class IdC[K: Order, P: Eq] {
    type Id = OpaqueId[K, P]
    object Id extends OpaqueIdC[K, P]

  }

  abstract class IdPC[K: Order, P] {

    type Id = OpaqueId[K, P]
    object Id extends OpaqueIdC[K, P] {}

    implicit lazy val orderId: Order[Id] = Order by (_.value)
    implicit lazy val eqP: Eq[P]         = Eq.fromUniversalEquals[P]
  }

  type IdC2[K, P] = IdC[K, P]

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
