package io.deftrade

/**
 Leave these here. Opaque means opaque.
  */
package opaqueid {

  import cats.{ Order }

  sealed trait IdTypeTraits extends Any {
    type KeyType
    type PhantomType
  }

  sealed trait Id[K] extends Any with IdTypeTraits {
    final type KeyType = K
    def id: K
  }

  final case class OpaqueId[K, P] private (val id: K) extends AnyVal with Id[K] {
    final type PhantomType = P
  }

  object OpaqueId {
    implicit def order[K: Order, P]: Order[OpaqueId[K, P]] = Order by (_.id)
  }

  abstract class OpaqueIdC[OIDT <: IdTypeTraits](implicit ev: cats.Order[OIDT#KeyType]) {
    ev |> discardValue
    def apply(k: OIDT#KeyType) = OpaqueId[OIDT#KeyType, OIDT#PhantomType](k)
  }

  abstract class IdC[N: cats.Order, P: cats.Eq] {
    type Id = OpaqueId[N, P]
    object Id extends OpaqueIdC[Id]
  }

  object LongId {
    def reserved[P] = OpaqueId[Long, P](Long.MinValue)
  }

  object IntId {
    def reserved[P] = OpaqueId[Int, P](Int.MinValue)
  }

}
