package io.deftrade

/**
 Opaque means opaque.
  */
package opaqueid

import cats.{ Order }
import spire.math.Integral

sealed trait IdTypeTraits extends Any {
  type KeyType
  type PhantomType
}

sealed trait Id[K] extends Any with IdTypeTraits {
  final type KeyType = K
  def id: K
}
object Id

final case class OpaqueId[K, P] private (val id: K) extends AnyVal with Id[K] {
  final type PhantomType = P
}

sealed trait Fresh[I] {
  def init: I
  def next: I => I
}

case class StrictFresh[I](val init: I, val next: I => I) extends Fresh[I]

object Fresh {

  def apply[ID: Fresh] = implicitly[Fresh[ID]]

}
object OpaqueId {

  implicit def order[K: Order, P]: Order[OpaqueId[K, P]] = Order by (_.id)

  implicit def freshOpaqueId[K: Integral, P]: Fresh[OpaqueId[K, P]] = new Fresh[OpaqueId[K, P]] {
    type Id = OpaqueId[K, P]
    private val K = Integral[K]
    import K.{ one, plus, zero } // TODO: infix?
    def init: Id       = OpaqueId(zero)
    def next: Id => Id = id => OpaqueId(plus(id.id, one))
  }

}

abstract class OpaqueIdC[OIDT <: IdTypeTraits](implicit ev: cats.Order[OIDT#KeyType]) {
  ev |> discardValue
  def apply(k: OIDT#KeyType) = OpaqueId[OIDT#KeyType, OIDT#PhantomType](k)
}

abstract class IdC[N: cats.Order, P: cats.Eq] {
  type Id = OpaqueId[N, P]
  object Id extends OpaqueIdC[Id]
}

abstract class IdC2[N: cats.Order, P] {
  type Id = OpaqueId[N, P]
  object Id extends OpaqueIdC[Id]
  implicit def eq: cats.Eq[P]
}

object LongId {
  def reserved[P] = OpaqueId[Long, P](Long.MinValue)
}

object IntId {
  def reserved[P] = OpaqueId[Int, P](Int.MinValue)
}
