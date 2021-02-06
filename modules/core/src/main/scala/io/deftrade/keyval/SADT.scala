package io.deftrade
package keyval

import cats.implicits._
import cats.{ Eq, Show }

import io.circe.syntax._
import io.circe.{ Decoder, Encoder, Json }

/** Serialized Algebraic Data Type.
  *
  * All such semistructured data - to use an antiquated term - is encoded as `Json` (for now).
  *
  * We can derive `codec`s for arbitrary `ADT`s to and from `Json`.
  *
  * TODO: list of alternatives to examine:
  *   - [[https://cbor.io/ CBOR]]
  *   - canonical sadt
  *   - protobuf
  *   - etc
  */
sealed abstract case class SADT private (final val sadt: Json) {

  /** TODO: revisit this */
  final def canonicalString: String = sadt.noSpacesSortKeys
}

/**
  */
object SADT {

  /**
    */
  def apply(sadt: Json): SADT = new SADT(sadt) {}

  /**
    */
  implicit class Aux[T: Decoder: Encoder](sadt: SADT) {

    /**
      */
    final def as: Result[T] =
      sadt.sadt.as[T] leftMap (x => Fail fromString x.toString)
  }

  /**
    */
  def from[T: Decoder: Encoder](t: T): SADT =
    SADT(t.asJson)

  implicit lazy val miscEq: Eq[SADT]     = Eq by (_.sadt)
  implicit lazy val miscShow: Show[SADT] = Show show (_.sadt.show)
}
