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
sealed abstract case class SADT private (final val json: Json) {

  /** TODO: revisit this
    */
  final def canonicalString: String =
    SADT canonicalStringFor json
}

/**
  */
object SADT {

  /**
    */
  def apply(json: Json): SADT = new SADT(json) {}

  /**
    */
  def canonicalStringFor(json: Json): String =
    json.noSpacesSortKeys // - known sketchy; CBOR / `borer` maybe?

  /**
    */
  implicit class Ops[T: Decoder: Encoder](sadt: SADT) {

    /**
      */
    final def as: Decoder.Result[T] =
      sadt.json.as[T]
  }

  /**
    */
  def from[T: Decoder: Encoder](t: T): SADT =
    SADT(t.asJson)

  implicit lazy val miscEq: Eq[SADT] =
    Eq by (_.json)

  implicit lazy val miscShow: Show[SADT] =
    Show show { x =>
      s"sadt:${x.json.show}"
    }
}
