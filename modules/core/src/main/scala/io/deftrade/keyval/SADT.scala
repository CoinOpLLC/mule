package io.deftrade
package keyval

import cats.implicits._
import cats.{ Eq, Show }

import io.circe.syntax._
import io.circe.{ parser, Decoder, Encoder, Json }

/** Serialized Algebraic Data Type.
  *
  * All such semistructured data - to use an antiquated term - is encoded as `json` (for now).
  *
  * We can derive `codec`s for arbitrary `ADT`s to and from `json`.
  *
  * TODO: list of alternatives to examine:
  *   - [[https://cbor.io/ CBOR]]
  *   - canonical json
  *   - protobuf
  *   - etc
  */
sealed abstract class SADT private (protected val json: Json) {

  /**
    */
  type ADT

  /**
    */
  def decoded: Result[ADT]

  /**
    */
  def canonicalString: String
}

/**
  */
object SADT {

  /**
    */
  sealed abstract case class Aux[T: Decoder: Encoder](
      final override protected val json: Json
  ) extends SADT(json) {

    /**
      */
    final type ADT = T

    /**
      */
    final def decoded: Result[ADT] =
      json.as[ADT] leftMap (x => Fail fromString x.toString)

    /** TODO: revisit this */
    final def canonicalString: String = json.noSpacesSortKeys
  }

  /** More D&D than type evasion. TODO: revisit this
    */
  def cast[T: Decoder: Encoder](json: Json): SADT.Aux[T] =
    new Aux[T](json) {}

  /**
    */
  def from[T: Decoder: Encoder](t: T): SADT.Aux[T] =
    new Aux[T](t.asJson) {}

  // private val Right(braces) = parser parse "{}"

  // // FIXME decide if this is the way or not
  // private[keyval] def empty[T]: SADT.Aux[T] = apply(braces)

  implicit lazy val miscEq: Eq[SADT]     = Eq by (_.json)
  implicit lazy val miscShow: Show[SADT] = Show show (_.json.show)
}
