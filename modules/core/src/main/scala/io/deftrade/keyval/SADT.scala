package io.deftrade
package keyval

import cats.implicits._
import cats.{ Eq, Show }

import io.circe.syntax._
import io.circe.{ Decoder, Encoder, Json }

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
  final def decoded(implicit ev: Decoder[ADT]): Result[ADT] =
    json.as[ADT] leftMap (x => Fail fromString x.toString)

  /** TODO: revisit this */
  final def canonicalString: String = json.noSpacesSortKeys
}

/**
  */
object SADT {

  implicit def sadtShow[T]: Show[Aux[T]] = ???

  /**
    */
  sealed abstract case class Aux[T](final override protected val json: Json) extends SADT(json) {

    /**
      */
    final type ADT = T
  }

  /**
    */
  private[deftrade] def apply[T](json: Json): Aux[T] = new Aux[T](json) {}

  /**
    */
  def from[T: Encoder](t: T): Aux[T] = apply(t.asJson)

  private val Right(braces) = io.circe.parser parse "{}"

  // FIXME decide if this is the way or not
  private[keyval] def empty[T]: SADT.Aux[T] = apply(braces)

  implicit lazy val miscEq: Eq[SADT]     = Eq by (_.json)
  implicit lazy val miscShow: Show[SADT] = Show show (_.json.show)
}

/** A `SADT` store is content-addressed: entries are indexed with their own `sha`.
  *
  * Therefore, if you have a `sha` (from a [[model.Transaction]], for instance) ''and'' access to
  * a `SADT` key value store containing the value, you have access to the value itself.
  *
  * Note this value is forgery resistant (up to the strength of the `sha`).
  */
abstract class WithSADT[T: Show] extends WithId.Aux[SADT.Aux[T]] {

  /** Every `ADT` shall decode `braces` as the legal and unique `empty: T`
    * FIXME: test this lmao
    */
  final def empty: T = {
    val Right(ret) = SADT.empty[T].decoded
    ret
  }

  /** TODO: review lawful evil implementation
    */
  final def from(sadt: SADT.Aux[T]): T = {
    val Right(ret) = sadt.decoded
    ret
  }

  implicit def decoder: Decoder[T]
  implicit def encoder: Encoder[T]
}
