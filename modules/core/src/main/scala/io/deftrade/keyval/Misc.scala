package io.deftrade
package keyval

import cats.implicits._
import cats.{ Eq, Show }
import cats.data.NonEmptyList
import cats.derived.{ auto, semi }

import io.circe.syntax._
import io.circe.{ Decoder, Encoder, Json }

/**
  * All `misc` data - semistructured, to use an antiquated term - is `json` (for now).
  *
  * We can derive `codec`s for arbitrary `ADT`s to and from `json`.
  */
sealed class Misc private (protected val json: Json) {

  /** */
  type ADT

  /** */
  final def decoded(implicit ev: Decoder[ADT]): Result[ADT] =
    json.as[ADT] leftMap (x => Fail.fromString(x.toString))

  final def canoncicalString: String = json.noSpacesSortKeys
}

/**
  * A `Meta` store is content-addressed: entries are indexed with their own `Sha`.
  *
  * Therefore, if you have the Sha (from a [[Transaction]], for instance) ''and'' access to
  * a `Meta` key value store containing the value, you have access to the value itself.
  *
  * Note this value is forgery resistant (up to the strength of the `Sha`).
  */
@SuppressWarnings(Array("org.wartremover.warts.Any"))
object Misc {

  /** */
  sealed abstract case class Aux[T](final override protected val json: Json) extends Misc(json) {
    final type ADT = T
  }

  /** */
  def from[T: Encoder: Decoder](json: Json): Misc.Aux[T] = new Aux[T](json) {}

  /** */
  def of[T: Encoder: Decoder](t: T): Misc = from[T](t.asJson)

  implicit lazy val miscEq: Eq[Misc]     = Eq by (_.json)
  implicit lazy val miscShow: Show[Misc] = Show show (_.json.show)
}
