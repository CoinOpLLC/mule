package io.deftrade
package model

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semi }

import io.circe.syntax._
import io.circe.{ Decoder, Encoder, Json }

import io.chrisdavenport.cormorant
import cormorant.implicits.stringPut
import cormorant.{ CSV, Error, Get, Put }

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
  def apply[T: Encoder: Decoder](json: Json): Misc = new Aux[T](json) {}
  def apply(json: Json): Misc                      = ???

  /** */
  def of[T: Encoder: Decoder](t: T): Misc =
    apply[T](t.asJson)

  implicit lazy val miscEq: Eq[Misc]     = ??? // { import auto.eq._; semi.eq }
  implicit lazy val miscShow: Show[Misc] = ??? // { import auto.show._; semi.show }

  /** FIXME: need [T] here? makes this a `def` */
  implicit lazy val miscGet: Get[Misc] =
    new Get[Misc] {

      /** */
      def get(field: CSV.Field): Either[Error.DecodeFailure, Misc] = ???
    }

  /**  */
  implicit lazy val miscPut: Put[Misc] =
    stringPut contramap (_.json.toString)

}
