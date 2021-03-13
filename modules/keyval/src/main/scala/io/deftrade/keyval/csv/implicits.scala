package io.deftrade
package keyval.csv

import keyval.SADT
// import money._

import cats.implicits._
import cats.data.NonEmptyList

// import io.circe.{ parser, Decoder, Encoder }
import io.circe.{ parser, Json }

import io.chrisdavenport.{ cormorant, fuuid }

import fuuid.FUUID

import cormorant.implicits.stringPut
import cormorant._

trait implicits {

  /**
    */
  implicit lazy val fuuidGet: Get[FUUID] =
    new Get[FUUID] {

      /**
        */
      def get(field: CSV.Field): Either[Error.DecodeFailure, FUUID] =
        FUUID fromString field.x leftMap toDecodeFailure
    }

  /**
    */
  implicit lazy val fuuidPut: Put[FUUID] =
    stringPut contramap (_.show)

  /**
    */
  implicit lazy val jsonGet: Get[Json] =
    new Get[Json] {

      def get(field: CSV.Field): Either[Error.DecodeFailure, Json] =
        for {
          json <- parser parse field.x leftMap toDecodeFailure
        } yield json
    }

  /**
    */
  implicit lazy val jsonPut: Put[Json] =
    stringPut contramap (SADT canonicalStringFor _)

  /**
    */
  protected val toDecodeFailure: Throwable => Error.DecodeFailure =
    fail => Error.DecodeFailure(NonEmptyList one fail.toString)
}

/** You shouldn't need this.
  */
object implicits extends implicits
