package io.deftrade
package keyval

import money._

import cats.implicits._
import cats.data.NonEmptyList

import io.chrisdavenport.cormorant
import cormorant._

import cormorant.implicits.stringPut

import io.chrisdavenport.fuuid
import fuuid.FUUID

import io.circe.{ Decoder, Encoder }

protected trait CsvImplicits {
  private[keyval] lazy val errorToFail: Error => Fail = Fail fromThrowable "csv failure"

  /**
    */
  private[keyval] def printer: Printer = Printer.default

  /**
    */
  private val toDecodeFailure: Throwable => Error.DecodeFailure =
    fail => Error.DecodeFailure(NonEmptyList one fail.toString)

  /**
    */
  implicit def fuuidGet: Get[FUUID] =
    new Get[FUUID] {

      /**
        */
      def get(field: CSV.Field): Either[Error.DecodeFailure, FUUID] =
        FUUID fromString field.x leftMap toDecodeFailure
    }

  /**
    */
  implicit def fuuidPut: Put[FUUID] =
    stringPut contramap (_.show)

  /**
    */
  implicit def moneyGet[N: Financial, C: Currency]: Get[Mny[N, C]] =
    new Get[Mny[N, C]] {

      /**
        */
      def get(field: CSV.Field): Either[Error.DecodeFailure, Mny[N, C]] =
        Mny parse field.x leftMap toDecodeFailure
    }

  /**
    */
  implicit def moneyPut[N: Financial, C: Currency]: Put[Mny[N, C]] =
    stringPut contramap Mny.format[N, C]

  /**
    */
  implicit def financialGet[N](implicit N: Financial[N]): Get[N] =
    new Get[N] {

      /**
        */
      def get(field: CSV.Field): Either[Error.DecodeFailure, N] =
        N parse field.x leftMap toDecodeFailure
    }

  /**
    */
  implicit def financialPut[N: Financial]: Put[N] =
    stringPut contramap (Financial[N] toString _)

  /**
    */
  implicit def sadtGet[T: Encoder: Decoder]: Get[SADT.Aux[T]] =
    new Get[SADT.Aux[T]] {

      import io.circe.parser._

      /**
        */
      def get(field: CSV.Field): Either[Error.DecodeFailure, SADT.Aux[T]] =
        for {
          json <- parse(field.x) leftMap toDecodeFailure
        } yield SADT[T](json)
    }

  /**
    */
  implicit lazy val sadtPut: Put[SADT] =
    stringPut contramap (_.canonicalString)

}

/**
  */
object CsvImplicits extends CsvImplicits
