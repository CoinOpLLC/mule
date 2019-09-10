package io.deftrade
package keyval

import money._

import cats.implicits._
import cats.data.NonEmptyList

import enumeratum.{ CatsEnum, Enum, EnumEntry }

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.implicits._

/** package mixin */
trait csv {

  /** cormorant csv `Get` */
  implicit def moneyGet[N: Financial, C: Currency]: Get[Money[N, C]] = new Get[Money[N, C]] {

    /** */
    def get(field: CSV.Field): Either[Error.DecodeFailure, Money[N, C]] =
      Money parse field.x leftMap (fail => Error.DecodeFailure(NonEmptyList one fail.toString))
  }

  /** cormorant csv `Put` */
  implicit def moneyPut[N: Financial, C: Currency]: Put[Money[N, C]] =
    stringPut contramap Money.format[N, C]

  /** cormorant csv `Get` */
  implicit def financialGet[N](implicit N: Financial[N]): Get[N] = new Get[N] {

    /** */
    def get(field: CSV.Field): Either[Error.DecodeFailure, N] =
      N parse field.x leftMap (fail => Error.DecodeFailure(NonEmptyList one fail.toString))
  }

  /** cormorant csv `Put` */
  implicit def financialPut[N: Financial]: Put[N] =
    stringPut contramap (Financial[N] toString _)
}

/** Standalone modulette. */
object csv extends csv

/** Mixin csv read and write capabilities per `Enum`[EE] */
trait CsvEnum[EE <: EnumEntry] { self: enumeratum.Enum[EE] =>

  import CsvEnum._

  /** */
  implicit lazy val get: Get[EE] = enumGet(self)

  /** */
  implicit lazy val put: Put[EE] = enumPut
}

/**  Integrates Enumeratum with Cormorant (CSV) */
object CsvEnum {

  /** Use these methods to create implicits per Enum. */
  def enumGet[EE <: EnumEntry](e: enumeratum.Enum[EE]): Get[EE] = Get tryOrMessage (
    field => scala.util.Try { e withName field.x },
    field => s"Failed to decode Enum: $e: Received $field"
  )
  def enumPut[EE <: EnumEntry]: Put[EE] = stringPut contramap (_.toString)
}

/** Fully stacc'd enum type. */
trait DtEnum[EE <: EnumEntry] extends Enum[EE] with CatsEnum[EE] with CsvEnum[EE] {

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unapply(key: EnumEntry): Option[EE] =
    if (values contains key) key.asInstanceOf[EE].some else none
}

/** */
