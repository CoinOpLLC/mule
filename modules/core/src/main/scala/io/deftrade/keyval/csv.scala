package io.deftrade
package keyval

import money._

import cats.implicits._
import cats.data.NonEmptyList

import enumeratum.{ CatsEnum, Enum, EnumEntry }

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.implicits._

/** module mixin */
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

  /** Mixin csv read and write capabilities per `Enum`[E] */
  trait CsvEnum[E <: EnumEntry] { self: Enum[E] =>

    import CsvEnum._

    /** */
    implicit lazy val get: Get[E] = enumGet(self)

    /** */
    implicit lazy val put: Put[E] = enumPut
  }

  /**  Integrates Enumeratum with Cormorant (CSV) */
  object CsvEnum {

    /** Use these methods to create implicits per Enum. */
    def enumGet[E <: EnumEntry](e: Enum[E]): Get[E] = Get tryOrMessage (
      field => scala.util.Try { e withName field.x },
      field => s"Failed to decode Enum: $e: Received $field"
    )

    /** */
    def enumPut[E <: EnumEntry]: Put[E] = stringPut contramap (_.toString)
  }

  /** Fully stacc'd enum type. */
  trait DtEnum[E <: EnumEntry] extends Enum[E] with CatsEnum[E] with CsvEnum[E] {

    /**
      * TODO:
      * Implementation relies on reasoning about set containment and downcast safety.
      * Warrents extreme vetting.
      */
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def unapply(key: EnumEntry): Option[E] =
      if (values contains key) key.asInstanceOf[E].some else none

    /** */
    def collect: PartialFunction[EnumEntry, E] = Function unlift unapply
  }

  /** namespace placeholder */
  object DtEnum
}

/** Standalone modulette. */
object csv extends csv
