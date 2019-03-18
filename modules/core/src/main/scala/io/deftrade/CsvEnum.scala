package io.deftrade

import enumeratum._
import cats.implicits._

import io.chrisdavenport.cormorant._
import io.chrisdavenport.cormorant.implicits._

/** Mixin csv read and write capabilities per `Enum`[EE] */
trait CsvEnum[EE <: EnumEntry] { self: Enum[EE] =>

  import CsvEnum._

  implicit lazy val get: Get[EE] = enumGet(self)
  implicit lazy val put: Put[EE] = enumPut
}

/**  Integrates Enumeratum with Cormorant (CSV) */
object CsvEnum {

  /** Use these methods to create implicits per Enum. */
  def enumGet[EE <: EnumEntry](e: Enum[EE]): Get[EE] = Get tryOrMessage (
    field => scala.util.Try { e withName field.x },
    field => s"Failed to decode Enum: $e: Received Field $field"
  )
  def enumPut[EE <: EnumEntry]: Put[EE] = stringPut contramap (_.toString)
}
