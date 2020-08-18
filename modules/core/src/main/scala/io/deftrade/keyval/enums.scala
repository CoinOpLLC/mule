/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.deftrade
package keyval

import cats.implicits._
import cats.Order
import enumeratum.{ CatsEnum, Enum, EnumEntry }

import io.chrisdavenport.cormorant
import cormorant.{ Get, Put }
import cormorant.implicits.stringPut

/** Mixin csv read and write capabilities per `Enum`[E] */
trait CsvEnum[E <: EnumEntry] { self: Enum[E] =>

  import CsvEnum._

  /**
    */
  implicit lazy val get: Get[E] = enumGet(self)

  /**
    */
  implicit lazy val put: Put[E] = enumPut
}

/** Integrates Enumeratum with Cormorant (CSV) */
object CsvEnum {

  /** Use these methods to create implicits per Enum. */
  def enumGet[E <: EnumEntry](e: Enum[E]): Get[E] =
    Get.tryOrMessage(
      field => scala.util.Try(e withName field.x),
      field => s"Failed to decode Enum: ${e.toString}: raw was `${field.toString}`"
    )

  /**
    */
  def enumPut[E <: EnumEntry]: Put[E] = stringPut contramap (_.toString)
}

/** Fully stacc'd enum type. */
trait DtEnum[E <: EnumEntry] extends Enum[E] with CatsEnum[E] with CsvEnum[E] {

  /**
    * This is a hack to use `SortedSet`s etc
    *
    * TODO: reconsider... it is almost certainly wrong to do this, but why?
    */
  implicit val orderInstance: Order[E] = Order by (_.entryName)

  /**
    * TODO: is there a better alternative than an explicit downcast?
    * Implementation relies on reasoning about set containment and downcast safety.
    * Fragile at best.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unapply(key: EnumEntry): Option[E] =
    if (values contains key) key.asInstanceOf[E].some else none

  /**
    */
  def collect: PartialFunction[EnumEntry, E] = Function unlift unapply
}

/** namespace placeholder */
object DtEnum
