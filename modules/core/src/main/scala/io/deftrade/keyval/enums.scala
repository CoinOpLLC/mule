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
import cats.{ Hash, Order, Show }
import enumeratum.{ Enum, EnumEntry }

import io.chrisdavenport.cormorant
import cormorant.{ Get, Put }
import cormorant.implicits.stringPut

sealed trait KittehEnum[E <: EnumEntry] { self: Enum[E] =>

  private val name: E => String = _.entryName

  /** `Hash` `Enum`s by `entryName`, consistent with universal `Eq`uals.
    */
  implicit final val enumHash: Hash[E] =
    Hash by name

  /** `Order` `Enum`s by `entryName`, consistent with universal `Eq`uals.
    */
  implicit final val enumOrder: Order[E] =
    Order by name

  /** `Show` `Enum`s by `entryName`, consistent with universal `Eq`uals.
    */
  implicit final val enumShow: Show[E] =
    Show show name
}

sealed trait CsvEnum[E <: EnumEntry] { self: Enum[E] with KittehEnum[E] =>

  /** `Get` an `Enum` `withName`.
    */
  implicit final lazy val enumGet: Get[E] =
    Get tryOrMessage (
      field => scala.util.Try(self withName field.x),
      field => s"Failed to decode supposed Enum: `${self.toString}`: raw was `${field.toString}`"
  )

  /** To `Put` is to `Show`.
    */
  implicit final lazy val enumPut: Put[E] =
    Put[String] contramap (_.show)
}

/** The stacc'd enum type de maison.
  */
trait DtEnum[E <: EnumEntry] extends Enum[E] with KittehEnum[E] with CsvEnum[E]

object DtEnum
