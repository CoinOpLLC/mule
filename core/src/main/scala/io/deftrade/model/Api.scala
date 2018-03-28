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
package model

import cats.Eq
// import cats.implicits._

import enumeratum._
// import enumeratum.values._

/**
  * This shall be the law of the Api: A `type Foo` may not depend on a `type FooId`.
  */
trait Api {

  sealed trait Asset

  sealed trait Role extends EnumEntry
  object Role extends Enum[Role] {

    /**
      * The name on the account.
      */
    case object Principle extends Role

    /**
      * Responsible for the disposition of assets in the Folio.
      */
    case object Manager extends Role

    /**
      * Semantics for `Subject` are conditioned on the status of account:
      * responsible party (liability)
      *  beneficial owner (asset)
      */
    case object Subject extends Role
    lazy val values = findValues
  }

  trait Id[T] extends Any { def id: T }

}

object impl {

  final case class GenericId[T, P](val id: T) extends AnyVal with Id[T]
  object GenericId {
    implicit def eq[T: Eq, P]: Eq[GenericId[T, P]] = Eq by (_.id)
  }

  object LongId {
    def reserved[P] = GenericId[Long, P](Long.MinValue)
  }

  object IntId {
    def reserved[P] = GenericId[Int, P](Int.MinValue)
  }

}
