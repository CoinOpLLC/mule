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

import money._

import cats.implicits._
import cats.data.{ NonEmptyList }
import cats.effect.{ ContextShift, Sync }

import eu.timepit.refined
import refined.api.Refined

/**
  * Derived types and implicit methods for the persistence and caching of
  * domain value types (typically case classes),
  * with complementary key value store algebras and implementations.
  *
  * Defines a
  * [[https://en.wikipedia.org/wiki/Convention_over_configuration convention over configuration]]
  * system for `id`s and `key`s:
  *   - `Id`: SHA type
  *       - computed via `sha(Row)`,
  *       - immutable representation
  *       - content addressed or chained
  *   - `Key`: identifier types
  *       - domain legible (e.g. `AccountNo`)
  *       - opaque (e.g. `UUID`s)
  *   - Both `Id` and `Key` have `Order`, and `Show` typeclass instances
  *   - `Value`:
  *       - value class
  *       - typeclass instances for `Eq`, and `Show`.
  *       - Map methods enabled when `Value <~< (K2: Order, V2: Eq)`
  *
  * A `type Foo` may not contain instances of, nor even depend upon, type `Foo.Key`.
  *
  * Point being: there will be no `id: Id` or `key: Key` fields within domain types!
  * These are stored separately (e.g. `key`s in an in-memory [[scala.collection.Map]])
  *
  * However, foreign keys which reference other domain value types are permitted within value types.
  *
  * It is assumed that package clients will generally pursue
  * [[https://en.wikipedia.org/wiki/Data_vault_modeling Data Vault]] style modelling and use `hub`s
  * and `link`s to define graphs of `value types` defined by `business key`s,
  * with certain exceptions to denormalize `essential attributes`.
  *
  *   - Q: what goes in `value types`?
  *   - A: `Business keys` and `essential attributes`.
  *   - Q: What is a "business key?"
  *   - A: "Real business keys only change when the business changes!"
  *   - Q: What is an "essential attribute"?
  *   - A: Some attributes are like `business keys`: necessary everywhere in the same form
  *       - essential
  *       - universal
  *       - canonical
  *
  * Implications for `essential attribute`s.
  *   - such attributes are non-nullable
  *   - subject to tactical denormalization
  *       - deviates from strict Data Vault methodology
  *       - mixes `satelite` fields in with `link` or `hub` shaped relations
  *    - nullable / polymorphic fields are modelled as `SADT.Aux[ADT]`
  *       - `ADT` := Algebraic Data Type
  *       - [[io.circe.Json Json]] `encoder`s and `decoder`s
  *       - which can be stored / indexed as binary in Mongo and Postgres
  *       - which can be projected to create true `satellite` views.
  *
  * TODO: snapshots
  *
  * TODO: Postgres / Mongo / Kafka integration
  */
package object keyval {

  import shapeless.syntax.singleton._

  /**
    * The [[Id]] column is by convention assigned a key column label: `'id: Symbol`.
    *
    * The `id` member is a `shapeless.Aux[Symbol @@ String(id)]` instance,
    * useful for type member `T`, which is the (singleton) type of the id column label.
    */
  private[keyval] final implicit val id = Symbol("id").witness

  /**
    * [[Key]] column type literal witness - same purpose as [[id]].
    */
  private[keyval] final implicit val key = Symbol("key").witness

  /** Just an alias. */
  type OpaqueKey[K, V] = Refined[K, V]

  /**
    */
  def valueStore[F[_]: Sync: ContextShift] = VsOps[F]()

  /**
    */
  def keyValueStore[F[_]: Sync: ContextShift] = KvsOps[F]()
}

package keyval {

  import io.circe.{ Decoder, Encoder }

  import io.chrisdavenport.cormorant
  import cormorant.implicits.stringPut
  import cormorant.{ CSV, Error, Get, Printer, Put }

  import io.chrisdavenport.fuuid
  import fuuid.FUUID

  trait CsvImplicits {

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
    implicit def miscGet[T: Encoder: Decoder]: Get[SADT.Aux[T]] =
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
    implicit lazy val miscPut: Put[SADT] =
      stringPut contramap (_.canoncicalString)

  }
}
