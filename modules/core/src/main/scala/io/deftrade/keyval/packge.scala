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

import cats.Eq
import cats.data.NonEmptyList
import cats.effect.{ ContextShift, Sync }

import shapeless.{ HList, LabelledGeneric, Lazy }
// import shapeless.labelled._

import shapeless.syntax.singleton._

import eu.timepit.refined
import refined.api.Refined
import fs2.{ Pipe }

import io.circe.Json

import io.chrisdavenport.cormorant
import cormorant.implicits.stringPut
import cormorant.{ CSV, Error, Get, LabelledRead, LabelledWrite, Printer, Put }
// import cormorant.refined._

import io.chrisdavenport.fuuid
import fuuid.FUUID

import java.nio.file.{ Paths }

/**
  * Derived types and implicit methods for the persistence and caching of
  * domain value types (typically case classes),
  * with complementary key value store algebras and implementations.
  *
  * Defines a
  * [[https://en.wikipedia.org/wiki/Convention_over_configuration convention over configuration]]
  * system for:
  *   - `id`s: opaque Long based `id` (with `Order` instances)
  *   - `key`s: identifiers (including opaque identifiers)
  * with `Order`, and `Show` typeclass instances
  *   - `value`s: value class typeclass instances (`Eq`, `Hash` and `Show`).
  *   - etc.
  *
  * '''Convention''': A `type Foo` may not depend upon the type of the `key` for `Foo`s.
  *
  * Point being: there will be no `id: Id` fields within domain types; these will be carried
  * separately (e.g. `key`s in an in-memory [[scala.collection.Map]])
  * and will not depend in any way on the domain value types.
  *
  * However, foreign keys which reference other domain value types are permitted within value types.
  *
  * This package provides `key` and `id` implementations which abide the
  * convention given above.
  *   - aliasing `Refined` as an opaque key for a collection of a given type of values
  *   - assinging the `Value` type to be the `phantom type` parameter
  * for the `Refined` type constructor
  *   - providing the `Key` types and instances as companion base classes.
  *   - providing a `Row` type `(Key, Value)`
  *   - providing implementations of `store`a with implicit derivations
  *   for csv file readers and writers of `Row`s, thereby enabling spreadsheet integration.
  *
  * So what goes in value types? Business keys and essential attributes.
  *   - Q: What is a "business key?"
  *   - A: "Real business keys only change when the business changes!"
  *   - Dito those essential, universal, canonical attributes
  *   - everything else is `meta: Meta`
  *   - `Meta` encapsulates `io.cice.Json`
  *      - which can be stored / indexed as binary in Mongo and Postgres
  *      - which can be projected to create `satellite views`.
  *   - TODO: consider explicitly separating the structural items (keys and links between keys)
  *   from the descriptive attributes, as with
  *   [[https://en.wikipedia.org/wiki/Data_vault_modeling Data Vault]] style modelling.
  *
  * TODO: snapshots
  *
  * TODO: enhance the in-memory aspect of the `stores`.
  *
  * TODO: Postgres and Kafka integration
  *
  */
package object keyval {

  /** Just an alias.  */
  type OpaqueKey[K, V] = Refined[K, V]

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

  private[keyval] lazy val errorToFail: Error => Fail = Fail fromThrowable "csv failure"

  /**  */
  private[keyval] def printer: Printer = Printer.default

  /** */
  private val decodeFailureFromThrowable: Throwable => Error.DecodeFailure =
    fail => Error.DecodeFailure(NonEmptyList one fail.toString)

  /** */
  implicit def fuuidGet: Get[FUUID] =
    new Get[FUUID] {

      /** */
      def get(field: CSV.Field): Either[Error.DecodeFailure, FUUID] =
        FUUID fromString field.x leftMap decodeFailureFromThrowable
    }

  /** */
  implicit def fuuidPut: Put[FUUID] =
    stringPut contramap (_.show)

  /** */
  implicit def moneyGet[N: Financial, C: Currency]: Get[Mny[N, C]] =
    new Get[Mny[N, C]] {

      /** */
      def get(field: CSV.Field): Either[Error.DecodeFailure, Mny[N, C]] =
        Mny parse field.x leftMap (fail => Error.DecodeFailure(NonEmptyList one fail.toString))
    }

  /** */
  implicit def moneyPut[N: Financial, C: Currency]: Put[Mny[N, C]] =
    stringPut contramap Mny.format[N, C]

  /** */
  implicit def financialGet[N](implicit N: Financial[N]): Get[N] =
    new Get[N] {

      /** */
      def get(field: CSV.Field): Either[Error.DecodeFailure, N] =
        N parse field.x leftMap (fail => Error.DecodeFailure(NonEmptyList one fail.toString))
    }

  /** */
  implicit def financialPut[N: Financial]: Put[N] =
    stringPut contramap (Financial[N] toString _)

  /** */
  implicit lazy val jsonGet: Get[Json] =
    new Get[Json] {

      /** */
      def get(field: CSV.Field): Either[Error.DecodeFailure, Json] = ???
      // Base58 => bytes => String(bytes) => Json (via parse)
      // N parse field.x leftMap (fail => Error.DecodeFailure(NonEmptyList one fail.toString))
    }

  /**  */
  implicit lazy val jsonPut: Put[Json] = ???
  // Json => String canonical print (no spaces, sort keys - centralize!)
  // String => ByteVector (UFT-8) => String (Base58)

  /** */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def valueStore[F[_]: Sync: ContextShift]: FVS[F] = new FVS[F] {}

  /** */
  def keyValueStore[F[_]: Sync: ContextShift]: FKVS[F] = new FKVS[F] {}
}

package keyval {

  /** dsl for value stores: `at` clause */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  sealed abstract class FVS[F[_]: Sync: ContextShift] {

    /** */
    def at(p: String): FVSP[F] = new FVSP(p) {}
  }

  /** dsl for value stores: `of` clause */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  sealed abstract class FVSP[F[_]: Sync: ContextShift](p: String) {

    /** */
    def ofChainAddressed[V: Eq, HV <: HList](
        v: WithId[V],
    )(
        implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[MemFileValueStore[F, V, HV]] = Result safe {
      new MemFileValueStore(v) {

        import V._

        final override protected def tableRows = permRows

        /** */
        final override def path = Paths get p

        /** */
        final lazy val permRowToCSV: Pipe[EffectType, PermRow, String] = deriveVToCsv

        /** */
        final lazy val csvToPermRow: Pipe[EffectType, String, Result[PermRow]] = deriveCsvToV

        /** */
        final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
      }
    }

    /** */
    def ofContentAddressed[V: Eq, HV <: HList](
        v: WithId[V],
    )(
        implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]]
    ): Result[MemFileValueStore[F, V, HV]] = Result safe {
      new MemFileValueStore(v) {

        import V._

        final override protected def tableRows = permRows

        /** */
        final override def path = Paths get p

        /** */
        final lazy val permRowToCSV: Pipe[EffectType, PermRow, String] = deriveVToCsv

        /** */
        final lazy val csvToPermRow: Pipe[EffectType, String, Result[PermRow]] = deriveCsvToV

        /** FIXME implementation is wrong */
        final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
        // final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaContentAddress
      }
    }
  }

  /** dsl for key value stores: `of` clause */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  sealed abstract class FKVS[F[_]: Sync: ContextShift] {

    /** */
    def at(p: String): FKVSP[F] = new FKVSP(p) {}
  }

  /** dsl for key value stores: `of` clause */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  sealed abstract class FKVSP[F[_]: Sync: ContextShift](p: String) {

    /** */
    def ofChainAddressed[K, V: Eq, HV <: HList](
        kv: WithKey.Aux[K, V]
    )(
        implicit
        lgv: LabelledGeneric.Aux[V, HV],
        llr: Lazy[LabelledRead[HV]],
        llw: Lazy[LabelledWrite[HV]],
        lgetk: Lazy[Get[K]],
        lputk: Lazy[Put[K]]
    ): Result[MemFileKeyValueStore[F, K, V, HV]] = Result safe {
      new MemFileKeyValueStore(kv) { self =>

        import V._

        /** */
        final override protected def tableRows = rows collect {
          case (k, Some(v)) => k -> v
        }

        /** */
        final override def path = Paths get p

        /** */
        final lazy val permRowToCSV: Pipe[EffectType, PermRow, String] = deriveKvToCsv

        /** */
        final lazy val csvToPermRow: Pipe[EffectType, String, Result[PermRow]] = deriveCsvToKv

        /** */
        final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
      }
    }
  }
}
