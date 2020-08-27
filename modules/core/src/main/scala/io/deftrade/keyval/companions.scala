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

import refinements.Sha

import cats.implicits._
import cats.{ Order, Show }

import eu.timepit.refined
import refined.refineV
import refined.api.{ Min, Refined, Validate }

import spire.implicits._

import io.chrisdavenport.fuuid.FUUID

import shapeless.labelled._

import java.util.UUID

/**
  */
object OpaqueKey {

  /**
    */
  private[keyval] def apply[K: Order: Show, V](k: K): OpaqueKey[K, V] = Refined unsafeApply k

  /**
    */
  def unsafe[K: Order: Show, V](k: K): OpaqueKey[K, V] = apply(k)

  /** FIXME questionable */
  implicit def validate[K: Order: Show, V]: Validate[K, V] = Validate alwaysPassed (())
}

/**
  * Defines `Id` and other persistence helpers for a given value class `V`.
  *
  * Note: consequence of this design is that there is only one `Key` type per value object type.
  *
  * TODO revisit this decision and its implication.
  */
protected sealed trait WithValue {

  /**
    */
  type Value

  /** `Id`s are all secure hashes of some kind
    */
  final type Id = Sha

  /** The full type of the [[Id]] column.
    */
  final type IdField = FieldType[id.T, Id]

  /**
    * Think spreadsheet or relational table,
    * keeping in mind that [[Value]]s are can be, and often are, compound.
    */
  type Row
}

/**
  */
protected object WithValue {

  /**
    * The type of the underlying record being indexed.
    */
  sealed abstract class Aux[V] extends WithValue {

    /**
      */
    final type Value = V
  }
}

/**
  */
sealed trait WithId extends WithValue {

  /**
    */
  final type Row = Value
}

/**
  */
object WithId {

  /**
    */
  abstract class Aux[V] extends WithValue.Aux[V] with WithId {

    /**
      */
    final type Store[F[_]]  = ValueStoreV[F, V] with Store.AuxV[F, WithId.Aux, V]
    final type StoreM[F[_]] = ValueStoreM[F, V] with Store.AuxL[F, WithId.Aux, V]
  }
}

/**
  */
trait WithKey extends WithValue {

  /**
    * `Foo` tables are indexed by `Foo.Key`s
    */
  type Key

  /** Known accomplices. */
  val Key: WithKey.KeyCompanion[Key]

  /** Absence of a `Value` indicates an intent to delete the `Row` indexed by `Key`. */
  final type Row = (Key, Option[Value])

  /** The full type of the [[Key]] column. */
  final type KeyField = FieldType[key.T, Key]
}

/**
  */
object WithKey {

  /** The `Key` type member is assigned type parameter `K`. */
  abstract class Aux[K, V] extends WithValue.Aux[V] with WithKey {

    /**
      */
    final type Key = K

    /**
      */
    final type Store[F[_]] = KeyValueStoreV[F, Key, Value]
  }

  /** Key type companion base class. */
  abstract class KeyCompanion[K] {

    /**
      */
    implicit def order: Order[K]

    /**
      */
    implicit def show: Show[K]
  }

  /** Companion mez class for `Refined` key types. */
  abstract class RefinedKeyCompanion[K: Order: Show, P] extends KeyCompanion[Refined[K, P]] {

    /**
      */
    def apply(k: K)(implicit ev: Validate[K, P]): Result[Refined[K, P]] =
      refineV[P](k) leftMap Fail.fromString

    /** for testability */
    def unsafe(k: K): Refined[K, P] = Refined unsafeApply k

    /**
      */
    final override implicit def order = Order[Refined[K, P]]

    import refined.cats._ // FIXME: why?

    /** nb `Show` is inferred for _all_ `OpaqueKey[K: Show, V]` (unquallified for V) */
    final override implicit def show = Show[Refined[K, P]]

    /** Where the key type is integral, we will reserve the min value. */
    def reserved(implicit K: Min[K]): Refined[K, P] = Refined unsafeApply K.min
  }
}

/**
  * Companion base class which defines a key as a `Refined`
  * type, parameterized with the value type we are indexing.
  */
abstract class WithOpaqueKey[K: Order: Show, V] extends WithRefinedKey[K, V, V]

/**
  * Phantom type used to tag the key, which has type K as its underlying representation.
  * This can either be a trivial tag which encodes the independance of a key from the record
  * that it indexes, or, some other kind of constraint (i.e. a `Predicate`).
  */
abstract class WithRefinedKey[K: Order: Show, P, V] extends WithKey.Aux[Refined[K, P], V] {

  /**
    */
  object Key extends WithKey.RefinedKeyCompanion[K, P]
}

/**
  * Companion supplying a [[java.util.UUID]] as `Key`.
  *
  * Note we use [[io.chrisdavenport.fuuid.FUUID FUUID]]s to (functionally) wrap the `UUID`s.
  */
abstract class WithFuuidKey[V] extends WithKey.Aux[FUUID, V] {

  /**
    */
  object Key extends WithKey.KeyCompanion[FUUID] {

    /**
      */
    def random: Key = FUUID fromUUID UUID.randomUUID

    /**
      */
    implicit def order = Order[FUUID]

    /**
      */
    implicit def show = Show[FUUID]
  }
}
