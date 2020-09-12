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
import cats.evidence._
import cats.{ Order, Show }
import cats.data.{ NonEmptyList, NonEmptyMap }

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
private[keyval] sealed trait VsParam

/**
  */
private[keyval] object VsParam {
  case object V        extends VsParam
  case object LV       extends VsParam
  case object MK2V2    extends VsParam
  case object MK2LV2   extends VsParam
  case object NELV     extends VsParam
  case object NEMK2V2  extends VsParam
  case object NEMK2LV2 extends VsParam
}

/**
  */
sealed trait WithId extends WithValue {

  /**
    */
  final type Row = Value

  /**
    */
  sealed trait VsSpec {

    val param: VsParam

    type Kappa

    type Vega

    type Spec

    type ValueSpec

    type Store[F[_]] <: ValueStore.Aux[F, ValueSpec]

    implicit def AsV: ValueSpec === Value
  }

  /**
    */
  object VsSpec {

    sealed abstract class Aux[K2, V2](final override val param: VsParam) extends VsSpec {
      final type Kappa = K2
      final type Vega  = V2
    }
  }

  def v(implicit asV: Value === Value): VsSpec.Aux[Nothing, Value] =
    new VsSpec.Aux[Nothing, Value](VsParam.V) {
      final type Spec        = Vega
      final type ValueSpec   = Vega
      final type Store[F[_]] = ValueStore.Aux[F, ValueSpec] with ValueStoreV[F, Vega]
      final implicit def AsV = asV
    }

  def lv[V2](implicit asV: Option[V2] === Value): VsSpec =
    new VsSpec.Aux[Nothing, V2](VsParam.LV) {
      final type Spec        = List[Vega]
      final type ValueSpec   = Option[Vega]
      final type Store[F[_]] = ValueStore.Aux[F, ValueSpec] with ValueStoreLV[F, Vega]
      final implicit def AsV = asV
    }

  def mk2v2[K2, V2](implicit asV: Option[(K2, V2)] === Value): VsSpec =
    new VsSpec.Aux[K2, V2](VsParam.MK2V2) {
      final type Spec        = Map[Kappa, Vega]
      final type ValueSpec   = Option[(Kappa, Vega)]
      final type Store[F[_]] = ValueStore.Aux[F, ValueSpec] with ValueStoreMK2V2[F, Kappa, Vega]
      final implicit def AsV = asV
    }

  def mk2lv2[K2, V2](implicit asV: Option[(K2, Option[V2])] === Value): VsSpec =
    new VsSpec.Aux[K2, V2](VsParam.MK2LV2) {
      final type Spec        = Map[Kappa, List[Vega]]
      final type ValueSpec   = Option[(Kappa, Option[Vega])]
      final type Store[F[_]] = ValueStore.Aux[F, ValueSpec] with ValueStoreMK2LV2[F, Kappa, Vega]
      final implicit def AsV = asV
    }

  def nelv(implicit asV: Value === Value): VsSpec =
    new VsSpec.Aux[Nothing, Value](VsParam.NELV) {
      final type Spec        = NonEmptyList[Vega]
      final type ValueSpec   = Vega
      final type Store[F[_]] = ValueStore.Aux[F, ValueSpec] with ValueStoreNELV[F, Vega]
      final implicit def AsV = asV
    }

  def nemk2v2[K2, V2](implicit asV: (K2, V2) === Value): VsSpec =
    new VsSpec.Aux[K2, V2](VsParam.NEMK2V2) {
      final type Spec        = NonEmptyMap[Kappa, Vega]
      final type ValueSpec   = (Kappa, Vega)
      final type Store[F[_]] = ValueStore.Aux[F, ValueSpec] with ValueStoreNEMK2V2[F, Kappa, Vega]
      final implicit def AsV = asV
    }

  def nemk2lv2[K2, V2](implicit asV: (K2, Option[V2]) === Value): VsSpec =
    new VsSpec.Aux[K2, V2](VsParam.NEMK2LV2) {
      final type Spec        = NonEmptyMap[Kappa, List[Vega]]
      final type ValueSpec   = (Kappa, Option[Vega])
      final type Store[F[_]] = ValueStore.Aux[F, ValueSpec] with ValueStoreNEMK2LV2[F, Kappa, Vega]
      final implicit def AsV = asV
    }
}

/**
  */
object WithId {

  /**
    */
  abstract class Aux[V] extends WithValue.Aux[V] with WithId
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
