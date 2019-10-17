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

// import cats.implicits._
import cats.{ Eq, Order }

import eu.timepit.refined
import refined.api.{ Min, Refined, Validate }

import spire.implicits._

import shapeless.labelled._

/** */
object OpaqueKey {

  /** */
  private[keyval] def apply[K: Order, V](k: K): OpaqueKey[K, V] = Refined unsafeApply k

  /** */
  def unsafe[K: Order, V](k: K): OpaqueKey[K, V] = apply(k)

  /** */
  implicit def validate[K: Order, V]: Validate[K, V] = Validate alwaysPassed (())
}

/** Key type companion base class. */
abstract class KeyCompanion[K] {

  /** */
  implicit def order: Order[K]
}

/** Companion mez class for `Refined` key types. */
abstract class RefinedKeyCompanion[K: Order, P] extends KeyCompanion[Refined[K, P]] {

  /** */
  implicit def order = Order[Refined[K, P]]

  /** */
  // def apply(k: K): Refined[K, P] = Refined unsafeApply k

  /** Where the key type is integral, we will reserve the min value. */
  def reserved(implicit K: Min[K]): Refined[K, P] = Refined unsafeApply K.min
}

/**
  * Defines `Id` and other persistence helpers for a given value class `V`.
  */
sealed trait WithValue {

  /** */
  type Value

  /** TODO: this is sketchy */
  implicit lazy val eqValue: Eq[Value] = Eq.fromUniversalEquals[Value]

  /** */
  // implicit lazy val showValue: Show[V] = cats.derived.semi.show

  /** A permanent identifier (eg auto-increment in a db col) */
  final type Id = OpaqueKey[Long, Value]

  /** namespace placeholder */
  object Id

  /**
    * Think spreadsheet or relational table,
    * keeping in mind that [[Value]]s are can be, and often are, compound.
    */
  type Row

  /** */
  final type PermRow = (Id, Row)

  /** Will be assigned either Id or Key. */
  type Index

  /** The full type of the [[Id]] column. */
  final type IdField = FieldType[id.T, Id]
}

/** */
object WithValue {

  /**
    * The type of the underlying record being indexed.
    */
  sealed abstract class Aux[V] extends WithValue {
    final type Value = V
  }
}

/** */
trait WithId extends WithValue {

  /** */
  final type Row = Value

  /** */
  final type Index = Id
}

/** */
object WithId {

  /**
    * Companion object base class.
    *
    * Note: consequence of this design is that there is only one `Key` type per value object type.
    *
    * TODO revisit this decision and its implication.
    */
  trait Aux[V] extends WithValue.Aux[V]
}

/**
  * Companion object base class.
  */
trait WithKey extends WithValue {

  /**
    * So `Foo`s are indexed with `Foo.Key`s
    */
  type Key

  /** Known accomplices. */
  val Key: KeyCompanion[Key]

  /** Think spreadsheet or relational table, keeping in mind that [[Value]]s are compound. */
  final type Row = (Key, Value)

  /** */
  final type Index = Key

  /** The full type of the [[Key]] column. */
  final type KeyField = FieldType[key.T, Key]
}

/** */
object WithKey {

  /** The `Key` type is assinged `K`. */
  abstract class Aux[K, V] extends WithValue.Aux[V] with WithKey {
    final type Key = K
  }
}

/**
  * Phantom type used to tag the key, which has type K as its underlying representation.
  * This can either be a trivial tag which encodes the independance of a key from the record
  * that it indexes, or, some other kind of constraint (i.e. a `Predicate`).
  */
abstract class WithRefinedKey[K: Order, P, V] extends WithKey.Aux[Refined[K, P], V] {

  /** */
  object Key extends RefinedKeyCompanion[K, P]
}

/**
  * Companion base class which defines a key as a `Refined`
  * type, parameterized with the value type we are indexing.
  */
abstract class WithOpaqueKey[K: Order, V] extends WithRefinedKey[K, V, V]

// /** */
// sealed abstract case class Key[K] private (k: K)
//
// /** */
// object Key {
//
//   /** */
//   def apply[K](k: K): Key[K] = new Key(k) {}
// }
//
// /** When you want a case class as a `Key`. */
// abstract class WithAdtKey[K: Order, V] extends WithKey.Aux[Key[K], V] {
//
//   /** */
//   object Key extends KeyCompanion[Key] {
//
//     /** */
//     override implicit def order: Order[Key] = Order by (_.k)
//   }
// }
