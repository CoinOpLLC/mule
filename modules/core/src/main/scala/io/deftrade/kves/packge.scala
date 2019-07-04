package io.deftrade

import cats.{ Eq, Order, Show }
import cats.implicits._

import eu.timepit.refined
import refined.api.{ Min, Refined, Validate }

import spire.math.Integral
import spire.implicits._

import shapeless.{ ::, HList, LabelledGeneric, Lazy }
import shapeless.labelled._
import shapeless.syntax.singleton._

import io.chrisdavenport.cormorant._
// import io.chrisdavenport.cormorant.generic.semiauto._
// import io.chrisdavenport.cormorant.parser._
// import io.chrisdavenport.cormorant.implicits._
// import io.chrisdavenport.cormorant.refined._

/**
  * kvse: Key Value Entity Scheme*:
  *
  * - keys: opaque identifiers with `Order`, `Hash` and `Show` typeclass instances
  * - values: value objects (case classes) with `Eq`, `Hash` and `Show`
  * - entities: `Map`s of Key -> Value entries: repos, logs...
  *
  * This shall be the law: A `type Foo` may not depend on any type used as a key for `Foo`s.
  *
  * This package will enforce the law by
  * - aliasing `Refined` as an opaque key for a collection of a given type of values
  * - assinging the `Value` type to be the phantom type parameter for the Refined type constructor
  * - providing the `Key` types and instances as companion base classes.
  *
  * Further, the package supports the instantiaton of the scheme by
  * - providing a `Row` type (`Key` -> `Value`)
  * - providing a `Table` type (Map[Key, Value])
  * - providing implicit derivations for CSV file readers and writers of `Row`s and `Tables`s.
  *
  * *It's a scheme because calling it a "schema" is too grand.
  */
package object kves {

  /** Just an alias, bssically.  */
  type OpaqueKey[K, V] = Refined[K, V]

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, V] (unquallified for V) */
  implicit def orderOpaqueKey[K: Order, V]: Order[OpaqueKey[K, V]] =
    Order by (_.value)

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, V] (unquallified for V) */
  implicit def showOpaqueKey[K: Show: Integral, V]: Show[OpaqueKey[K, V]] =
    Show show (key => s"key=${key.value.show}")

  /**
    * Key column type literal witness.
    *
    * The [[Key]] column is by convention assigned a key column label: `'key: Symbol`.
    *
    * The `key` member is a `shapeless.Aux[Symbol @@ String(key)]` instance,
    * useful for type member `T`, which is the (singleton) type of the key column label.
    */
  final val key = 'key.witness

}

package kves {

  object OpaqueKey {

    def apply[K: Order, V](k: K): OpaqueKey[K, V] = (Refined unsafeApply k)

  }

  /** Key type companion base class. */
  abstract class KeyCompanion[K: Order, P, V] {}

  /** Key type companion mez class. */
  abstract class OpaqueKeyCompanion[K: Order, V] {

    def apply(k: K) = OpaqueKey[K, V](k)

    /** Where the key type is integral, we will reserve the min value. */
    def reserved(implicit K: Min[K]) = OpaqueKey[K, V](K.min)
  }

  /**
    * Make a fresh *globally unique* key, suitable to be persisted.
    * TODO: consider threading F[_] thru Fresh.
    */
  final case class Fresh[K](init: K, next: K => K)

  /**
    * FIXME use lifted functions from K to Refined[K, Value]
    */
  object Fresh {

    def apply[K: Fresh] = implicitly[Fresh[K]]

    def zeroBasedIncr[K: Integral, P]: Fresh[OpaqueKey[K, P]] = {

      val K = Integral[K]
      import K._

      Fresh(
        OpaqueKey(zero),
        key => OpaqueKey(key.value + one)
      )
    }
  }

  /**
    * Companion object base class for "value types".
    * (Value types in the DDD sense, not the scala sense.)
    */
  abstract class WithRefinedKey[K: Order, P, V] {

    /**
      * The type of the underlying record being indexed.
      */
    final type Value = V

    /**
      * Phantom type used to tag the key, which has type K as its underlying representation.
      * This can either be a trivial tag which encodes the independance of a key from the record
      * that it indexes, or, some other kind of constraint.
      *
      * The assumption is that some kind of tagging (e.g. `Refine` or `@@`) is
      * combining `K` and `P` to create the `Key` type.
      */
    final type Tag = P

    /**
      * So `Foo`s are indexed with `Foo.Key`s
      */
    final type Key = OpaqueKey[K, Tag]

    /** Think spreadsheet or relational table. Keep in mind that [[Value]]s are compound. */
    final type Row = (Key, Value)

    /** A basic in-memory table structure. TODO: revisit this. */
    final type Table = Map[Key, Value]

    /** The full type of the [[Key]] column. */
    final type KeyFieldType = FieldType[key.T, Key]

    // import io.deftrade.money.Money.{ moneyGet, moneyPut }

    implicit final def deriveLabelledWriteRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlw: Lazy[LabelledWrite[FieldType[key.T, Key] :: HV]]
    ): LabelledWrite[Row] =
      new LabelledWrite[Row] {
        val writeHKV: LabelledWrite[KeyFieldType :: HV] = hlw.value
        def headers: CSV.Headers                        = writeHKV.headers
        def write(r: Row): CSV.Row                      = writeHKV write field[key.T](r._1) :: (genV to r._2)
      }

    implicit final def deriveLabelledReadRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlr: Lazy[LabelledRead[KeyFieldType :: HV]]
    ): LabelledRead[Row] =
      new LabelledRead[Row] {
        val readHKV: LabelledRead[KeyFieldType :: HV] = hlr.value
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Row] =
          readHKV.read(row, headers) map { h =>
            (h.head, genV from h.tail)
          }
      }
  }

  /**
    * **By convention**, this companion tag keys with the value type of the
    * record table we are indexing.
    *
    * This phantom type for the `Refined` Key type is [[[Value]]]).
    */
  abstract class WithKey[K: Order, V] extends WithRefinedKey[K, V, V] {
    object Key extends OpaqueKeyCompanion[K, V]

    /** No constraint on validation. */
    implicit final lazy val keyValidate: Validate[K, Value] = Validate alwaysPassed (())

    /** FIXME: use kittens? extend to show and hash? break into separate trait or add to base? */
    implicit lazy val eqP: Eq[V] = Eq.fromUniversalEquals[V]

  }

}
