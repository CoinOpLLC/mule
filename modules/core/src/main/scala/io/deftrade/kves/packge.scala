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
  type OpaqueKey[K0, V] = Refined[K0, V] with kves.OpaqueKeyLike { type K = K0 }

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

  trait OpaqueKeyLike extends Any {
    type K
    def value: K
    final def key: K = value
  }

  object OpaqueKey {

    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply[K: Order, V](k: K) = (Refined unsafeApply k).asInstanceOf[OpaqueKey[K, V]]

  }

  /** Key type companion base class. */
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
    * fresh `Fresh`
    * FIXME this is horrible
    */
  object Fresh {

    def apply[K: Fresh] = implicitly[Fresh[K]]

    def zeroBasedIncr[K: Integral, V]: Fresh[OpaqueKey[K, V]] = {

      val K = Integral[K]
      import K._

      Fresh(
        OpaqueKey[K, V](zero),
        key => OpaqueKey(key.value + one)
      )
    }
  }

  abstract class WithKeyCompanion[K: Order, V]()(implicit HV: LabelledGeneric[V]) {

    /**
      * n.b. `V` is used as a phantom type here
      */
    final type Value = V
    final val lgv = HV
    final type LGV = lgv.Repr

    /**
      * **By convention**, we tag keys with the value type of the record table we are indexing.
      * (This is the phantom type [[[Value]]]).
      * This is a policy decision, and, as such, is subject to revision.
      */
    final type Key = OpaqueKey[K, Value]

    /** Think spreadsheet or relational table. Keep in mind that [[Value]]s are compound. */
    final type Row = (Key, Value)

    /** A basic in-memory table structure. TODO: revisit this. */
    final type Table = Map[Key, Value]

    /** The full type of the [[Key]] column. */
    final type KeyFieldType = FieldType[key.T, Key]

    /** No constraint on validation. */
    implicit final lazy val keyValidate: Validate[K, Value] = Validate alwaysPassed (())

    // import io.deftrade.money.Money.{ moneyGet, moneyPut }

    implicit final def deriveLabelledWriteRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlw: Lazy[LabelledWrite[FieldType[key.T, Key] :: HV]]
    ): LabelledWrite[Row] =
      new LabelledWrite[Row] {
        type HKV = KeyFieldType :: HV
        val writeH: LabelledWrite[HKV] = hlw.value
        def headers: CSV.Headers       = writeH.headers
        def write(r: Row): CSV.Row     = writeH write field[key.T](r._1) :: (genV to r._2)
      }

    implicit final def deriveLabelledReadRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlr: Lazy[LabelledRead[KeyFieldType :: HV]]
    ): LabelledRead[Row] =
      new LabelledRead[Row] {
        type HKV = KeyFieldType :: HV
        val readH: LabelledRead[HKV] = hlr.value
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Row] =
          readH.read(row, headers) map { h =>
            (h.head, genV from h.tail)
          }
      }

  }

  /** Same but with implicit Eq[V] typeclass instance */
  abstract class WithKey[K: Order, V: LabelledGeneric] extends WithKeyCompanion[K, V] {
    object Key extends OpaqueKeyCompanion[K, V]
    implicit lazy val eqP: Eq[V] = Eq.fromUniversalEquals[V]

    /**
      */
    implicit def freshKey(implicit K: Integral[K]): Fresh[Key] = Fresh.zeroBasedIncr[K, V]
  }

}
