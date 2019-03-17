package io.deftrade

import cats.{ Eq, Order, Show }
import cats.implicits._

import eu.timepit.refined
import refined.api.{ Min, Refined, Validate }

import spire.math.Integral
import spire.implicits._

import shapeless._
import shapeless.labelled._
import shapeless.syntax.singleton._

import io.chrisdavenport.cormorant._
// import io.chrisdavenport.cormorant.generic.semiauto._
// import io.chrisdavenport.cormorant.parser._
// import io.chrisdavenport.cormorant.implicits._
// import io.chrisdavenport.cormorant.refined._

// import scala.language.higherKinds

/**
  * kvse: Key Value Entity Scheme*:
  *
  * - keys: opaque identifiers with `Order`, `Eq`, `Hash` and `Show` (uses `refined`).
  * - values: value objects (case classes) with `Eq`, `Hash` and `Show`
  * - entities: ("aggregate roots") `Map`s of Key -> Value entries: repos, logs...
  *
  * This shall be the law: A `type Foo` may not depend on any type used as a key for `Foo`s.
  *
  * This package will enforce the law by
  * - aliasing `Refined` as an opaque key for a collection of a given type of values
  * - using the type of the `Value` to be the phantom types
  * - providing the `Key` types and instances as companion base classes.
  *
  * *It's a scheme because calling it a "schema" is too grand.
  */
package object kves {

  /** Just an alias.  */
  type OpaqueKey[K, V] = Refined[K, V]

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, V] (unquallified for V) */
  implicit def orderOpaqueKey[K: Order, V]: Order[OpaqueKey[K, V]] =
    Order by (_.value)

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, V] (unquallified for V) */
  implicit def showOpaqueKey[K: Show: Order, V]: Show[OpaqueKey[K, V]] =
    Show show (key => s"k: ${key.value.show}")

  final val key = 'key.witness
  final type keyT = key.T

}

package kves {

  object OpaqueKey {

    def apply[K: Order, V: Eq](k: K): OpaqueKey[K, V] = Refined unsafeApply k

  }

  /**
    * Make a fresh *globally unique* key, suitable to be persisted.
    * TODO: consider threading F[_] thru Fresh.
    */
  final case class Fresh[I](init: I, next: I => I)

  /** fresh `Fresh` */
  object Fresh {

    def apply[KY: Fresh] = implicitly[Fresh[KY]]

    def zeroBasedIncr[K: Integral, V: Eq]: Fresh[OpaqueKey[K, V]] = {

      val K = Integral[K]
      import K._

      Fresh(
        OpaqueKey(zero),
        key => OpaqueKey(key.value + one)
      )
    }
  }

  private[kves] abstract class WithKeyBase[K: Order, V] {

    /**
      * n.b. `V` is used as a phantom type here
      */
    type Key                = OpaqueKey[K, V]
    final type KeyFieldType = FieldType[keyT, Key]
    implicit lazy val keyValidate: Validate[K, V] = Validate alwaysPassed (())

    final type Value = V
    // val ValueLG: LabelledGeneric[Value]

    /** `Repr` is more valuable `<: HList` */
    type Repr <: HList

    final type Row = (Key, Value)
    // val RowLG: LabelledGeneric[Row]

    type RowRepr <: HList

    final def deriveLabelledWriteRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlw: Lazy[LabelledWrite[FieldType[keyT, Key] :: HV]]
    ): LabelledWrite[Row] =
      new LabelledWrite[Row] {
        type H = KeyFieldType :: HV
        val writeH: LabelledWrite[H] = hlw.value
        def headers: CSV.Headers     = writeH.headers
        def write(r: Row): CSV.Row   = writeH write field[key.T](r._1) :: (genV to r._2)
      }

    final def deriveLabelledReadRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlr: Lazy[LabelledRead[KeyFieldType :: HV]]
    ): LabelledRead[Row] =
      new LabelledRead[Row] {
        type H = KeyFieldType :: HV
        val readH: LabelledRead[H] = hlr.value
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Row] =
          readH.read(row, headers) map { h =>
            (h.head, genV from h.tail)
          }
      }
  }

  /** Module-level companion base class with default Key type */
  abstract class WithKey[K: Order, V: Eq] extends WithKeyBase[K, V] {
    object Key extends OpaqueKeyC[K, V]
  }

  /** Same but with implicit Eq[V] typeclass instance */
  abstract class WithKeyAndEq[K: Order, V] extends WithKeyBase[K, V] {
    object Key extends OpaqueKeyC[K, V]
    implicit lazy val eqP: Eq[V] = Eq.fromUniversalEquals[V]
  }

  /** Key type companion base class. */
  private[kves] abstract class OpaqueKeyC[K: Order, V: Eq] {

    def apply(k: K) = OpaqueKey[K, V](k)

    /** Where the key type is integral, we will reserve the min value. */
    def reserved(implicit K: Min[K]) = OpaqueKey[K, V](K.min)
  }
}
