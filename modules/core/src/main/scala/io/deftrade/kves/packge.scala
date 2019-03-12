package io.deftrade

import cats.{ Eq, Order, Show }
import cats.implicits._

import eu.timepit.refined
import refined.api.{ Min, Refined, Validate }

import spire.math.Integral
import spire.implicits._

import scala.language.higherKinds

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
  type OpaqueKey[K, P] = Refined[K, P]

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, P] (unquallified for P) */
  implicit def orderOpaqueKey[K: Order, P]: Order[OpaqueKey[K, P]] =
    Order by (_.value)

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, P] (unquallified for P) */
  implicit def showOpaqueKey[K: Show: Order, P]: Show[OpaqueKey[K, P]] =
    Show show (key => s"k: ${key.value.show}")

  import enumeratum._
  import io.chrisdavenport.cormorant._
  import io.chrisdavenport.cormorant.implicits._

  /** Integrates Enumeratum into Cormorant (CSV). Use these methods to create implicits per Enum. */
  final def enumGet[EE <: EnumEntry](e: Enum[EE]): Get[EE] = Get tryOrMessage (
    field => scala.util.Try { e withName field.x },
    field => s"Failed to decode Enum: $e: Received Field $field"
  )
  final def enumPut[EE <: EnumEntry]: Put[EE] = stringPut contramap (_.toString)

}

package kves {

  object OpaqueKey {

    def apply[K: Order, P: Eq](k: K): OpaqueKey[K, P] = Refined unsafeApply k

    val fieldName = "key"

  }

  /**
    * Make a fresh *globally unique* key, suitable to be persisted.
    * TODO: consider threading F[_] thru Fresh.
    */
  final case class Fresh[I](init: I, next: I => I)

  /** fresh `Fresh` */
  object Fresh {

    def apply[KY: Fresh] = implicitly[Fresh[KY]]

    def zeroBasedIncr[K: Integral, P: Eq]: Fresh[OpaqueKey[K, P]] = {

      val K = Integral[K]
      import K._

      Fresh(
        OpaqueKey(zero),
        key => OpaqueKey(key.value + one)
      )
    }
  }

  private[kves] abstract class WithKeyBase[K: Order, V] {
    final type Key   = OpaqueKey[K, V]
    final type Value = V
    final type Row   = (Key, Value)

    implicit lazy val keyValidate: Validate[K, V] = Validate alwaysPassed (())

    import shapeless._
    import shapeless.labelled.FieldType
    import shapeless.syntax.singleton._

    import io.chrisdavenport.cormorant._
    // import io.chrisdavenport.cormorant.generic.semiauto._
    // import io.chrisdavenport.cormorant.parser._
    // import io.chrisdavenport.cormorant.implicits._
    // import io.chrisdavenport.cormorant.refined._

    val key = "key".witness
    type KeyFieldType = FieldType[key.T, Key]

    // implicit def deriveLabelledWrite[HV <: HList](
    //     implicit
    //     genV: LabelledGeneric.Aux[Value, HV],
    //     hlw: Lazy[LabelledWrite[KeyFieldType :: HV]]
    // ): LabelledWrite[Row] =
    //   new LabelledWrite[Row] {
    //     type H = KeyFieldType :: HV
    //     val writeH: LabelledWrite[H] = hlw.value
    //     def headers: CSV.Headers     = writeH.headers
    //     def write(r: Row): CSV.Row   = writeH.write(genV to r)
    //   }

    implicit def deriveLabelledReadRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlw: Lazy[LabelledRead[KeyFieldType :: HV]]
    ): LabelledRead[Row] =
      new LabelledRead[Row] {
        type H = KeyFieldType :: HV
        val readH: LabelledRead[H] = hlw.value
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Row] =
          readH.read(row, headers) map { h =>
            (h.head, genV from h.tail)
          }
      }

  }

  /** Module-level companion base class with default Key type */
  abstract class WithKey[K: Order, P: Eq] extends WithKeyBase[K, P] {
    object Key extends OpaqueKeyC[K, P]
  }

  /** Same but with implicit Eq[P] typeclass instance */
  abstract class WithKeyAndEq[K: Order, P] extends WithKeyBase[K, P] {
    object Key extends OpaqueKeyC[K, P]
    implicit lazy val eqP: Eq[P] = Eq.fromUniversalEquals[P]
  }

  /** Key type companion base class. */
  private[kves] abstract class OpaqueKeyC[K: Order, P: Eq] {
    def apply(k: K) = OpaqueKey[K, P](k)

    /** Where the key type is integral, we will reserve the min value. */
    def reserved(implicit K: Min[K]) = OpaqueKey[K, P](K.min)
  }

}
