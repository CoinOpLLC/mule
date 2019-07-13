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
  * Defines types and implicit methods for a given domain value type (typically a case class).
  *
  * - ids:
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
package object keyval {

  /** Just an alias, bssically.  */
  type OpaqueKey[K, V] = Refined[K, V]

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, V] (unquallified for V) */
  implicit def orderOpaqueKey[K: Order, V]: Order[OpaqueKey[K, V]] =
    Order by (_.value)

  /** n.b. `Order` is inferred for _all_ `OpaqueKey`[K: Order, V] (unquallified for V) */
  implicit def showOpaqueKey[K: Show: Integral, V]: Show[OpaqueKey[K, V]] =
    Show show (key => s"key=${key.value.show}")

  /**
    * The [[Id]] column is by convention assigned a key column label: `'id: Symbol`.
    *
    * The `id` member is a `shapeless.Aux[Symbol @@ String(id)]` instance,
    * useful for type member `T`, which is the (singleton) type of the id column label.
    */
  private[keyval] final val id = Symbol("id").witness

  /**
    * [[Key]] column type literal witness - same purpose as [[id]].
    */
  private[keyval] final val key = Symbol("key").witness

}

package keyval {

  object OpaqueKey {

    def apply[K: Order, V](k: K): OpaqueKey[K, V] = (Refined unsafeApply k)

  }

  /** Key type companion mez class. */
  abstract class OpaqueKeyCompanion[K: Order, P] {

    def apply(k: K) = OpaqueKey[K, P](k)

    /** Where the key type is integral, we will reserve the min value. */
    def reserved(implicit K: Min[K]) = OpaqueKey[K, P](K.min)
  }

  /**
    * Intance which defines how to create a fresh *globally unique* key, suitable to be persisted.
    *
    * TODO: consider threading F[_] thru Fresh.
    */
  final case class Fresh[K](init: K, next: K => K)

  object Fresh {

    def apply[K: Fresh] = implicitly[Fresh[K]]

    /** FIXME use lifted functions from K to Refined[K, Value] */
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
  private[keyval] abstract class WithKeyBase[V] {

    /**
      * So `Foo`s are indexed with `Foo.Key`s
      */
    type Key

    /**
      * The type of the underlying record being indexed.
      */
    final type Value = V

    /** FIXME: use kittens? extend to show and hash? */
    implicit lazy val eqP: Eq[Value] = Eq.fromUniversalEquals[Value]

    /** An permanent identifier (e.g. auto-increment in a db col)*/
    final type Id = OpaqueKey[Long, Value]

    /**
      * Standard auto-increment behavior on Long permanent id numbers.
      * Resisting the tempation to parameterize this for now.
      */
    object Id {
      implicit lazy val freshId: Fresh[Id] = Fresh.zeroBasedIncr
    }

    /** Think spreadsheet or relational table. Keep in mind that [[Value]]s are compound. */
    final type Row = (Key, Value)
    object Row {
      final type T = Row
    }

    final type PermaRow = (Id, Row)
    object PermaRow {
      final type T = PermaRow
    }

    final type PitRow = (time.Instant, PermaRow)
    object PitRow {
      final type T = PitRow
    }

    /**
      * Basic in-memory table structures.
      * (Be kind, naming is hard.)
      */
    final type Table = Map[Key, Value]

    /** The full type of the [[Key]] column. */
    final type KeyField = FieldType[key.T, Key]

    // import io.deftrade.money.Money.{ moneyGet, moneyPut }

    implicit final def deriveLabelledGenericRow[HR <: HList, HV <: HList](
        implicit
        lgR: LabelledGeneric.Aux[Row, HR],
        lgV: LabelledGeneric.Aux[Value, HV],
    ): LabelledGeneric[KeyField :: HV] = ???
    // FIXME really what I want to do is LabelledGeneric[Row], but flattened

    implicit final def deriveLabelledWriteRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlw: Lazy[LabelledWrite[KeyField :: HV]]
    ): LabelledWrite[Row] =
      new LabelledWrite[Row] {
        val writeHKV: LabelledWrite[KeyField :: HV] = hlw.value
        def headers: CSV.Headers                    = writeHKV.headers
        def write(r: Row): CSV.Row                  = writeHKV write field[key.T](r._1) :: (genV to r._2)
      }

    implicit final def deriveLabelledReadRow[HV <: HList](
        implicit
        genV: LabelledGeneric.Aux[Value, HV],
        hlr: Lazy[LabelledRead[KeyField :: HV]]
    ): LabelledRead[Row] =
      new LabelledRead[Row] {
        val readHKV: LabelledRead[KeyField :: HV] = hlr.value
        def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, Row] =
          readHKV.read(row, headers) map { h =>
            (h.head, genV from h.tail)
          }
      }
  }

  private[keyval] abstract class WithRefinedKeyBase[K: Order, P, V] extends WithKeyBase[V] {

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
      * Keys type is auto generated and uniform
      */
    final type Key = OpaqueKey[K, Tag]

  }

  abstract class WithAdtKey[K: Order, V] extends WithKeyBase[V] {
    sealed abstract class Key(val k: K)
    object Key {
      def apply(k: K): Key              = new Key(k) {}
      implicit def orderKey: Order[Key] = Order by (_.k)
    }
  }

  abstract class WithRefinedKey[K: Order, P, V] extends WithRefinedKeyBase[K, P, V] {

    /** */
    object Key extends OpaqueKeyCompanion[K, P]
  }

  /**
    * **By convention**, this companion tag keys with the value type of the
    * record table we are indexing.
    *
    * This phantom type for the `Refined` Key type is [[[Value]]]).
    */
  abstract class WithOpaqueKey[K: Order, V] extends WithRefinedKeyBase[K, V, V] {

    /** Uses Value type as (phantom) predicate type */
    object Key extends OpaqueKeyCompanion[K, V]

    /** No constraint on validation. */
    implicit final lazy val keyValidate: Validate[K, Value] = Validate alwaysPassed (())

  }

}
