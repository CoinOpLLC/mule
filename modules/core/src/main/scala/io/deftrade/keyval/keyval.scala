package io.deftrade
package keyval

// import cats.implicits._
import cats.{ Eq, Order }

import eu.timepit.refined
import refined.api.{ Min, Refined }

import spire.math.Integral
import spire.implicits._

import shapeless.{ ::, HList, LabelledGeneric, Lazy }
import shapeless.labelled._
// import shapeless.syntax.singleton._

import _root_.io.chrisdavenport.cormorant._

/** */
object OpaqueKey {

  /** */
  private[keyval] def apply[K: Order, V](k: K): OpaqueKey[K, V] = Refined unsafeApply k

  /** */
  def unsafe[K: Order, V](k: K): OpaqueKey[K, V] = apply(k)
}

/** Key type companion base class. */
abstract class KeyCompanion[K] {

  /** */
  implicit def order: Order[K]
}

/** Key type companion mez class for [[OpaqueKey]] types. */
abstract class OpaqueKeyCompanion[K: Order, P] extends KeyCompanion[OpaqueKey[K, P]] {

  /** */
  implicit def order = Order[OpaqueKey[K, P]]

  /** */
  def apply(k: K) = OpaqueKey[K, P](k)

  /** Where the key type is integral, we will reserve the min value. */
  def reserved(implicit K: Min[K]) = OpaqueKey[K, P](K.min)
}

/**
  * Defines how to create a fresh '''globally unique''' key which
  * is suitable to be persisted.
  */
final case class Fresh[K](init: K, next: K => K)

/** */
object Fresh {

  def apply[K: Fresh] = implicitly[Fresh[K]]

  /**
    * Equivalent to `autoincrement` or `serial` from SQL.
    *
    * TODO: PRNG version.
    */
  def zeroBasedIncr[K: Integral, P]: Fresh[OpaqueKey[K, P]] = {

    val K = Integral[K]
    import K._

    Fresh(
      OpaqueKey(zero),
      key => OpaqueKey(key.value + one)
    )
  }
}

/**  @inheritanceDiagram */
trait WithValue[V] {

  /**
    * The type of the underlying record being indexed.
    */
  final type Value = V

  /** */
  implicit lazy val eqValue: Eq[Value] = Eq.fromUniversalEquals[Value]

  /** */
  // implicit lazy val showValue: Show[V] = cats.derived.semi.show

  /** An permanent identifier (eg auto-increment in a db col)*/
  final type Id = OpaqueKey[Long, Value]

  /**
    * Standard auto-increment behavior on Long permanent id numbers.
    * Resisting the temptation to parameterize this for now.
    */
  object Id {
    implicit lazy val freshId: Fresh[Id] = Fresh.zeroBasedIncr
  }

  /**
    * Think spreadsheet or relational table,
    * keeping in mind that [[Value]]s are can be, and often are, compound.
    */
  type Row

  /** */
  object Row {
    type T = Row
  }

  /** */
  final type PermRow = (Id, Row)

  /** */
  object PermRow {
    final type T = PermRow
  }

  /** Will be assigned either Id or Key. */
  type Index

  /** The full type of the [[Id]] column. */
  final type IdField = FieldType[id.T, Id]

}

/**
  * Companion object base class.
  */
abstract class WithId[V] extends WithValue[V] {

  /** */
  final type Row = Value

  /** */
  final type Index = Id

  /** */
  implicit final def deriveLabelledWriteRow[HV <: HList](
      implicit
      genV: LabelledGeneric.Aux[Value, HV],
      hlw: Lazy[LabelledWrite[IdField :: HV]]
  ): LabelledWrite[PermRow] =
    new LabelledWrite[PermRow] {

      val writeHKV: LabelledWrite[IdField :: HV] = hlw.value

      def headers: CSV.Headers       = writeHKV.headers
      def write(r: PermRow): CSV.Row = writeHKV write field[id.T](r._1) :: (genV to r._2)
    }

  /** */
  implicit final def deriveLabelledReadRow[HV <: HList](
      implicit
      genV: LabelledGeneric.Aux[Value, HV],
      hlr: Lazy[LabelledRead[IdField :: HV]]
  ): LabelledRead[PermRow] =
    new LabelledRead[PermRow] {
      val readHKV: LabelledRead[IdField :: HV] = hlr.value
      def read(row: CSV.Row, headers: CSV.Headers): Either[Error.DecodeFailure, PermRow] =
        readHKV.read(row, headers) map { h =>
          (h.head, genV from h.tail)
        }
    }
}

/**
  * Companion object base class.
  */
abstract class WithKey[V] extends WithValue[V] {

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

  /** */
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

  /** */
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

/** When you absolutely, positively wanna (well behaved) case class as a `Key`. */
abstract class WithAdtKey[K: Order, V] extends WithKey[V] {

  /** */
  sealed abstract case class Key private (val k: K)

  /** */
  object Key extends KeyCompanion[Key] {

    /** */
    def apply(k: K): Key = new Key(k) {}

    /** */
    override implicit def order: Order[Key] = Order by (_.k)
  }
}

/** */
abstract class WithRefinedKey[K: Order, P, V] extends WithKey[V] {

  /**
    * Phantom type used to tag the key, which has type K as its underlying representation.
    * This can either be a trivial tag which encodes the independance of a key from the record
    * that it indexes, or, some other kind of constraint (i.e. a `Predicate`).
    *
    * The assumption is that some kind of tagging (eg `Refine` or `@@`) is
    * combining `K` and `P` to create the `Key` type.
    */
  final type Tag = P

  /**
    * Keys type is auto generated and presents a uniform convention.
    */
  final type Key = Refined[K, Tag]

  /** */
  object Key extends OpaqueKeyCompanion[K, P]
}

/**
  * Use case: `Predicate` type is non trival.
  *
  * For example, the `Key` might be of {{{ type Label = String Refined (NonEmpty And Trimmed) }}}.
  */
abstract class WithPredicateKey[K: Order, P, V] extends WithRefinedKey[K, P, V]

/**
  * '''By convention''', this companion defines a key as a [[eu.timepit.refined.api.Refined]]
  * type, parameterized with the value type we are indexing.
  *
  * This phantom type for the `Refined` Key type is [[[Value]]]).
  */
abstract class WithOpaqueKey[K: Order, V] extends WithRefinedKey[K, V, V]

/** No constraint on validation. */
// implicit final lazy val keyValidate: Validate[K, Value] = Validate alwaysPassed (())