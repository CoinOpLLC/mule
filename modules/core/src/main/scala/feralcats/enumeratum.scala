/** FIXME: this is only here because of that one time maven couldn't find enum-cats */
package enumeratum

import cats.instances.string._
import cats.{ Eq, Hash, Order, Show }

trait CatsEnum[A <: EnumEntry] { this: Enum[A] =>

  /**
    * `Eq` instance for the enum entries - treats all enum values as distinct.
    */
  implicit val eqInstance: Eq[A] = Cats.eqForEnum[A]

  /**
    * `Show` instance for the enum entries - returns the (transformed) entry name.
    */
  implicit val showInstance: Show[A] = Cats.showForEnum[A]

  /**
    * `Hash` instance for the enum entries - based on entry name.
    */
  implicit val hashInstance: Hash[A] = Cats.hashForEnum[A]

  /** FIXME this is just a hack to use `SortedSet`s etc
    * it is almost certainly wrong to do this, but why?
    */
  implicit val orderInstance: Order[A] = Cats.orderForEnum[A]

}

object Cats {

  /**
    * Builds an `Eq` instance which differentiates all enum values as it's based on universal equals.
    */
  def eqForEnum[A <: EnumEntry]: Eq[A] = Eq.fromUniversalEquals[A]

  /**
    * Builds a `Show` instance returning the entry name (respecting possible mixins).
    */
  def showForEnum[A <: EnumEntry]: Show[A] = Show.show[A](_.entryName)

  /**
    * `Hash` instance based on the entry name.
    */
  def hashForEnum[A <: EnumEntry]: Hash[A] = Hash.by[A, String](_.entryName)

  /** FIXME this is just a hack to use `SortedSet`s etc */
  def orderForEnum[A <: EnumEntry]: Order[A] = Order by (_.entryName)

}

package values {

  import cats.{ Eq, Hash, Order, Show }
  import cats.syntax.contravariant._

  trait CatsValueEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]] {
    this: ValueEnum[ValueType, EntryType] =>

    /**
      * `Eq` instance for the enum entries - treats all enum values as distinct.
      */
    implicit val eqInstance: Eq[EntryType] = Cats.eqForEnum[EntryType]

    /**
      * Builds a `Show` instance based on `toString`.
      */
    implicit val showInstance: Show[EntryType] = Cats.showForEnum[EntryType]

  }

  trait CatsCustomOrderValueEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]] {
    this: ValueEnum[ValueType, EntryType] =>

    /**
      * Order for the enum's value type - used to derive [[orderInstance]].
      */
    implicit val valueTypeOrder: Order[ValueType]

    /**
      * Builds a `Order` instance from the given `Order` (see [[valueTypeOrder]] on the value type.
      */
    implicit val orderInstance: Order[EntryType] =
      Cats.valueOrderForEnum[EntryType, ValueType](valueTypeOrder)

  }

  abstract class CatsOrderValueEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]](
      implicit override val valueTypeOrder: Order[ValueType]
  ) extends CatsCustomOrderValueEnum[ValueType, EntryType] {
    this: ValueEnum[ValueType, EntryType] =>
    // nothing needed here
  }

  object Cats {

    /**
      * Builds an `Eq` instance which differentiates all enum values as it's based on universal equals.
      */
    def eqForEnum[A <: ValueEnumEntry[_]]: Eq[A] = Eq.fromUniversalEquals[A]

    /**
      * Builds an `Eq` instance which acts accordingly to the given `Eq` on the value type. Allows to implement different
      * behaviour than [[eqForEnum]], for example grouping several enum values in special contexts.
      */
    def valueEqForEnum[A <: ValueEnumEntry[V], V: Eq]: Eq[A] = Eq.by[A, V](_.value)

    /**
      * Builds a `Show` instance based on `toString`.
      */
    def showForEnum[A <: ValueEnumEntry[_]]: Show[A] = Show.fromToString[A]

    /**
      * Builds a `Show` instance from the given `Show` on the value type.
      */
    def valueShowForEnum[A <: ValueEnumEntry[V], V: Show]: Show[A] = Show[V].contramap[A](_.value)

    /**
      * Builds a `Order` instance from the given `Order` on the value type.
      */
    def valueOrderForEnum[A <: ValueEnumEntry[V], V: Order]: Order[A] = Order.by[A, V](_.value)

    /**
      * Builds a `Hash` instance from the given `Hash` on the value type.
      */
    def valueOrderForEnum[A <: ValueEnumEntry[V], V: Hash]: Hash[A] = Hash.by[A, V](_.value)

  }
}
