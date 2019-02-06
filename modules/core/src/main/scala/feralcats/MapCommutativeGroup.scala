package feralcats {

  import cats.kernel.CommutativeGroup
  import cats.kernel.instances.MapMonoid

  class MapCommutativeGroup[K, V: CommutativeGroup] extends MapMonoid[K, V] with CommutativeGroup[Map[K, V]] {
    def inverse(a: Map[K, V]): Map[K, V] =
      a.foldLeft(Map.empty[K, V]) { case (my, (k, x)) => my updated (k, CommutativeGroup inverse x) }
  }

  object instances {
    implicit def catsFeralStdCommutativeGroup[K, V: CommutativeGroup]: CommutativeGroup[Map[K, V]] =
      new MapCommutativeGroup[K, V]
  }

}

/** FIXME: this is only here because of that one time maven couldn't find enum-cats */
package enumeratum {

  import cats.instances.string._
  import cats.{ Eq, Hash, Show }

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

  }
}
