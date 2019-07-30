package io.deftrade
package rdb

// import cats._
// import cats.implicits._

// URLParser.parse("jdbc:postgresql://localhost:5233/my_database?user=postgres&password=somepassword")
// val connection: Connection = new PostgreSQLConnection(configuration)

import enumeratum.{ Enum, EnumEntry }

trait CatsOrder[EE <: EnumEntry] { _: Enum[EE] =>
  import cats.Order
  import cats.instances.int._
  implicit lazy val catsOrder: Order[EE] = Order by indexOf
}

trait StdOrdering[EE <: EnumEntry] { _: Enum[EE] =>
  import scala.math.Ordering
  implicit lazy val stdOrdering: Ordering[EE] = Ordering by indexOf
}

sealed trait Bounds {
  final def lower = toString charAt 0
  final def upper = toString charAt 1
}

object Bounds {

  case object `[)` extends Bounds
  case object `[]` extends Bounds
  case object `(]` extends Bounds
  case object `()` extends Bounds

  implicit val defaultBounds: Bounds = `[)`
}

/**
  * Typeclass for traits which have distinguished values to represent infinities.
  */
trait Boundless[A] {
  def noLower: A
  def noUpper: A
}

object Boundless {

  /**
    * since SQL uses `NULL` to signal ±∞ (I know, right?),
    * we need pairs of `Option[A]` to construct a `PgRange` in the general case
    */
  implicit def optionBoundless[A]: Boundless[Option[A]] = new Boundless[Option[A]] {
    override def noLower = None
    override def noUpper = None
  }

  /**
    * If we know the type `A` is Boundless (that is, has distinguished values for -∞ and ∞),
    * we don't need to wrap `A` in `Option`.
    */
  implicit val doubleBoundless: Boundless[Double] = new Boundless[Double] {
    override def noLower = Double.NegativeInfinity
    override def noUpper = Double.PositiveInfinity
  }
  import java.time.OffsetDateTime
  implicit val odtBoundless: Boundless[OffsetDateTime] = new Boundless[OffsetDateTime] {
    override def noLower = OffsetDateTime.MIN
    override def noUpper = OffsetDateTime.MAX
  }
  import java.time.LocalDateTime
  implicit val ldtBoundless: Boundless[LocalDateTime] = new Boundless[LocalDateTime] {
    override def noLower = LocalDateTime.MIN
    override def noUpper = LocalDateTime.MAX
  }

  def apply[A: Boundless]: Boundless[A] = implicitly
}

// nb since I'm not sure what postgres does when lower > upper, don't check yet
class PgRange[A] private (lower: A, upper: A, bound: Bounds) {
  final override def toString = s"${bound.lower}${lower}, ${upper}${bound.upper}"
}

object PgRange {

  /**
    * The spec says we can only make `PgRange[A]`s out of `Boundless` types.
    */
  def apply[A: Boundless](lower: A, upper: A)(implicit bounds: Bounds): PgRange[A] =
    apply(upper, lower, bounds)

  /**
    * Accepting an explicit bounds spec is always polite.
    */
  def apply[A: Boundless](lower: A, upper: A, bounds: Bounds): PgRange[A] =
    new PgRange[A](upper, lower, bounds)

  /**
    * Enrange all the Pairs fit to be enRanged.
    */
  implicit class RangePair[A: Boundless](val aa: (A, A))(implicit bounds: Bounds) {
    def toRange = PgRange[A](aa._1, aa._2)
  }

  object ImplicitConversions {
    import scala.language.implicitConversions
    implicit def convertToRange[A: Boundless](aa: (A, A))(implicit bound: Bounds) =
      PgRange(aa._1, aa._2, bound)
  }
}
