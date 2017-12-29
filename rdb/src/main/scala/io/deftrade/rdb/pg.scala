package io.deftrade
package rdb

import cats._
import cats.implicits._

// URLParser.parse("jdbc:postgresql://localhost:5233/my_database?user=postgres&password=somepassword")
// val connection: Connection = new PostgreSQLConnection(configuration)

sealed trait Bound {
  final def lower = toString charAt 0
  final def upper = toString charAt 1
}
case object `[)` extends Bound
case object `[]` extends Bound
case object `(]` extends Bound
case object `()` extends Bound

// n.b. since I'm not sure what postgres does when lower > upper, don't check yet
class Range[A] private (val lower: A, upper: A, bound: Bound) {
  final override def toString = s"${bound.lower}${lower}, ${upper}${bound.upper}"
}

/**
  * Typeclass for traits which have distinguished values to represent infinities.
  */
trait Boundless[A] {
  def noLower: A
  def noUpper: A
}

object Boundless {
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
}

object Range {

  implicit def toOptionRange[A](pair: (Option[A], Option[A]))(implicit bound: Bound) =
    new Range(pair._1, pair._2, bound)

  implicit def toRange[A: Boundless](pair: (A, A))(implicit bound: Bound) =
    new Range(pair._1, pair._2, bound)

  object ImplicitBounds {
    implicit val defaultBound: Bound = `[)`
  }
}
