package io.deftrade
package test

import time._
import money._
import kves._

import enumeratum._

import cats._
import cats.implicits._
import cats.syntax.eq._ // ups the implicit priority (?!)

import eu.timepit.refined
import refined.{ refineMV, refineV }
import refined.api.Refined
import refined.collection.NonEmpty
import refined.numeric.Positive

import io.chrisdavenport.cormorant
import cormorant._
import cormorant.generic.auto._
// import cormorant.parser._
// import cormorant.refined._
import cormorant.implicits._

import org.scalatest.{ FlatSpec, PropSpec }

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._

/** Nuts exist in the test package. Make of that what you will. */
sealed trait Nut extends EnumEntry with Product with Serializable

/** All the nuts I could think of. */
object Nut extends Enum[Nut] with CsvEnum[Nut] {

  case object Peanut     extends Nut
  case object Hazelnut   extends Nut
  case object Almond     extends Nut
  case object Cashew     extends Nut
  case object Walnut     extends Nut
  case object Pecan      extends Nut
  case object Pistaschio extends Nut
  case object Brazil     extends Nut

  lazy val values: IndexedSeq[Nut] = findValues

}

object minviablethingie {
  import shapeless._
  final case class Foo(d: Double, s: String, b: Boolean)
  object Foo extends WithKey[Long, Foo] {
    def mk(s: String): Foo =
      Foo(
        s = s,
        d = s.length / 17.0,
        b = s.isEmpty || ((s.head.toInt % 2) == 0)
      )
  }

}

object xaction {

  type SsnPattern = refined.W.`"[0-9]{3}-[0-9]{2}-[0-9]{4}"`.T
  type IsSsn      = refined.string.MatchesRegex[SsnPattern]
  type Ssn        = String Refined IsSsn

  type NonEmptyString = String Refined NonEmpty

  sealed abstract case class Entity private (ssn: Ssn, name: NonEmptyString)

  object Entity extends WithKey[Long, Entity] {
    def apply(ssn: Ssn, name: NonEmptyString): Entity = new Entity(ssn, name) {}
  }

  sealed abstract case class Xaction[N: Financial, CCY: Currency](
      asOf: Instant,
      debitFrom: Entity.Key,
      creditTo: Entity.Key,
      amount: Money[N, CCY],
      memo: String
  )

  object Xaction extends WithKey[Long, Xaction[BigDecimal, Currency.USD]]
}

class KvesSpec extends FlatSpec {

  "`Foo`s" should "be created randomly" in {

    import shapeless._

    import csvUnderTest._

    val xs: List[Foo]       = List.fill(3)(Foo.unsafeRandom)
    val ks: List[Foo.Key]   = xs map (_ => Fresh[Foo.Key].init)
    val rows: List[Foo.Row] = ks zip xs

    // val lwFoo    = LabelledWrite[Foo]
    // val lrFoo    = LabelledRead[Foo]
    // val lrFooRow = LabelledRead[Foo.Row]
    // val lwFooRow = LabelledWrite[Foo.Row]

    // val csv = rows.writeComplete print Printer.default

    // // From String to Type
    // val decoded: Either[Error, List[Foo.Row]] = {
    //   parseComplete(csv)
    //     .leftWiden[Error]
    //     .flatMap(_.readLabelled[Foo.Row].sequence)
    // }

    // val Some(roundTripRows: List[Foo.Row]) = decoded.toOption
    // assert(rows === roundTripRows)
  }
}
class KvesPropSpec extends PropSpec with ScalaCheckDrivenPropertyChecks {
// with TableDrivenPropertyChecks {
  import demoUnderTest._
  property("some property about Foo") {
    forAll { foo: Foo =>
      // Ensure foo has the required property
    }
  }
}

object Jt8Gen {
  import time._
  def durationGen: Gen[Duration]                           = ???
  def finiteDurationGen(range: Duration): Gen[Duration]    = ???
  def localDateTimeInPeriod(p: Period): Gen[LocalDateTime] = ???
}

object csvUnderTest {

  import kves._
  import time._
  import money._
  import Currency.USD

  import java.util.UUID

  case class Bar(i: Int, s: String)
  object Bar extends WithKey[Long, Bar] {
    implicit lazy val freshKey: Fresh[Key] = Fresh.zeroBasedIncr
  }

  /** Serialize this. */
  // case class Foo[C: Currency](
  case class Foo(
      uuid: UUID,
      ts: Instant,
      s: String,
      i: Int,
      l: Long,
      d: Double,
      bd: BigDecimal,
      date: LocalDate,
      time: LocalTime,
      nut: Nut,
      bar: Bar.Key,
      x: Double Refined Positive,
      usd: Currency[USD],
      amount: Money[Double, USD],
  )

  object Foo extends WithKey[Long, Foo] {

    def unsafeRandom: Foo = {
      val uuid   = UUID.randomUUID
      val ts     = instant
      val s      = uuid.toString
      val i      = s.map(_.toInt).sum
      val l      = (i * 555).toLong
      val d      = scala.math sqrt i.toDouble
      val bd     = d |> BigDecimal.apply
      val rn     = ts atZone zoneId
      val date   = rn.localDate
      val time   = rn.localTime
      val nut    = Nut.Almond
      val bar    = Bar.Key.reserved
      val x      = refineMV[Positive](3.14)
      val usd    = Currency.USD
      val amount = USD(1.0)
      Foo(
        uuid = uuid,
        ts = ts,
        s = s,
        i = i,
        l = l,
        d = d,
        bd = bd,
        date = date,
        time = time,
        nut = nut,
        bar = bar,
        x = x,
        usd = usd,
        amount = amount,
      )
    }
  }
}

object demoUnderTest {

  import time._

  case class Foo(i: Int, s: String, b: Boolean, nut: Nut)

  case class Bar(foo: Foo, s: String, z: Instant)

  sealed trait Base
  case class BaseIntString(i: Int, s: String)         extends Base
  case class BaseDoubleBoolean(d: Double, b: Boolean) extends Base

  implicit def arbitraryBar: Arbitrary[Bar] = ???

  // Instant, LocalDateTime, LocalDate, LocalTime, Period

  implicitly[Arbitrary[Foo]]
  implicitly[Arbitrary[Bar]]
  implicitly[Arbitrary[Base]]

}
