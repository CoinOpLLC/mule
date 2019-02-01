package io.deftrade
package test

import cats.implicits._
import cats.syntax.eq._

import org.scalatest._, prop._

class TimeFlatSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import io.deftrade.time._
  import implicits._

  "time" should "move forward" in {
    val today = localDateTime
    val later = 33.hours + 33.minutes + 33.seconds + 33.millis + 33.nanos
    (today + later) should be > today
  }

  it should "throw an exception if a bad date is parsed" in {
    a[DateTimeParseException] should be thrownBy {
      "144 Doritos" |> instant
    }
  }

  "The integers" should "generally behave as advertized" in {
    forAll { (n: Int) =>
      whenever(n > 1) { n / 2 should be > 0 }
    }
  }
}

class TimeSpec extends FlatSpec with Matchers {

  import io.deftrade.time._
  import java.time.Month
  import java.time.temporal.ChronoUnit, ChronoUnit.{ HOURS => Hours, MINUTES => Minutes }

  "j8 date/time factory methods" should "work" in {

    // java.time.Duration
    assert(minutes(20) - minutes(10) == minutes(10))
    assert(seconds(10) + seconds(10) == seconds(20))
    assert(duration(Minutes)(20) / 5 == duration(ChronoUnit.MINUTES)(4))
    assert(duration(Hours)(10) * 5 == duration(ChronoUnit.HOURS)(50))

    // java.time.Period
    assert(days(1) + days(1) == days(2))
    assert(months(2) - months(1) == months(1))

    // java.time.LocalTime
    assert(localTime(20, 30, 0) + minutes(5) == localTime(20, 35, 0))
    assert(localTime(20, 30, 0) - minutes(5) == localTime(20, 25, 0))

    // java.time.LocalDate
    assert(localDate(2015, Month.JANUARY, 1) + months(2) == localDate(2015, Month.MARCH, 1))
    assert(localDate(2015, Month.MARCH, 1) - months(2) == localDate(2015, Month.JANUARY, 1))
  }

  "domain Order stuff" should "work" in {
    // import cats.{ Eq, Monoid }
    // import model._

    // val o1 = Order.legacy(555.550001, 78345)
    // val o2 = Order.legacy(168.020660, 186283)

    // val o12 = o1 |+| o2
    //
    // // assert(o12 == Order.legacy(BigDecimal(723.570661), 264628))
    //
    // val m1   = Map(1337 -> o1)
    // val m1_a = Map(1337 -> o2)
    // // val m2   = Map(4958 -> Order.legacy(666.880033, 123456))
    // val mmm = m1 |+| m1_a |+| m2
    // assert(
    //   mmm === Map(
    //     4958 -> Order.legacy(666.880033, 123456),
    //     1337 -> Order.legacy(723.570661, 264628)
    //   )
    // )

  }
}

class TimePropSpec extends PropSpec with GeneratorDrivenPropertyChecks {
// with TableDrivenPropertyChecks {
  // import org.scalacheck.Gen._
  // import java.time._
  // property("same as it ever was") {
  //   forAll { (ldt: LocalDateTime) =>
  //     whenever(true) {
  //       assert(ldt === ldt)
  //     }
  //   }
  // }
}

class CamelCasePropSpec extends PropSpec with GeneratorDrivenPropertyChecks {

  // Our Gold standard (for testing): yet another take on an old fav:
  // https://github.com/lift/framework/search?utf8=%E2%9C%93&q=%22def+snakify%22

  // splits off strings of capital letters leaving one...
  val rx1 = """([A-Z]+)([A-Z][a-z])""".r

  // splits transition from lower -> upper case
  val rx2 = """([a-z\d])([A-Z])""".r

  def delimit(rx: scala.util.matching.Regex)(s: String): String = rx replaceAllIn (s, "$1•$2")

  def goldCamelTo(sep: String)(name: String): String =
    (name |> delimit(rx1) |> delimit(rx2)) split "•" mkString sep

  property("CamelCase: verify the gold standard") {
    forAll { s: String =>
      whenever(true) {
        assert(goldCamelTo("")(s) === s)
      }
    }
  }
  property("CamelCase: test impl against gold standard") {
    forAll { s: String =>
      whenever(true) {
        assert(CamelTo("")(s) === s)
        assert(CamelTo("•")(s) === goldCamelTo("•")(s))
      }
    }
  }
}
