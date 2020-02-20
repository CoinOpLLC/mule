package io.deftrade
package test

import io.deftrade._

import implicits._, time._
import java.time.Month
import java.time.temporal.ChronoUnit, ChronoUnit.{ HOURS => Hours, MINUTES => Minutes }

import cats.implicits._
import cats.syntax.eq._ // FIXME: don't understand why this works, or is necessary

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TimeFlatSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

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

class TimeSpec extends AnyFlatSpec with Matchers {

  "j8 date/time factory methods" should "work" in {

    // java.time.Duration
    assert(minutes(20) - minutes(10) === minutes(10))
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
}

class TimePropSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks {}
