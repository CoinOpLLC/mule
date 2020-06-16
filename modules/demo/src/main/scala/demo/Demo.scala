/**
* Demonstration vehicle for the `deftrade` suite of libraries.
*/

package demo

import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import fs2.{io, text, Stream, Pipe}

import java.nio.file.Paths
import java.{ time => jt }, jt.{ temporal => jtt }, jtt.TemporalAdjusters
import jt.{ DayOfWeek, Month }, Month.{ JANUARY => January } // if you must

import scala.concurrent.{ duration => scd }

/** Simple usage examples for the [[io.deftrade.time]] package. */
object TimeExample {


  import io.deftrade.time._

  // java.time.Duration
  val d1 = duration(seconds = 20, nanos = 1)
  val d2 = hours(10)
  val d3 = minutes(10)
  val d4 = seconds(10)
  val d5 = millis(10)
  val d6 = nanos(10)

  // java.time.Perod
  val p1 = period(years = 1, months = 2, days = 3)
  val p2 = days(10)
  val p3 = weeks(10)
  val p4 = months(10)
  val p5 = years(10)

  val oldRule     = years(1) + days(1)
  val quarterYear = months(12 / 4)

  // java.time.LocalDate
  val ld1 = localDate(2015, 1, 1)
  val ld2 = localDate(2015, January, 1)
  val ld3 = localDate("2015-01-01")

  // java.time.LocalTime
  val lt1 = localTime(20, 30, 0)
  val lt2 = localTime(hour = 20, minute = 30, second = 12, nano = 5)

  // java.time.LocalDateTime
  val ldt1 = localDateTime(localDate(2015, 1, 1), localTime(20, 30, 5))
  val ldt3 = localDateTime(localDate(2015, January, 1), localTime(20, 30, 5))
  val ldt4 = ld1
  val ldt5 = localDateTime("2015-01-01T20:30:05")

  // java.time.ZonedDateTime
  val zdt1 = zonedDateTime(localDate(2015, 1, 1), localTime(20, 30, 10), zoneId("GMT"))

  // java.time.YearMonth
  val ym1 = yearMonth(2015, 1)
  val ym2 = yearMonth(2015, Month.JANUARY)

  // If (if!) we ever have to mix / convert between scala.concurrent.duration, this is how.

  implicit val scdDurationEq = cats.Eq.fromUniversalEquals[scd.Duration]
  implicit val durationEq    = cats.Eq.fromUniversalEquals[Duration]
  implicit val periodEq      = cats.Eq.fromUniversalEquals[Period]

  val t2fd: scd.Duration = 2.seconds.toFiniteDuration

  val t1 = 1.nano
  val t2 = 2.millis
  val t3 = 2.seconds
  val t4 = 3.minutes
  val t5 = 1.hour

  val t6 = 5.days
  val t7 = 1.week
  val t8 = 7.months
  val t9 = 2.years

  val x = 19.hours + 47.minutes

  // third tuesday
  val TemporalAdjuster(firstTuesday) = TemporalAdjusters firstInMonth DayOfWeek.WEDNESDAY
  val thirdTuesday                   = TemporalAdjuster { firstTuesday andThen (_ + 2.weeks) }
}


/** */
object Demo extends IOApp {

  final val BUFFER_SIZE = 4096 // * 4096 // nostalgia

  type Report = Pipe[IO, Transaction, BookSet]
  val report: Report = ???

  val reporter: Stream[IO, Unit] = (Stream resource Blocker[IO]) flatMap { blocker =>
    io.file.readAll[IO](Paths get "transactions.csv", blocker, BUFFER_SIZE)
      .through(text.utf8Decode)
      .through(text.lines)
      .through(/* csv read => Transaction */)
      .through(report)
      .through(/* BookSet.show */)
      .through(text.utf8Encode)
      .through(io.file.writeAll(Paths get "reports/booksets.txt", blocker))
  }

  def run(args: List[String]): IO[ExitCode] =
    reporter.compile.drain as ExitCode.Success
}
