package io.deftrade

import java.time._, chrono.{ Chronology, IsoChronology }
import java.time.{ temporal => jtt }
import jtt._

package object time extends time.Api {

  implicit class SweetClock(val base: Clock) extends AnyVal {
    def offset(d: Duration) = Clock offset (base, d)
    def tick(d: Duration)   = Clock tick (base, d)
  }

  implicit class J8TimeLong(val n: Long) extends AnyVal {

    def hour: Duration    = time.hours(n)
    def hours: Duration   = time.hours(n)
    def minute: Duration  = time.minutes(n)
    def minutes: Duration = time.minutes(n)
    def second: Duration  = time.seconds(n)
    def seconds: Duration = time.seconds(n)
    def milli: Duration   = time.millis(n)
    def millis: Duration  = time.millis(n)
    def nano: Duration    = time.nanos(n)
    def nanos: Duration   = time.nanos(n)
  }

  implicit class J8TimeInt(val n: Int) extends AnyVal {

    def day: Period    = time.days(n)
    def days: Period   = time.days(n)
    def week: Period   = time.weeks(n)
    def weeks: Period  = time.weeks(n)
    def month: Period  = time.months(n)
    def months: Period = time.months(n)
    def year: Period   = time.years(n)
    def years: Period  = time.years(n)
  }

  implicit class SweetLocalDateTime(val ldt: LocalDateTime) extends AnyVal {

    def year: Int              = ldt.getYear
    def dayOfMonth: Int        = ldt.getDayOfMonth
    def month: Month           = ldt.getMonth
    def monthValue: Int        = ldt.getMonthValue
    def dayOfWeek: DayOfWeek   = ldt.getDayOfWeek
    def dayOfYear: Int         = ldt.getDayOfYear
    def hour: Int              = ldt.getHour
    def minute: Int            = ldt.getMinute
    def second: Int            = ldt.getSecond
    def nano: Int              = ldt.getNano
    def chronology: Chronology = ldt.getChronology

    def +(amount: Period): LocalDateTime   = ldt plus amount
    def -(amount: Period): LocalDateTime   = ldt minus amount
    def +(amount: Duration): LocalDateTime = ldt plus amount
    def -(amount: Duration): LocalDateTime = ldt minus amount

    def -(end: LocalDateTime): Duration = Duration between (ldt, end)

  }

  implicit class SweetLocalDate(val ld: LocalDate) extends AnyVal {

    def year: Int              = ld.getYear
    def dayOfMonth: Int        = ld.getDayOfMonth
    def month: Month           = ld.getMonth
    def monthValue: Int        = ld.getMonthValue
    def dayOfWeek: DayOfWeek   = ld.getDayOfWeek
    def dayOfYear: Int         = ld.getDayOfYear
    def chronology: Chronology = ld.getChronology

    def yearMonthDay = (ld.year, ld.monthValue, ld.dayOfMonth)
    def yearMonth    = (ld.year, ld.monthValue)
    def monthDay     = (ld.monthValue, ld.dayOfMonth)

    def +(amount: Period): LocalDate = ld plus amount
    def -(amount: Period): LocalDate = ld minus amount

    def -(end: LocalDate): Period = ld until end
  }

  implicit class SweetLocalTime(val lt: LocalTime) extends AnyVal {

    def hour: Int   = lt.getHour
    def minute: Int = lt.getMinute
    def second: Int = lt.getSecond
    def nano: Int   = lt.getNano

    def +(duration: Duration): LocalTime = lt plus duration
    def -(duration: Duration): LocalTime = lt minus duration
  }

  implicit class SweetZonedDateTime(val zdt: ZonedDateTime) extends AnyVal {
    def year: Int            = zdt.getYear
    def dayOfMonth: Int      = zdt.getDayOfMonth
    def month: Month         = zdt.getMonth
    def monthValue: Int      = zdt.getMonthValue
    def dayOfWeek: DayOfWeek = zdt.getDayOfWeek
    def dayOfYear: Int       = zdt.getDayOfYear
    def hour: Int            = zdt.getHour
    def minute: Int          = zdt.getMinute
    def second: Int          = zdt.getSecond
    def nano: Int            = zdt.getNano
    def zone: ZoneId         = zdt.getZone
    def offset: ZoneOffset   = zdt.getOffset

    def localDate: LocalDate         = zdt.toLocalDate
    def localTime: LocalTime         = zdt.toLocalTime
    def localDateTime: LocalDateTime = zdt.toLocalDateTime

    def +(p: Period): ZonedDateTime   = zdt plus p
    def -(p: Period): ZonedDateTime   = zdt minus p
    def +(d: Duration): ZonedDateTime = zdt plus d
    def -(d: Duration): ZonedDateTime = zdt minus d

    def -(other: ZonedDateTime): Duration = Duration between (zdt, other)

    def chronology: Chronology = zdt.getChronology
  }

  implicit class SweetDuration(val d: Duration) extends AnyVal {

    import scala.concurrent.duration.{ FiniteDuration, NANOSECONDS, SECONDS }

    def nanos: Long   = d.toNanos
    def millis: Long  = d.toMillis
    def seconds: Long = d.getSeconds
    def minutes: Long = d.toMinutes
    def hours: Long   = d.toHours
    def days: Long    = d.toDays

    def -(other: Duration): Duration = d minus other
    def +(other: Duration): Duration = d plus other
    def /(divisor: Long): Duration   = d dividedBy divisor
    def *(scalar: Long): Duration    = d multipliedBy scalar

    def toFiniteDuration: FiniteDuration =
      d.getNano match {
        case 0 =>
          FiniteDuration(d.getSeconds, SECONDS)
        case nanos =>
          FiniteDuration(d.getSeconds, SECONDS) + FiniteDuration(nanos.toLong, NANOSECONDS)
      }

  }
  implicit class SweetPeriod(val p: Period) extends AnyVal {

    def days: Int                 = p.getDays
    def months: Int               = p.getMonths
    def years: Int                = p.getYears
    def chronology: IsoChronology = p.getChronology

    def -(other: TemporalAmount): Period = p minus other
    def +(other: TemporalAmount): Period = p plus other
    def *(scalar: Int): Period           = p multipliedBy scalar

  }
  implicit class SweetYear(val y: Year) extends AnyVal {
    def year: Int = y get ChronoField.YEAR

    def -(amount: Period) = y minus amount
    def +(amount: Period) = y plus amount
  }

  implicit class SweetYearMonth(val ym: YearMonth) extends AnyVal {

    def year: Int       = ym.getYear
    def month: Month    = ym.getMonth
    def monthValue: Int = ym.getMonthValue

    def -(amount: Period) = ym minus amount
    def +(amount: Period) = ym plus amount

  }

  implicit class WithToAdjusted(val value: Temporal) extends AnyVal {
    def adjusted(ta: TemporalAdjuster) = value `with` ta
  }

}
