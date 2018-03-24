/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import scala.language.implicitConversions

package io {
  package object deftrade extends Api /*with MyTime */ {

    /**
      * Civilized function invocation.
      */
    implicit def pipeToFunction1[A](a: A) = PipeToFunction1(a)

    def assertOrElse(msg: String): Boolean => Unit = assert(_, msg)

    def camelToSnake(name: String): String  = camelTo(name)("_")
    def camelToHyphen(name: String): String = camelTo(name)("-")
    def camelToDot(name: String): String    = camelTo(name)(".")
    def camelToWord(name: String): String   = camelTo(name)(" ")

  }

  package deftrade {

    final case class PipeToFunction1[A](val a: A) extends AnyVal {
      def |>[B](f: A => B): B = f(a)
      def p2f1[B](f: A => B): B = a |> f
    }

    final case class PhatString(val s: String) extends AnyVal {
      def noSpaces: String = s filterNot (" \n\r\t" contains _)
    }

    object time { self =>

      import java.time._, chrono.{ Chronology, IsoChronology }, format.DateTimeFormatter
      import java.time.{ temporal => jtt }
      import jtt._, ChronoUnit._

      // Clock

      type Clock = java.time.Clock

      def clockDefaultZone                      = Clock.systemDefaultZone
      def clockDefaultUTC                       = Clock.systemUTC
      def clockFrozen(i: Instant, zone: ZoneId) = Clock fixed (i, zone)

      def clock(zone: ZoneId) = Clock system zone

      implicit class SweetClock(val base: Clock) extends AnyVal {
        def offset(d: Duration) = Clock offset (base, d)
        def tick(d: Duration)   = Clock tick (base, d)
      }

      // Duration

      type Duration = java.time.Duration

      def duration(unit: ChronoUnit)(n: Long) = Duration of (n, unit)
      def duration(seconds: Long, nanos: Int) = Duration ofSeconds seconds withNanos nanos

      def hours(n: Long)   = duration(HOURS)(n)
      def minutes(n: Long) = duration(MINUTES)(n)
      def seconds(n: Long) = duration(SECONDS)(n)
      def millis(n: Long)  = duration(MILLIS)(n)
      def nanos(n: Long)   = duration(NANOS)(n)

      // Period

      type Period = java.time.Period

      def period(years: Int, months: Int, days: Int) = Period of (years, months, days)

      def years(y: Int)  = period(y, 0, 0)
      def months(m: Int) = period(0, m, 0)
      def days(d: Int)   = period(0, 0, d)
      def weeks(w: Int)  = days((w: Int) * 7)

      object DurationOf {
        def unapply(d: Duration) = Option((d.getSeconds, d.getNano))
      }

      object PeriodOf {
        def unapply(p: Period) = Option((p.getYears, p.getMonths, p.getDays))
      }

      implicit class WithToAdjusted(val value: Temporal) extends AnyVal {
        def adjusted(ta: TemporalAdjuster) = value `with` ta
      }

      type TemporalAdjuster = java.time.temporal.TemporalAdjuster

      object TemporalAdjuster {
        type HM[A] = A => A
        type HMLD  = HM[LocalDate]

        def apply(adjust: HMLD): TemporalAdjuster = new TemporalAdjuster {
          // justification: creating an instance of a java lib class; this is what the doc says: throw.
          @SuppressWarnings(Array("org.wartremover.warts.Throw"))
          override def adjustInto(t: Temporal): Temporal = t match {
            case ld: LocalDate => ld |> adjust
            case _ =>
              throw new DateTimeException(
                s"only args of type LocalDate are accepted; found ${t.getClass}"
              )
          }
        }

        @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
        def unapply(ta: jtt.TemporalAdjuster): Option[HM[LocalDate]] =
          Some(
            localDate =>
              // justification for this downcast:
              // per the `TemporalAdjuster` javadocs, and I quote:
              // > The returned object must have the same observable type as the input object
              // i.e., skating on thin ice; assuming this is obeyed.
              (ta adjustInto localDate).asInstanceOf[LocalDate]
          )
      }

      type TemporalQuery[R] = java.time.temporal.TemporalQuery[R]

      // This name is free in Value Land, because it's occupied by a lone j8 interface in Type Land
      object TemporalQuery {
        import jtt.{ TemporalQueries => TQs }
        import cats._, implicits._ // FIXME choke down on that bat son

        type TQ[+R] = LocalDate => R
        // consider R := LocalDate => LocalDate... !
        // in other words: A TQ[LocalDate => LocalDate] Query produces an Adjuster from a LocalDate
        type TA = TQ[TemporalAdjuster.HMLD]

        def asTemporalQuery[R](tq: TQ[R]): TemporalQuery[Option[R]] = new TemporalQuery[Option[R]] {
          override def queryFrom(ta: TemporalAccessor): Option[R] = ta match {
            case ld: LocalDate => (ld |> tq).some
            case _             => none
          }
        }

        def chronology: TQ[Option[Chronology]]  = lift(_ query TQs.chronology)
        def precision: TQ[Option[TemporalUnit]] = lift(_ query TQs.precision)

        def localDate: TQ[Option[LocalDate]] = lift(_ query TQs.localDate)
        def localTime: TQ[Option[LocalTime]] = lift(_ query TQs.localTime)

        def offset: TQ[Option[ZoneOffset]] = lift(_ query TQs.offset)
        def zoneId: TQ[Option[ZoneId]]     = lift(_ query TQs.zoneId)
        def zone: TQ[Either[Option[ZoneOffset], ZoneId]] = _ query TQs.zone match {
          case zo: ZoneOffset     => zo.some.asLeft // (sic) ZoneOffset <: ZoneId – too clever...
          case zid if zid != null => zid.asRight // actual ZoneId
          case _                  => none.asLeft // not applicable - zone makes no sense
        }

        private def lift[R](tq: TQ[R]): TQ[Option[R]] = ta => Option(ta |> tq)
      }

      implicit class J8TimeLong(val n: Long) extends AnyVal {

        def hour: Duration    = self.hours(n)
        def hours: Duration   = self.hours(n)
        def minute: Duration  = self.minutes(n)
        def minutes: Duration = self.minutes(n)
        def second: Duration  = self.seconds(n)
        def seconds: Duration = self.seconds(n)
        def milli: Duration   = self.millis(n)
        def millis: Duration  = self.millis(n)
        def nano: Duration    = self.nanos(n)
        def nanos: Duration   = self.nanos(n)

      }

      implicit class J8TimeInt(val n: Int) extends AnyVal {

        def day: Period    = self.days(n)
        def days: Period   = self.days(n)
        def week: Period   = self.weeks(n)
        def weeks: Period  = self.weeks(n)
        def month: Period  = self.months(n)
        def months: Period = self.months(n)
        def year: Period   = self.years(n)
        def years: Period  = self.years(n)
      }
      // LocalDateTime

      type LocalDateTime = java.time.LocalDateTime

      def localDateTime               = LocalDateTime.now
      def localDateTime(zone: ZoneId) = LocalDateTime now zone
      def localDateTime(clock: Clock) = LocalDateTime now clock

      def localDateTime(ta: TemporalAccessor) = LocalDateTime from ta

      def localDateTime(iso8601: String) = LocalDateTime parse iso8601

      def localDateTime(s: String, fmt: DateTimeFormatter) = LocalDateTime parse (s, fmt)

      def localDateTime(ld: LocalDate, lt: LocalTime) = LocalDateTime of (ld, lt)

      object LocalDateTimeOf {
        def unapply(dt: LocalDateTime) = Option((dt.toLocalDate, dt.toLocalTime))
      }

      implicit class SweetLocalDateTime(val ldt: LocalDateTime) extends AnyVal with Ordered[LocalDateTime] {

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

        override def compare(that: LocalDateTime): Int = ldt compareTo that
      }

      // LocalDate

      type LocalDate = java.time.LocalDate

      def localDate               = LocalDate.now
      def localDate(clock: Clock) = LocalDate now clock
      def localDate(zone: ZoneId) = LocalDate now zone

      def localDate(ta: TemporalAccessor)              = LocalDate from ta
      def localDate(iso8601: String)                   = LocalDate parse iso8601
      def localDate(fmt: DateTimeFormatter)(s: String) = LocalDate parse (s, fmt)

      def localDate(year: Int, month: Month, day: Int) = LocalDate of (year, month, day)
      def localDate(year: Int, month: Int, day: Int)   = LocalDate of (year, month, day)

      object LocalDateOf {
        def unapply(ld: LocalDate) = Option((ld.year, ld.month, ld.dayOfMonth))
      }
      implicit class SweetLocalDate(val ld: LocalDate) extends AnyVal with Ordered[LocalDate] {

        def year: Int              = ld.getYear
        def dayOfMonth: Int        = ld.getDayOfMonth
        def month: Month           = ld.getMonth
        def monthValue: Int        = ld.getMonthValue
        def dayOfWeek: DayOfWeek   = ld.getDayOfWeek
        def dayOfYear: Int         = ld.getDayOfYear
        def chronology: Chronology = ld.getChronology

        def +(amount: Period): LocalDate = ld plus amount
        def -(amount: Period): LocalDate = ld minus amount

        def +(wpp: money.ProxiedPeriod): LocalDate = ld plus wpp.period
        def -(wpp: money.ProxiedPeriod): LocalDate = ld minus wpp.period

        def -(end: LocalDate): Period = ld until end

        override def compare(that: LocalDate): Int = ld compareTo that
      }

      // LocalTime

      type LocalTime = java.time.LocalTime

      def localTime               = LocalTime.now
      def localTime(clock: Clock) = LocalTime now clock
      def localTime(zone: ZoneId) = LocalTime now zone

      def localTime(hour: Int, minute: Int, second: Int): LocalTime =
        LocalTime of (hour, minute, second, 0)

      def localTime(hour: Int, minute: Int, second: Int, nano: Int): LocalTime =
        LocalTime of (hour, minute, second, nano)

      object LocalTimeOf {
        def unapply(lt: LocalTime): Option[(Int, Int, Int, Int)] =
          Some((lt.getHour, lt.getMinute, lt.getSecond, lt.getNano))

      }

      implicit class SweetLocalTime(val lt: LocalTime) extends AnyVal with Ordered[LocalTime] {

        def hour: Int   = lt.getHour
        def minute: Int = lt.getMinute
        def second: Int = lt.getSecond
        def nano: Int   = lt.getNano

        def +(duration: Duration): LocalTime = lt plus duration
        def -(duration: Duration): LocalTime = lt minus duration

        override def compare(that: LocalTime): Int = lt compareTo that
      }

      // ZonedDateTime

      type ZonedDateTime = java.time.ZonedDateTime

      def zonedDateTime(ld: LocalDate, lt: LocalTime, zone: ZoneId) =
        ZonedDateTime of (ld, lt, zone)

      implicit class SweetZonedDateTime(val zdt: ZonedDateTime) extends AnyVal with Ordered[ZonedDateTime] {

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

        def chronology: Chronology                      = zdt.getChronology
        override def compare(other: ZonedDateTime): Int = zdt compareTo other
      }

      type Instant = java.time.Instant

      def instant                   = Instant.now()
      def instant(clock: Clock)     = Instant now clock
      def instant(epochMilli: Long) = Instant ofEpochMilli epochMilli
      def instant(epochSecond: Long, nanoAdjustment: Long) =
        Instant ofEpochSecond (epochSecond, nanoAdjustment)
      def instant(text: CharSequence) = Instant parse text

      implicit class SweetInstant(val i: Instant) extends Ordered[Instant] {

        def nano: Int         = i.getNano
        def epochSecond: Long = i.getEpochSecond

        def +(amount: TemporalAmount): SweetInstant = i plus amount
        def -(amount: TemporalAmount): SweetInstant = i minus amount

        override def compare(that: Instant): Int = i compareTo that
      }

      implicit class SweetDuration(val d: Duration) extends AnyVal with Ordered[Duration] {

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

        override def compare(other: Duration): Int = d.compareTo(other)
      }

      implicit class SweetPeriod(val p: Period) extends AnyVal with Ordered[Period] {

        def days: Int                 = p.getDays
        def months: Int               = p.getMonths
        def years: Int                = p.getYears
        def chronology: IsoChronology = p.getChronology

        def -(other: TemporalAmount): Period = p minus other
        def +(other: TemporalAmount): Period = p plus other
        def *(scalar: Int): Period           = p multipliedBy scalar

        override def compare(that: Period): Int = (p minus that).getDays
      }

      type Year = java.time.Year

      def year               = Year.now
      def year(clock: Clock) = Year now clock
      def year(zone: ZoneId) = Year now zone

      def year(s: String)                         = Year parse s
      def year(s: String, fmt: DateTimeFormatter) = Year parse (s, fmt)

      def year(isoYear: Int)               = Year of isoYear
      def year(temporal: TemporalAccessor) = Year from temporal

      object YearOf {
        def unapply(year: Year): Option[Int] = Some(year get ChronoField.YEAR)
      }

      type YearMonth = java.time.YearMonth

      def yearMonth               = YearMonth.now
      def yearMonth(clock: Clock) = YearMonth now clock
      def yearMonth(zone: ZoneId) = YearMonth now zone

      def yearMonth(s: String)                         = YearMonth parse s
      def yearMonth(s: String, fmt: DateTimeFormatter) = YearMonth parse (s, fmt)

      def yearMonth(year: Int, month: Int)      = YearMonth of (year, month)
      def yearMonth(year: Int, month: Month)    = YearMonth of (year, month)
      def yearMonth(temporal: TemporalAccessor) = YearMonth from temporal

      object YearMonthOf {
        def unapply(ym: YearMonth): Option[(Int, Month)] =
          Some((ym.getYear, ym.getMonth))
      }

      implicit class SweetYear(val y: Year) extends AnyVal with Ordered[Year] {
        def year: Int = y get ChronoField.YEAR

        def -(amount: Period) = y minus amount
        def +(amount: Period) = y plus amount

        override def compare(other: Year): Int = y compareTo other

      }

      implicit class SweetYearMonth(val ym: YearMonth) extends AnyVal with Ordered[YearMonth] {

        def year: Int       = ym.getYear
        def month: Month    = ym.getMonth
        def monthValue: Int = ym.getMonthValue

        def -(amount: Period) = ym minus amount
        def +(amount: Period) = ym plus amount

        override def compare(other: YearMonth): Int = ym compareTo other
      }

      import collection.JavaConverters._

      type ZoneId = java.time.ZoneId

      def zoneId(s: String)                                = ZoneId of s
      def zoneId(s: String, aliasMap: Map[String, String]) = ZoneId of (s, aliasMap.asJava)
      def zoneId(prefix: String, offset: ZoneOffset)       = ZoneId ofOffset (prefix, offset)
      def zoneId(ta: TemporalAccessor)                     = ZoneId from ta

      final lazy val zoneIdMap: Map[String, String] = ZoneId.SHORT_IDS.asScala.toMap

      final val GmtZoneId = ZoneId of "GMT"
      final val UtcZoneId = ZoneId of "UTC"
      final val UtZoneId  = ZoneId of "UT"
      final val ZZoneId   = ZoneId of "Z"

      object ZoneIdOf {} // TODO: finish ZoneId including sugar

      // Exceptions

      type DateTimeException                = java.time.DateTimeException
      type DateTimeParseException           = java.time.format.DateTimeParseException
      type UnsupportedTemporalTypeException = java.time.temporal.UnsupportedTemporalTypeException
      type ZoneRulesException               = java.time.zone.ZoneRulesException

      object money {

        import io.deftrade.time.work.WorkYear

        import scala.collection.immutable.SortedSet
        import scala.concurrent.{ duration => scd }

        import cats.Order
        import cats.data.Reader
        import cats.implicits._

        import enumeratum._, values._

        // issue: can I just go ahead and make these ValueEnum[Duration]? I don't see *why*.

        sealed abstract class CashTenor(override val entryName: String) extends EnumEntry
        object CashTenor extends Enum[CashTenor] {

          case object Spot         extends Tenor("SP") // zero days later
          case object SpotNext     extends Tenor("SN") // one dat later
          case object Overnight    extends Tenor("ON") // one day later
          case object TomorrowNext extends Tenor("TN") // two days later? Sorta...
          // issue: might need a **pair** of dates as state for the algebra... but do we?
          // TomorrowNext increments both cursors, in effect... why, exactly?!
          // type DateCalculatorState = (LocalDate, LocalDate) // maybe?

          lazy val values = findValues

        }
        // from Objectkitlab - homolog in opengamma?
        sealed trait SpotLag extends EnumEntry
        object SpotLag extends Enum[SpotLag] {
          def values = findValues
          case object T_0 extends SpotLag
          case object T_1 extends SpotLag
          case object T_2 extends SpotLag
        }

        // TODO: implicitly enrich LocalDate such that it comprehends the addition of business days

        // maybe we should have `exactly` one ChronoUnit per Frequency

        trait ProxiedPeriod extends EnumEntry {
          import ProxiedPeriod._

          def pspec: String

          final val period = Period parse pspec // parse exception is effectily compile-time

          final lazy val field: Map[ChronoUnit, Int] = {
            val values = (rx findFirstMatchIn pspec).fold(??? /* sic */ )(identity).subgroups map {
              case s if s != null => s.toInt
              case _              => 0
            }
            ((ymwd zip values) filter (_ match { case (_, v) => v =!= 0 })).toMap
          }

          override def toString: String = 'T' +: (pspec drop 1)
        }
        object ProxiedPeriod {

          import jtt.ChronoUnit._

          private val ymwd = List(YEARS, MONTHS, WEEKS, DAYS)
          private val rx =
            s"\\A${(ymwd map (x => s"(?:(\\d+)${x.toString.head})?")).mkString}\\z".r

        }

        sealed abstract class Tenor(override val pspec: String) extends ProxiedPeriod

        object Tenor extends Enum[Tenor] {
          case object T_SPOT      extends Tenor("P0D")
          case object T_OVERNIGHT extends Tenor("P1D")
          case object T1D         extends Tenor("P1D")
          case object T2D         extends Tenor("P2D")
          case object T1W         extends Tenor("P1W")
          case object T1M         extends Tenor("P1M")
          case object T2M         extends Tenor("P2M")
          case object T3M         extends Tenor("P3M")
          case object T6M         extends Tenor("P6M")
          case object T9M         extends Tenor("P9M")
          case object T1Y         extends Tenor("P1Y")
          case object T2Y         extends Tenor("P2Y")
          case object T3Y         extends Tenor("P3Y")
          case object T4Y         extends Tenor("P4Y")
          case object T5Y         extends Tenor("P5Y")
          case object T7Y         extends Tenor("P7Y")
          case object T10Y        extends Tenor("P10Y")
          case object T20Y        extends Tenor("P20Y")
          case object T30Y        extends Tenor("P30Y")
          case object T50Y        extends Tenor("P50Y")
          case class TCustom(p: String) extends Tenor(p) {
            // FIXME: `require(period >= 0)`
          }
          lazy val values = findValues
        }

        sealed abstract class Frequency(override val pspec: String) extends ProxiedPeriod
        object Frequency extends Enum[Frequency] {
          private lazy val kalpa: String = (LocalDate.MAX - LocalDate.MIN).toString
          case object F_TERM extends Frequency(kalpa)  //  0
          case object F1D    extends Frequency("P1D")  //  364 / Y
          case object F1W    extends Frequency("P1W")  //  52
          case object F2W    extends Frequency("P2W")  //  26
          case object F4W    extends Frequency("P4W")  //  13
          case object F13W   extends Frequency("P13W") //  4
          case object F26W   extends Frequency("P26W") //  2
          case object F52W   extends Frequency("P52W") //  1
          case object F1M    extends Frequency("P1M")  //  12
          case object F2M    extends Frequency("P2M")  //  6
          case object F3M    extends Frequency("P3M")  //  4
          case object F4M    extends Frequency("P4M")  //  3
          case object F6M    extends Frequency("P6M")  //  2
          case object F12M   extends Frequency("P12M") //  1

          lazy val values = findValues

        }

        sealed trait DayCount extends EnumEntry {
          def days(start: LocalDate, end: LocalDate): Int
          def years(start: LocalDate, end: LocalDate)(implicit wy: WorkYear): Double
        }
        //ACT_[360|365|ACT]
        // CONV_30_360, CONV_360E_[ISDA|IMSA]
        object DayCount extends Enum[DayCount] {

          lazy val values = findValues

          case object ACT_ACT_ISDA extends DayCount {
            def days(start: LocalDate, end: LocalDate): Int = ???
            def years(start: LocalDate, end: LocalDate)(implicit wy: WorkYear): Double =
              if (end.year === start.year) {
                (end.dayOfYear - start.dayOfYear) / start.year.toDouble
              } else {
                val startYearFraction =
                  (start.lengthOfYear - start.dayOfYear + 1) / start.lengthOfYear
                val wholeYears      = (end.year - start.year - 1).toDouble
                val endYearFraction = (end.dayOfYear - 1) / end.lengthOfYear
                startYearFraction + wholeYears + endYearFraction
              }
          }
          case object CONV_30_360 extends DayCount {
            def days(start: LocalDate, end: LocalDate): Int                            = ???
            def years(start: LocalDate, end: LocalDate)(implicit wy: WorkYear): Double = ???
          }
        }

        sealed trait ImmPeriod extends EnumEntry {
          def indexes: SortedSet[Int] // BitSet // [0], [0,2], [1,3], [0,1,2,3]
        }

        object ImmPeriod {
          def apply(period: ImmPeriod)(year: Year): SortedSet[LocalDate] = ???
        }

        // dayOfWeek   getFirstDayOfWeek getMinimalDaysInFirstWeek
        // weekBasedYear   weekOfMonth  weekOfWeekBasedYear   weekOfYear

        // issue to get correct by construction: dealing with `DayOfWeek` int values across `Locale`s

        // 2 `WeekField`s of note: ISO and SundayStart (used in Asia)
        // otherwise use `Locale`

        // scala>   val dow = ISO.dayOfWeek
        // dow: java.time.temporal.TemporalField = DayOfWeek[WeekFields[MONDAY,4]]
        //
        // scala>   val sow = jtt.WeekFields.SUNDAY_START.dayOfWeek
        // sow: java.time.temporal.TemporalField = DayOfWeek[WeekFields[SUNDAY,1]]
        //
        // scala> today get dow
        // res108: Int = 6
        //
        // scala> today get sow
        // res109: Int = 7

        // Note: from the javadocs: would like to compile away the non-ISO blues. Can we?
        // The input temporal object may be in a calendar system other than ISO. Implementations may choose to document compatibility with other calendar systems, or reject non-ISO temporal objects by querying the chronology.
      }
      object work {

        import cats._, implicits._, data._
        import enumeratum._
        import scala.collection.SortedSet
        import jtt.WeekFields

        object WorkTimeConf {
          val daysOff = Seq(
            (12, 25),
            (1, 1),
            (7, 4)
          ) map { case (m, d) => (2018, m, d) }
        }

        val iso: WeekFields        = WeekFields.ISO
        val dow: jtt.TemporalField = iso.dayOfWeek

        // n.b.: all `jtt.Temporal`s are also `Comparable`
        implicit def temporalOrder[T <: Temporal with Comparable[T]]: Order[T]   = Order.fromComparable[T]
        implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]

        def yesterday = today - 1.day
        def today     = localDate
        def tomorrow  = today + 1.day

        // stick with ISO and be rigorous about others
        // TODO: make use of locale?
        type WorkWeek = SortedSet[DayOfWeek]
        type Holidays = SortedSet[LocalDate]

        /**
          * do some configure-with-code here...
          */
        final case class WorkYear(workWeek: WorkWeek, holidays: Holidays) {
          def workDay(ld: LocalDate): Boolean =
            (workWeek contains ld.dayOfWeek) && !(holidays contains ld)
        }

        object WorkYear {

          import WorkTimeConf._

          lazy val workWeek: WorkWeek =
            SortedSet.empty[DayOfWeek] ++ DayOfWeek.values - DayOfWeek.SATURDAY - DayOfWeek.SUNDAY

          private def mkLD: (Int, Int, Int) => LocalDate = localDate(_, _, _)

          lazy val holidays: Holidays = SortedSet.empty[LocalDate] ++ (daysOff map mkLD.tupled)

          implicit lazy val workYear: WorkYear = WorkYear(workWeek, holidays)

        }

        // need to define an ordering on DayOfWeek. Which would be great.
        // TODO: when does the week roll around?
        // TODO: what about that succ stuff from scalaz?

        // `TemporalQuery` is the way to do the "is this a working day or not" thing.
        // Just build an immutable list of `WorkWeek`s out as far as you can.

        // FIXME: this really belongs in core
        implicit val monthOrder: Order[Month] = Order.fromComparable[Month]

        type TqReader[R] = Reader[LocalDate, Option[R]]
        type TaReader[R] = Reader[LocalDate, R]

        def workDay(ld: LocalDate)(implicit wy: WorkYear): Boolean = wy workDay ld

        sealed abstract class SeekWorkDay private (delta: Period, sameMonth: Boolean) extends EnumEntry {

          final def temporalAdjuster: jtt.TemporalAdjuster = TemporalAdjuster(adjuster)
          final def adjuster: LocalDate => LocalDate       = adjust(delta, sameMonth)

          private def adjust(d: Period, sameMonth: Boolean): LocalDate => LocalDate = { ld: LocalDate =>
            val dd = ld + d
            ld match {
              case ld if workDay(dd) && sameMonth =>
                if (dd.getMonth === ld.getMonth) dd else ld |> adjust(delta.negated, false)
              case _ if workDay(dd) => dd
              case ld =>
                val succDay = if (d.isNegative) d - 1.day else d + 1.day
                ld |> adjust(succDay, sameMonth)
            }
          }
        }

        object SeekWorkDay extends Enum[SeekWorkDay] {

          case object Next          extends SeekWorkDay(delta = 1.day, sameMonth = false)
          case object NextSameMonth extends SeekWorkDay(delta = 1.day, sameMonth = true)
          case object Prev          extends SeekWorkDay(delta = -1.day, sameMonth = false)
          case object PrevSameMonth extends SeekWorkDay(delta = -1.day, sameMonth = true)

          lazy val values = findValues
        }
        import SeekWorkDay._

        @annotation.tailrec
        def moveByWorkDays(nwds: Int)(ld: LocalDate): LocalDate = nwds match {
          case n if n == 0 => ld |> Next.adjuster
          case n =>
            val (adjuster, delta) = if (n < 0) (Prev.adjuster, -1.day) else (Next.adjuster, 1.day)
            val movedByOne        = ld + delta |> adjuster
            n match {
              case 1 => movedByOne
              case n => moveByWorkDays(n - 1)(movedByOne)
            }
        }

        val adjustWorkDay: LocalDate => LocalDate = moveByWorkDays(0)
        val prevWorkDay: LocalDate => LocalDate   = moveByWorkDays(-1)

        // there is quite a bit to critique here - not very efficient.
        val sameMonthAdjustWorkDay: LocalDate => LocalDate =
          (for {
            twd <- Reader(identity: LocalDate => LocalDate)
            nwd <- Reader(adjustWorkDay)
            pwd <- Reader(prevWorkDay)
          } yield if (twd.getMonth === nwd.getMonth) nwd else pwd).run

        // this should enrich LocalDate...
        def plusWorkDays(day: LocalDate, offset: Int): LocalDate = ???
      }
    }
  }
}
