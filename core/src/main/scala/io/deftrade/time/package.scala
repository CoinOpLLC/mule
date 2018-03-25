package io.deftrade

import java.time._, chrono.{ Chronology, IsoChronology }, format.DateTimeFormatter
import java.time.{ temporal => jtt }
import jtt._, ChronoUnit._

package object time {

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

  final val ZoneIdGmt = ZoneId of "GMT"
  final val ZoneIdUtc = ZoneId of "UTC"
  final val ZoneIdUt  = ZoneId of "UT"
  final val ZoneIdZ   = ZoneId of "Z"

  object ZoneIdOf {} // TODO: finish ZoneId including sugar

  // Exceptions

  type DateTimeException                = java.time.DateTimeException
  type DateTimeParseException           = java.time.format.DateTimeParseException
  type UnsupportedTemporalTypeException = java.time.temporal.UnsupportedTemporalTypeException
  type ZoneRulesException               = java.time.zone.ZoneRulesException

}
