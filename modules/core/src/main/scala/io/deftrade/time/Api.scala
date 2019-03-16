package io.deftrade
package time

import cats.{ Eq, Hash, Order, Show }

import java.time._, chrono.Chronology, format.DateTimeFormatter
import java.time.{ temporal => jtt }
import jtt._, ChronoUnit._

/**
  * Single-import package providing minimal scala-ideomatic bindings for java.time.
  */
trait Api {

  // Clock

  type Clock = java.time.Clock

  def clockDefaultZone                      = Clock.systemDefaultZone
  def clockDefaultUTC                       = Clock.systemUTC
  def clockFrozen(i: Instant, zone: ZoneId) = Clock fixed (i, zone)

  def clock(zone: ZoneId) = Clock system zone

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

  type TemporalAdjuster = java.time.temporal.TemporalAdjuster

  /** */
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

  /**
    * This name is free for `value`s
    * because it is occupied only by a lone java interface in `type` namespace`
    */
  object TemporalQuery {
    import cats.implicits._ // n.b. this is "just syntax sugar" here but it's _very_ sweet...
    import jtt.{ TemporalQueries => TQs }

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

  // LocalDateTime

  type LocalDateTime = java.time.LocalDateTime

  def localDateTime               = LocalDateTime.now
  def localDateTime(zone: ZoneId) = LocalDateTime now zone
  def localDateTime(clock: Clock) = LocalDate now clock

  def localDateTime(ta: TemporalAccessor) = LocalDate from ta

  def localDateTime(iso8601: String) = LocalDate parse iso8601

  def localDateTime(s: String, fmt: DateTimeFormatter) = LocalDate parse (s, fmt)

  def localDateTime(ld: LocalDate, lt: LocalTime) = LocalDateTime of (ld, lt)

  object LocalDateTimeOf {
    def unapply(dt: LocalDateTime) = Option((dt.toLocalDate, dt.toLocalTime))
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

  // ZonedDateTime

  type ZonedDateTime = java.time.ZonedDateTime

  def zonedDateTime(ld: LocalDate, lt: LocalTime, zone: ZoneId) =
    ZonedDateTime of (ld, lt, zone)

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

  import collection.JavaConverters._

  type ZoneId = java.time.ZoneId

  def zoneId                                           = ZoneId.systemDefault()
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

  abstract class FormatShow[TA <: TemporalAccessor](val formatter: DateTimeFormatter, tq: TemporalAccessor => TA) extends Show[TA] {
    override def show(x: TA): String = formatter format x

    // TODO: refactor this where it belongs (i.e. will pick up operator)
    def parse(s: String): Either[Throwable, TA] =
      scala.util.Try {
        tq(formatter parse s)
      }.toEither
  }

  implicit lazy val shoLocalDate =
    new FormatShow[LocalDate](DateTimeFormatter.ISO_LOCAL_DATE_TIME, LocalDate.from(_)) with Hash[LocalDate] with cats.Order[LocalDate] {
      override def hash(x: LocalDate): Int                  = x.hashCode
      override def compare(x: LocalDate, y: LocalDate): Int = x compareTo y
    }

  implicit lazy val shoLocalTime =
    new FormatShow[LocalTime](DateTimeFormatter.ISO_LOCAL_DATE_TIME, LocalTime.from(_)) with Hash[LocalTime] with cats.Order[LocalTime] {
      override def hash(x: LocalTime): Int                  = x.hashCode
      override def compare(x: LocalTime, y: LocalTime): Int = x compareTo y
    }

  implicit lazy val shoLocalDateTime =
    new FormatShow[LocalDateTime](DateTimeFormatter.ISO_LOCAL_DATE_TIME, LocalDateTime.from(_)) with Hash[LocalDateTime]
    with cats.Order[LocalDateTime] {
      override def hash(x: LocalDateTime): Int                      = x.hashCode
      override def compare(x: LocalDateTime, y: LocalDateTime): Int = x compareTo y
    }

  implicit lazy val shoInstant =
    new FormatShow[Instant](DateTimeFormatter.ISO_LOCAL_DATE_TIME, Instant.from(_)) with Hash[Instant] with cats.Order[Instant] {
      override def hash(x: Instant): Int                = x.hashCode
      override def compare(x: Instant, y: Instant): Int = x compareTo y
    }
}
