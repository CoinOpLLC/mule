package wut

import java.{ time => jt }
import jt.{ temporal => jtt }
import com.markatta.timeforscala._
import com.markatta.timeforscala.Month.{ January, March }

object Time4SExample {

  // java.time.Duration
  val d1 = Duration(seconds = 20, nanos = 1)
  val d2 = Hours(10)
  val d3 = Minutes(10)
  val d4 = Seconds(10)
  val d5 = Millis(10)
  val d6 = Nanos(10)

  // java.time.Perod
  val p1 = Period(years = 1, months = 2, days = 3)
  val p2 = Days(10)
  val p3 = Weeks(10)
  val p4 = Months(10)
  val p5 = Years(10)

  val oldRule = Years(1) + Days(1)
  // val severanceRule = Weeks(2) per Year(10) // how to do this?
  val quarterYear = Months(12 / 4)

  // java.time.LocalDate
  val ld1 = LocalDate(2015, 1, 1)
  val ld2 = LocalDate(2015, January, 1)
  val ld3 = LocalDate("2015-01-01")

  // java.time.LocalTime
  val lt1 = LocalTime(20, 30)
  val lt2 = LocalTime(hour = 20, minute = 30, second = 12, nano = 5)

  // java.time.LocalDateTime
  val ldt1 = LocalDateTime(2015, 1, 1, 20, 30, 5)
  val ldt3 = LocalDateTime(2015, January, 1, 20, 30, 5)
  val ldt4 = LocalDateTime(LocalDate(2015, 1, 1), LocalTime(20, 30, 5))
  val ldt5 = LocalDateTime("2015-01-01T20:30:05")

  // java.time.ZonedDateTime
  val zdt1 = ZonedDateTime(LocalDate(2015, 1, 1), LocalTime(20, 30, 10), ZoneId("GMT"))

  // java.time.YearMonth
  val ym1 = YearMonth(2015, 1)
  val ym2 = YearMonth(2015, January)

// java.time.Duration
  assert(Minutes(20) - Minutes(10) == Minutes(10))
  assert(Duration(10) + Duration(10) == Duration(20))
  assert(Duration(20) / 5 == Duration(4))
  assert(Duration(10) * 5 == Duration(50))

// java.time.Period
  assert(Days(1) + Days(1) == Days(2))
  assert(Months(2) - Months(1) == Months(1))

// java.time.LocalTime
  assert(LocalTime(20, 30) + Minutes(5) == LocalTime(20, 35))
  assert(LocalTime(20, 30) - Minutes(5) == LocalTime(20, 25))

// java.time.LocalDate
  assert(LocalDate(2015, January, 1) + Months(2) == LocalDate(2015, March, 1))
  assert(LocalDate(2015, March, 1) - Months(2) == LocalDate(2015, January, 1))

}

/**
  * If (if!) we ever have to mix / convert between these types, this is how.
  */
object SyntaxForwardTime {
  implicit class WithHack(val value: Temporal) extends AnyVal {
    def adjusted(ta: java.time.temporal.TemporalAdjuster) = value `with` ta
  }

  import com.markatta.timeforscala.TimeExpressions._

  import scala.concurrent.{ duration => scd }
  import cats.syntax.eq._

  implicit val scdDurationEq = cats.Eq.fromUniversalEquals[scd.Duration]
  implicit val durationEq    = cats.Eq.fromUniversalEquals[Duration]
  implicit val periodEq      = cats.Eq.fromUniversalEquals[Period]

  val t2fd: scd.Duration = 2.seconds.toFiniteDuration
  t2fd === scd.Duration(t2fd.toString) |> assert

  val t1 = 1.nano
  val t2 = 2.millis
  val t3 = 2.seconds
  val t4 = 3.minutes
  val t5 = 1.hour

  val t6 = 5.days
  val t7 = 1.week
  val t8 = 7.months
  val t9 = 2.years

  1.year + 1.day === Period(years = 1, days = 1, months = 0) |> assert

  val x = 19.hours + 47.minutes

  import squants.{ time => st }

  val wwDays    = st.Days(5)
  val wwSeconds = wwDays.toSeconds / 3

  import jt.DayOfWeek
  import jtt.{ TemporalAdjusters => jttTAs }

  // third tuesday
  val firstTuesday = jttTAs firstInMonth DayOfWeek.TUESDAY
  val thirdTuesday = TA { _ adjusted firstTuesday plus 2.weeks }

  def squantsToJavaTime(from: st.Time): jt.Duration = Nanos(from.toNanoseconds.toLong)

  Money4S |> discardValue
}

/**
Keep Calm, and Carry On.
{{{
  scala> Nanos(Double.MaxValue.toLong)
  res0: com.markatta.timeforscala.Duration = PT2562047H47M16.854775807S


}}}


  */
object Money4S {

  def showme[T <: AnyRef](t: T): Unit = println(t)

  import squants.{ market => sm }

  implicit val moneyContext = sm.defaultMoneyContext

  val doubleEagle  = sm.Money(20)
  val silverTalent = 15 * 12 * sm.Money(1, sm.XAG)

  import java.{ util => ju }
  val javabux = ju.Currency.getAvailableCurrencies

}

object FansiCrap {
  import fansi.Color.{ LightMagenta }
  def colorme[S <: AnyRef](s: S): String = (fansi.Str(s.toString) overlay LightMagenta).render
  def fade(n: Int) =
    (
      (0 to 255) map { i =>
        fansi.Back.True(i, 255 - i, 255)(" ")
      } grouped n map (_.mkString)
    ) mkString "\n"

  // fade(32) |> showme
}

import jtt.{ TemporalAccessor, TemporalQuery }

import jt.chrono.Chronology

object TA {
  import java.time.temporal.TemporalAdjuster
  type TA[+R <: Temporal] = LocalDate => R
  type TALD               = TA[LocalDate]
  def apply[R <: Temporal](ta: TA[R]): TemporalAdjuster = new TemporalAdjuster {
    override def adjustInto(t: Temporal): R = t match {
      case ld: LocalDate => ld |> ta
      case _             => ??? // I know, right?
    }
  }
}

object TQ {
  import cats.syntax.either._, cats.syntax.option._
  import jtt.{ TemporalQueries => TQs }
  type TQ[+R] = LocalDate => R
  def asTemporalQuery[R](tq: TQ[R]): TemporalQuery[Option[R]] = new TemporalQuery[Option[R]] {
    override def queryFrom(ta: TemporalAccessor): Option[R] = ta match {
      case ld: LocalDate => (ld |> tq).some
      case _             => none
    }
  }

  def chronology: TQ[Option[Chronology]]  = optionize(_ query TQs.chronology)
  def precision: TQ[Option[TemporalUnit]] = optionize(_ query TQs.precision)

  def localDate: TQ[Option[LocalDate]] = optionize(_ query TQs.localDate)
  def localTime: TQ[Option[LocalTime]] = optionize(_ query TQs.localTime)

  def offset: TQ[Option[ZoneOffset]] = optionize(_ query TQs.offset)
  def zoneId: TQ[Option[ZoneId]]     = optionize(_ query TQs.zoneId)
  def zone: TQ[Either[Option[ZoneOffset], ZoneId]] = _ query TQs.zone match {
    case zo: ZoneOffset     => zo.some.asLeft // (sic) ZoneOffset <: ZoneId – too clever...
    case zid if zid != null => zid.asRight // actual ZoneId
    case _                  => none.asLeft // not applicable - zone makes no sense
  }

  private def optionize[R](tq: TQ[R]): TQ[Option[R]] = ta => Option(ta |> tq)
}

object WeekTimeStuff {

  import jt.DayOfWeek
  import jtt.WeekFields

  val iso = WeekFields.ISO
  val dow = iso.dayOfWeek

  import cats.Order
  import cats.syntax.all._

  import com.markatta.timeforscala._
  import com.markatta.timeforscala.TimeExpressions._

  // Time Lording...
  // implicit def temporalEq[T <: Temporal]: Eq[T] = Eq.fromUniversalEquals[T]
  implicit def temporalOrder[T <: Temporal with Comparable[T]]: Order[T]   = Order.fromComparable[T]
  implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]

  val today     = LocalDate()
  val yesterday = today - 1.day

  yesterday < today |> assert

  // adopt a Firm wide convention
  import scala.collection.immutable.SortedSet

  // val coinopWeekFields = jtt.WeekFields of (jt.DayOfWeek.MONDAY, 3)
  // better idea: stick with ISO and be rigorous about others
  // make use of locale
  type WorkWeek = SortedSet[jt.DayOfWeek]
  type Holidays = SortedSet[jt.LocalDate] // TODO: is day gran sufficient?
  // need to define an ordering on DayOfWeek. Which would be great.
  // TODO: when does the week roll around?
  // TODO: what about that succ stuff from scalaz?

  // `TemporalQuery` is the way to do the "is this a working day or not" thing.
  // Just build an immutable list of `WorkWeek`s out as far as you can.

  import cats.data.Reader
  import TQ.TQ
  import TA.{ TA, TALD }

  implicit val monthOrder: Order[Month] = Order.fromComparable[Month]

  type TqReader[R] = Reader[LocalDate, Option[R]]
  type TaReader[R] = Reader[LocalDate, R]

  type HolidayPredicate = LocalDate => Boolean
  val holidayPredicate: HolidayPredicate = _ => false
  val isWorkDay: TQ[Boolean]             = holidayPredicate(_)

  @annotation.tailrec
  private[this] def findWorkDay(delta: Period)(ld: LocalDate): LocalDate = ld match {
    case d if d |> isWorkDay => d
    case _ => findWorkDay(delta)(ld + delta)
  }

  val nextWorkDay: TALD = findWorkDay(1.day)
  val prevWorkDay: TALD = findWorkDay(-1.day)

  val sameMonthNextWorkDay = {
    val x = for {
      ld <- (identity[LocalDate] _).pure[TaReader]
      // nwd <- nextWorkDay.pure[TaReader]
      // pwd <- prevWorkDay.pure[TaReader]
      // b = day.getMonth === nwd(ld).getMonth
    } yield ld //yield if (b) nwd else pwd
    x.run
  }

  // this should enrich LocalDate...
  def plusWorkDays(day: LocalDate, offset: Int): LocalDate = ???
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
