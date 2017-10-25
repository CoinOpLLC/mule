package wut

import java.{ time => jt }
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
  import jt.temporal.{ TemporalAdjusters => jttTAs }

  // third tuesday
  val firstTuesday = jttTAs firstInMonth DayOfWeek.TUESDAY
  val thirdTuesday = TemporalAdjuster { _ adjusted firstTuesday plus 2.weeks }

  def squantsToJavaTime(from: st.Time): jt.Duration = Nanos(from.toNanoseconds.toLong)

  Money4S |> discardValue
}

object TemporalAdjuster {
  import java.time.temporal.TemporalAdjuster
  def apply(adjust: Temporal => Temporal): TemporalAdjuster = new TemporalAdjuster {
    override def adjustInto(t: Temporal): Temporal = adjust(t)
  }
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
