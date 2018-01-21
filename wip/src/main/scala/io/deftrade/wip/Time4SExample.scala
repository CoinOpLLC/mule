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

package io.deftrade
package wip

import java.{ time => jt }, jt.{ temporal => jtt }
import jt.{ DayOfWeek, Month, Year }, Month.{ JANUARY => January, MARCH => March } // if you must
import jtt.{ ChronoUnit, Temporal, TemporalAdjusters }, ChronoUnit.{ HOURS => Hours, MINUTES => Minutes }

import io.deftrade.time._

import scala.util.Try
import scala.collection.immutable.SortedSet
import scala.concurrent.{ duration => scd }

import cats.Order
import cats.data.Reader
import cats.implicits._

import enumeratum._, values._

object Time4SExample {

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

  val oldRule = years(1) + days(1)
  // val severanceRule = Weeks(2) per Year(10) // how to do this?
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

/**
  * If (if!) we ever have to mix / convert between these types, this is how.
  */
object SyntaxForwardTime {

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

  1.year + 1.day === period(years = 1, days = 1, months = 0) |> assert

  val x = 19.hours + 47.minutes

  import squants.{ time => st }

  val wwDays    = st.Days(5)
  val wwSeconds = wwDays.toSeconds / 3

  import io.deftrade.time.TemporalAdjuster // FIXME: awkward
  // third tuesday
  val TemporalAdjuster(firstTuesday) = TemporalAdjusters firstInMonth DayOfWeek.WEDNESDAY
  val thirdTuesday                   = TemporalAdjuster { firstTuesday andThen (_ plus 2.weeks) }

  def squantsToJavaTime(from: st.Time): Duration = nanos(from.toNanoseconds.toLong)

  // Money4S |> discardValue
}

// issue: can I just go ahead and make these Durations?
sealed abstract class Tenor(val code: String) extends EnumEntry
object Tenor extends Enum[Tenor] {

  case object Spot         extends Tenor("SP") // zero days later
  case object SpotNext     extends Tenor("SN") // one dat later
  case object Overnight    extends Tenor("ON") // one day later
  case object TomorrowNext extends Tenor("TN") // two days later? Sorta...
  // issue: might need a **pair** of dates as state for the algebra... but do we?
  // TomorrowNext increments both cursors, in effect... why, exactly?!
  // type DateCalculatorState = (LocalDate, LocalDate) // maybe?

  lazy val values = findValues

}
// TODO: implicitly enrich LocalDate such that it comprehends the addition of a tenor.

final case class WorkDay(val n: Int) extends AnyVal

// TODO: implicitly enrich LocalDate such that it comprehends the addition of business days

sealed trait SpotLag extends EnumEntry // T_0, T_1, T_2

sealed trait DayCountConvention extends EnumEntry //
//ACT_[360|365|ACT]
// CONV_30_360, CONV_360E_[ISDA|IMSA]
// others?
// idea: build the calc func into this trait?

sealed trait ImmPeriod extends EnumEntry {
  def indexes: SortedSet[Int] // BitSet // [0], [0,2], [2,3], [0,1,2,3]
}

object ImmPeriod {
  def apply(period: ImmPeriod)(year: Year): SortedSet[LocalDate] = ???
}

object WorkTime {

  import jtt.WeekFields

  val iso: WeekFields        = WeekFields.ISO
  val dow: jtt.TemporalField = iso.dayOfWeek

  // n.b.: all `jtt.Temporal`s are also `Comparable`
  implicit def temporalOrder[T <: Temporal with Comparable[T]]: Order[T]   = Order.fromComparable[T]
  implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]

  val today     = localDate
  val yesterday = today - 1.day

  yesterday < today |> assert

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

  object WorkTimeConf {

    val daysOff = Seq(
      (12, 25),
      (1, 1),
      (7, 4)
    ) map { case (m, d) => (2017, m, d) }

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

  import io.deftrade.time.TemporalQuery.TQ

  implicit val monthOrder: Order[Month] = Order.fromComparable[Month]

  type TqReader[R] = Reader[LocalDate, Option[R]]
  type TaReader[R] = Reader[LocalDate, R]

  def workDay(ld: LocalDate)(implicit wy: WorkYear): Boolean = wy workDay ld

  sealed abstract class SeekWorkDay private (delta: Period, sameMonth: Boolean) extends EnumEntry {
    import io.deftrade.time.TemporalAdjuster // FIXME, awkward

    final def temporalAdjuster: jtt.TemporalAdjuster = TemporalAdjuster(adjuster)
    final def adjuster: LocalDate => LocalDate       = _adj(delta, sameMonth)

    private def _adj(d: Period, sm: Boolean): LocalDate => LocalDate = {
      case ld if workDay(ld) && sm && (ld + d).getMonth === ld.getMonth =>
        ld + d
      case ld if workDay(ld) && sm =>
        ld |> _adj(delta.negated, false)
      case ld if workDay(ld) =>
        ld + d
      case ld =>
        val dd = if (d.isNegative) d - 1.day else d + 1.day
        ld |> _adj(dd, sm)
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
/**
Keep Calm, and Carry On.
{{{
  scala> Nanos(Double.MaxValue.toLong)
  res0: com.markatta.timeforscala.Duration = PT2562047H47M16.854775807S
}}}
  */
