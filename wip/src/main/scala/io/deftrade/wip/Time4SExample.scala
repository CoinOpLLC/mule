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

import java.{ time => jt }
import jt.{ temporal => jtt }
import jtt.{ TemporalAdjuster, TemporalField, TemporalQuery }

import scala.util.Try
import scala.collection.immutable.SortedSet
import scala.concurrent.{ duration => scd }

import com.markatta.timeforscala._
import com.markatta.timeforscala.Month.{ January, March }
import com.markatta.timeforscala.TimeExpressions._

import cats.Order
import cats.data.Reader
import cats.implicits._

import enumeratum._, values._

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

  // third tuesday
  val TemporalAdjuster(firstTuesday) = jtt.TemporalAdjusters firstInMonth jt.DayOfWeek.WEDNESDAY
  val thirdTuesday                   = TemporalAdjuster { firstTuesday andThen (_ plus 2.weeks) }

  def squantsToJavaTime(from: st.Time): jt.Duration = Nanos(from.toNanoseconds.toLong)

  Money4S |> discardValue
}

object TemporalAdjuster {

  def apply(adjust: LocalDate => LocalDate): TemporalAdjuster = new TemporalAdjuster {
    // justification: creating an instance of a java lib class; this is what the doc says: throw.
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    override def adjustInto(t: Temporal): Temporal = t match {
      case ld: LocalDate => ld |> adjust
      case _ =>
        throw new jt.DateTimeException(
          s"only args of type LocalDate are accepted; found ${t.getClass}"
        )
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unapply(ta: TemporalAdjuster): Option[LocalDate => LocalDate] =
    Some(
      localDate =>
        // justification for this downcast:
        // per the `TemporalAdjuster` javadocs, and I quote:
        // > The returned object must have the same observable type as the input object
        // i.e., skating on thin ice, assuming this is obeyed.
        (ta adjustInto localDate).asInstanceOf[LocalDate]
    )
}

object TemporalQuery {
  import jtt.{ TemporalQueries => TQs }
  type TQ[+R] = LocalDate => R // TODO: interface design: consider R := LocalDate => LocalDate... !
  // in other words: A TQ[LocalDate => LocalDate] Query produces an Adjuster from a LocalDate
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

  val iso: WeekFields    = WeekFields.ISO
  val dow: TemporalField = iso.dayOfWeek

  // n.b.: all `jtt.Temporal`s are also `Comparable`
  implicit def temporalOrder[T <: Temporal with Comparable[T]]: Order[T]   = Order.fromComparable[T]
  implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]

  val today     = LocalDate()
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

    import jt.DayOfWeek

    lazy val workWeek: WorkWeek =
      SortedSet.empty[DayOfWeek] ++ DayOfWeek.values - DayOfWeek.SATURDAY - DayOfWeek.SUNDAY

    private def mkLD: (Int, Int, Int) => LocalDate = LocalDate.apply(_, _, _)

    lazy val holidays: Holidays = SortedSet.empty[LocalDate] ++ (daysOff map mkLD.tupled)

    implicit lazy val workYear: WorkYear = WorkYear(workWeek, holidays)

  }

  // need to define an ordering on DayOfWeek. Which would be great.
  // TODO: when does the week roll around?
  // TODO: what about that succ stuff from scalaz?

  // `TemporalQuery` is the way to do the "is this a working day or not" thing.
  // Just build an immutable list of `WorkWeek`s out as far as you can.

  import TemporalQuery.TQ

  implicit val monthOrder: Order[Month] = Order.fromComparable[Month]

  type TqReader[R] = Reader[LocalDate, Option[R]]
  type TaReader[R] = Reader[LocalDate, R]

  def workDay(ld: LocalDate)(implicit wy: WorkYear): Boolean = wy workDay ld

  sealed abstract class SeekWorkDay private (delta: Period, sameMonth: Boolean) extends EnumEntry {

    final def temporalAdjuster: TemporalAdjuster = TemporalAdjuster(adjuster)
    final def adjuster: LocalDate => LocalDate   = _adj(delta, sameMonth)

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
// FROM branch vcdenom
// package wut
//
// import java.{ time => jt }
// import jt.{ temporal => jtt }
// import com.markatta.timeforscala._
// import com.markatta.timeforscala.Month.{ January, March }
//
// object Time4SExample {
//
//   // java.time.Duration
//   val d1 = Duration(seconds = 20, nanos = 1)
//   val d2 = Hours(10)
//   val d3 = Minutes(10)
//   val d4 = Seconds(10)
//   val d5 = Millis(10)
//   val d6 = Nanos(10)
//
//   // java.time.Perod
//   val p1 = Period(years = 1, months = 2, days = 3)
//   val p2 = Days(10)
//   val p3 = Weeks(10)
//   val p4 = Months(10)
//   val p5 = Years(10)
//
//   val oldRule = Years(1) + Days(1)
//   // val severanceRule = Weeks(2) per Year(10) // how to do this?
//   val quarterYear = Months(12 / 4)
//
//   // java.time.LocalDate
//   val ld1 = LocalDate(2015, 1, 1)
//   val ld2 = LocalDate(2015, January, 1)
//   val ld3 = LocalDate("2015-01-01")
//
//   // java.time.LocalTime
//   val lt1 = LocalTime(20, 30)
//   val lt2 = LocalTime(hour = 20, minute = 30, second = 12, nano = 5)
//
//   // java.time.LocalDateTime
//   val ldt1 = LocalDateTime(2015, 1, 1, 20, 30, 5)
//   val ldt3 = LocalDateTime(2015, January, 1, 20, 30, 5)
//   val ldt4 = LocalDateTime(LocalDate(2015, 1, 1), LocalTime(20, 30, 5))
//   val ldt5 = LocalDateTime("2015-01-01T20:30:05")
//
//   // java.time.ZonedDateTime
//   val zdt1 = ZonedDateTime(LocalDate(2015, 1, 1), LocalTime(20, 30, 10), ZoneId("GMT"))
//
//   // java.time.YearMonth
//   val ym1 = YearMonth(2015, 1)
//   val ym2 = YearMonth(2015, January)
//
// // java.time.Duration
//   assert(Minutes(20) - Minutes(10) == Minutes(10))
//   assert(Duration(10) + Duration(10) == Duration(20))
//   assert(Duration(20) / 5 == Duration(4))
//   assert(Duration(10) * 5 == Duration(50))
//
// // java.time.Period
//   assert(Days(1) + Days(1) == Days(2))
//   assert(Months(2) - Months(1) == Months(1))
//
// // java.time.LocalTime
//   assert(LocalTime(20, 30) + Minutes(5) == LocalTime(20, 35))
//   assert(LocalTime(20, 30) - Minutes(5) == LocalTime(20, 25))
//
// // java.time.LocalDate
//   assert(LocalDate(2015, January, 1) + Months(2) == LocalDate(2015, March, 1))
//   assert(LocalDate(2015, March, 1) - Months(2) == LocalDate(2015, January, 1))
//
// }
//
// /**
//   * If (if!) we ever have to mix / convert between these types, this is how.
//   */
// object SyntaxForwardTime {
//   implicit class WithHack(val value: Temporal) extends AnyVal {
//     def adjusted(ta: java.time.temporal.TemporalAdjuster) = value `with` ta
//   }
//
//   import com.markatta.timeforscala.TimeExpressions._
//
//   import scala.concurrent.{ duration => scd }
//   import cats.syntax.eq._
//
//   implicit val scdDurationEq = cats.Eq.fromUniversalEquals[scd.Duration]
//   implicit val durationEq    = cats.Eq.fromUniversalEquals[Duration]
//   implicit val periodEq      = cats.Eq.fromUniversalEquals[Period]
//
//   val t2fd: scd.Duration = 2.seconds.toFiniteDuration
//   t2fd === scd.Duration(t2fd.toString) |> assert
//
//   val t1 = 1.nano
//   val t2 = 2.millis
//   val t3 = 2.seconds
//   val t4 = 3.minutes
//   val t5 = 1.hour
//
//   val t6 = 5.days
//   val t7 = 1.week
//   val t8 = 7.months
//   val t9 = 2.years
//
//   1.year + 1.day === Period(years = 1, days = 1, months = 0) |> assert
//
//   val x = 19.hours + 47.minutes
//
//   import squants.{ time => st }
//
//   val wwDays    = st.Days(5)
//   val wwSeconds = wwDays.toSeconds / 3
//
//   import jt.DayOfWeek
//   import jtt.{ TemporalAdjusters => jttTAs }
//
//   // third tuesday
//   val firstTuesday = jttTAs firstInMonth DayOfWeek.TUESDAY
//   val thirdTuesday = TemporalAdjuster { _ adjusted firstTuesday plus 2.weeks }
//
//   def squantsToJavaTime(from: st.Time): jt.Duration = Nanos(from.toNanoseconds.toLong)
//
//   Money4S |> discardValue
// }
//
// object TemporalAdjuster {
//   import java.time.temporal.TemporalAdjuster
//   def apply(adjust: Temporal => Temporal): TemporalAdjuster = new TemporalAdjuster {
//     override def adjustInto(t: Temporal): Temporal = adjust(t)
//   }
// }
//
// /**
// Keep Calm, and Carry On.
// {{{
//   scala> Nanos(Double.MaxValue.toLong)
//   res0: com.markatta.timeforscala.Duration = PT2562047H47M16.854775807S
//
//
// }}}
//
//
//   */
// object Money4S {
//
//   def showme[T <: AnyRef](t: T): Unit = println(t)
//
//   import squants.{ market => sm }
//
//   implicit val moneyContext = sm.defaultMoneyContext
//
//   val doubleEagle  = sm.Money(20)
//   val silverTalent = 15 * 12 * sm.Money(1, sm.XAG)
//
//   import java.{ util => ju }
//   val javabux = ju.Currency.getAvailableCurrencies
//
// }
//
// object FansiCrap {
//   import fansi.Color.{ LightMagenta }
//   def colorme[S <: AnyRef](s: S): String = (fansi.Str(s.toString) overlay LightMagenta).render
//   def fade(n: Int) =
//     (
//       (0 to 255) map { i =>
//         fansi.Back.True(i, 255 - i, 255)(" ")
//       } grouped n map (_.mkString)
//     ) mkString "\n"
//
//   // fade(32) |> showme
// }
//
// object WeekTimeStuff {
//
//   import jt.DayOfWeek
//   import jtt.WeekFields
//   // dayOfWeek   getFirstDayOfWeek getMinimalDaysInFirstWeek
//   // weekBasedYear   weekOfMonth  weekOfWeekBasedYear   weekOfYear
//
//   // issue to get correct by construction: dealing with `DayOfWeek` int values across `Locale`s
//
//   // 2 `WeekField`s of note: ISO and SundayStart (used in Asia)
//   // otherwise use `Locale`
//
//   // scala>   val dow = ISO.dayOfWeek
//   // dow: java.time.temporal.TemporalField = DayOfWeek[WeekFields[MONDAY,4]]
//   //
//   // scala>   val sow = jtt.WeekFields.SUNDAY_START.dayOfWeek
//   // sow: java.time.temporal.TemporalField = DayOfWeek[WeekFields[SUNDAY,1]]
//   //
//   // scala> today get dow
//   // res108: Int = 6
//   //
//   // scala> today get sow
//   // res109: Int = 7
//
//   val iso = WeekFields.ISO
//   val dow = iso.dayOfWeek
//
//   import cats.Order
//   import cats.syntax.all._
//
//   import com.markatta.timeforscala._
//   import com.markatta.timeforscala.TimeExpressions._
//
//   // Time Lording...
//   // implicit def temporalEq[T <: Temporal]: Eq[T] = Eq.fromUniversalEquals[T]
//   implicit def temporalOrder[T <: Temporal with Comparable[T]]: Order[T]   = Order.fromComparable[T]
//   implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]
//
//   val today     = LocalDate()
//   val yesterday = today - 1.day
//
//   yesterday < today |> assert
//
//   // adopt a Firm wide convention
//   import scala.collection.immutable.SortedSet
//
//   // val coinopWeekFields = jtt.WeekFields of (jt.DayOfWeek.MONDAY, 3)
//   // better idea: stick with ISO and be rigorous about others
//   // make use of locale
//   type WorkWeek = SortedSet[jt.DayOfWeek]
//   // need to define an ordering on DayOfWeek. Which would be great.
//   // TODO: when does the week roll around?
//   // TODO: what about that succ stuff from scalaz?
// }
