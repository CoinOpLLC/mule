package io.deftrade
package time
package money

import io.deftrade.time.work.WorkYear

import scala.collection.immutable.SortedSet
import scala.concurrent.{ duration => scd }

import cats.Order
import cats.data.Reader
import cats.implicits._

import enumeratum._, values._

import java.{ time => jt }
import jt.{ chrono, format, temporal => jtt }, jtt.TemporalUnit
import chrono.{ Chronology, IsoChronology }
import format.DateTimeFormatter

trait Api {

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

    final val period = java.time.Period parse pspec // parse exception is effectily compile-time

    final lazy val field: Map[jtt.ChronoUnit, Int] = {
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
    lazy val P_KALPA: String = (jt.LocalDate.MAX - jt.LocalDate.MIN).toString
    case object F_TERM extends Frequency(P_KALPA) //  0
    case object F1D    extends Frequency("P1D")   //  364 / Y
    case object F1W    extends Frequency("P1W")   //  52
    case object F2W    extends Frequency("P2W")   //  26
    case object F4W    extends Frequency("P4W")   //  13
    case object F13W   extends Frequency("P13W")  //  4
    case object F26W   extends Frequency("P26W")  //  2
    case object F52W   extends Frequency("P52W")  //  1
    case object F1M    extends Frequency("P1M")   //  12
    case object F2M    extends Frequency("P2M")   //  6
    case object F3M    extends Frequency("P3M")   //  4
    case object F4M    extends Frequency("P4M")   //  3
    case object F6M    extends Frequency("P6M")   //  2
    case object F12M   extends Frequency("P12M")  //  1

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
