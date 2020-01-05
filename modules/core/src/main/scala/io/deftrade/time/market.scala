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
package time

import implicits._, time.work._, keyval._

import scala.collection.immutable.SortedSet
import cats.implicits._

import enumeratum._

import java.{ time => jt }, jt.{ temporal => jtt }

package object market {
  implicit class MarketLocalDate(val ld: LocalDate) extends AnyVal {
    def +(pp: market.ProxiedPeriod): LocalDate = ld plus pp.period
    def -(pp: market.ProxiedPeriod): LocalDate = ld minus pp.period
  }
}

/** Day count conventions, [[Tenor]]s and [[Frequency]]s, etc. */
package market {

  //

  /** TODO: use [[Duration]] instead of [[Period]]? */
  sealed abstract class CashTenor(
      override val entryName: String,
      pspec: String
  ) extends Tenor(pspec)

  object CashTenor extends Enum[CashTenor] {

    case object Spot         extends CashTenor("SP", "P0D")
    case object SpotNext     extends CashTenor("SN", "P1D")
    case object Overnight    extends CashTenor("ON", "P1D")
    case object TomorrowNext extends CashTenor("TN", "P2D")

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

    val ymwd = List(YEARS, MONTHS, WEEKS, DAYS)
    val rx =
      s"""\\A${(ymwd map { x =>
        s"""(?:[0-9]+)${x.toString.charAt(0).toString})?"""
      }).mkString}\\z""".r

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
    case object T1Q         extends Tenor("P3M")
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

  sealed abstract class Frequency(
      override final val pspec: String,
      final val eventsPerYear: Int
  ) extends ProxiedPeriod

  /**
    * Captures common `Frequency` instances. The 364 day year is financial convention.
    */
  object Frequency extends Enum[Frequency] {

    val MonthsPerYear = 12
    val DaysPerWeek   = 7
    val WeeksPerYear  = 52
    val DaysPerYear   = DaysPerWeek * WeeksPerYear
    DaysPerYear === 364 |> assert

    val P_KALPA: String = (jt.LocalDate.MAX - jt.LocalDate.MIN).toString

    case object F_TERM extends Frequency(P_KALPA, 0)
    case object F1D    extends Frequency("P1D", DaysPerYear / 1)
    case object F1W    extends Frequency("P1W", DaysPerYear / DaysPerWeek)
    case object F2W    extends Frequency("P2W", DaysPerYear / (2 * DaysPerWeek))
    case object F4W    extends Frequency("P4W", DaysPerYear / (4 * DaysPerWeek))
    case object F13W   extends Frequency("P13W", WeeksPerYear / 4)
    case object F26W   extends Frequency("P26W", WeeksPerYear / 4)
    case object F52W   extends Frequency("P52W", WeeksPerYear / 1)
    case object F1M    extends Frequency("P1M", MonthsPerYear / 1)
    case object F2M    extends Frequency("P2M", MonthsPerYear / 2)
    case object F3M    extends Frequency("P3M", MonthsPerYear / 3)
    case object F1Q    extends Frequency("P3M", MonthsPerYear / 3)
    case object F4M    extends Frequency("P4M", MonthsPerYear / 4)
    case object F6M    extends Frequency("P6M", MonthsPerYear / 6)
    case object F12M   extends Frequency("P12M", MonthsPerYear / 12)
    case object F1Y    extends Frequency("P12M", MonthsPerYear / 12)

    lazy val values = findValues
  }

  sealed trait DayCount extends EnumEntry {
    def days(start: LocalDate, end: LocalDate): Int
    def years(start: LocalDate, end: LocalDate)(implicit isWd: IsWorkDay): Double
  }
  //ACT_[360|365|ACT]
  // CONV_30_360, CONV_360E_[ISDA|IMSA]
  object DayCount extends Enum[DayCount] {

    lazy val values = findValues

    case object ACT_ACT_ISDA extends DayCount {
      def days(start: LocalDate, end: LocalDate): Int = ???
      def years(start: LocalDate, end: LocalDate)(implicit isWd: IsWorkDay): Double =
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
      def days(start: LocalDate, end: LocalDate): Int                               = ???
      def years(start: LocalDate, end: LocalDate)(implicit isWd: IsWorkDay): Double = ???
    }
  }

  /**
  TODO: implement
  [[http://strata.opengamma.io/apidocs/com/opengamma/strata/basics/schedule/RollConventions.html Roll Conventions]]
    */
  sealed trait RollConvention extends EnumEntry with Serializable

  /** */
  object RollConvetion extends DtEnum[RollConvention] {
    lazy val values = findValues
  }

  // sealed trait ImmPeriod extends EnumEntry {
  //   def indexes: SortedSet[Int] // BitSet // [0], [0,2], [1,3], [0,1,2,3]
  // }
  //
  // object ImmPeriod {
  //   def apply(period: ImmPeriod)(year: Year): SortedSet[LocalDate] = ???
  // }

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

}
