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

import syntax._

import cats.implicits._
import cats.{ Order }
import cats.data.{ NonEmptySet }

import enumeratum.{ Enum, EnumEntry }

import scala.collection.immutable.SortedSet

import java.time.DayOfWeek
import java.time.temporal.WeekFields.{ ISO => weekFields }

/**
  * Stick with ISO work calendars '''only''' for now.
  *
  * TODO: make use of locale?
  */
package object work {

  import WorkDay._

  type WorkWeek = NonEmptySet[DayOfWeek]

  /** */
  implicit lazy val WorkWeek: WorkWeek = NonEmptySet(
    weekFields.getFirstDayOfWeek,
    SortedSet((1L to 4L) map (weekFields.getFirstDayOfWeek plus _): _*)
  )

  def yesterday = today - 1.day
  def today     = localDate
  def tomorrow  = today + 1.day

  implicit final class WorkDayOps(val ld: LocalDate) extends AnyVal {

    def isWorkDay(implicit iwd: IsWorkDay): Boolean                  = iwd(ld)
    def workDaysUntil(that: LocalDate)(implicit iwd: IsWorkDay): Int = ???
    def isLastWorkDayOfMonth(implicit iwd: IsWorkDay): Boolean       = ???
    def lastWorkDayOfMonth(implicit iwd: IsWorkDay): LocalDate       = ???
    def next(implicit iwd: IsWorkDay): LocalDate                     = (ld + 1.day).nextOrSame
    def nextOrSame(implicit iwd: IsWorkDay): LocalDate               = ld plusWorkDays 0
    def nextSameOrLastInMonth(implicit iwd: IsWorkDay): LocalDate    = ???
    def previous(implicit iwd: IsWorkDay): LocalDate                 = ???
    def previousOrSame(implicit iwd: IsWorkDay): LocalDate           = ???

    @annotation.tailrec
    def plusWorkDays(nwds: Int)(implicit iwd: IsWorkDay): LocalDate = {

      val signum = math.signum(nwds)

      val adjust: LocalDate => LocalDate = {
        val wd: WorkDay = signum match {
          case 0 | 1 => Next
          case -1    => Prev
        }
        wd adjuster iwd
      }

      def movedByOne = ld + signum.days |> adjust

      nwds match {
        case 0      => adjust(ld)
        case 1 | -1 => movedByOne
        case n      => movedByOne plusWorkDays (n - signum)
      }
    }
  }

  implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]

  implicit def fixedHolidays: FixedHolidays = ???

  implicit def sheduledHoliday(implicit fhs: FixedHolidays): IsScheduledHoliday =
    IsScheduledHoliday(fhs.holidays contains _)

  implicit lazy val unscheduledHolidays: IsUnscheduledHoliday =
    IsUnscheduledHoliday(Set.empty[LocalDate])

  /** FIXME: this should be implicit but then causes ambig... */
  def isWorkDay(
      implicit workWeek: WorkWeek,
      sheduledHoliday: IsScheduledHoliday,
      unsheduledHoliday: IsUnscheduledHoliday
  ): IsWorkDay = IsWorkDay { ld =>
    (workWeek contains ld.dayOfWeek) && !sheduledHoliday(ld) && !unsheduledHoliday(ld)
  }

}

package work {

  /** */
  sealed trait IsDay extends Any {

    /** */
    def is: IsDay.Predicate

    /** */
    final def apply(ld: LocalDate): Boolean = is(ld)
  }

  /** */
  object IsDay {

    /** */
    type Predicate = LocalDate => Boolean
  }

  /** */
  final case class IsScheduledHoliday(val is: IsDay.Predicate) extends AnyVal with IsDay

  /** */
  final case class IsUnscheduledHoliday(val is: IsDay.Predicate) extends AnyVal with IsDay

  /** */
  final case class IsWorkDay(val is: IsDay.Predicate) extends AnyVal with IsDay

  /** */
  sealed abstract class WorkDay private (signum: Int, sameMonth: Boolean) extends EnumEntry {

    /** */
    final def adjuster(implicit iwd: IsWorkDay): LocalDate => LocalDate =
      signum match {
        case 0      => identity
        case 1 | -1 => adjust(0)(signum, sameMonth)
      }

    private def adjust(n: Int)(signum: Int, sameMonth: Boolean)(
        implicit iwd: IsWorkDay
    ): LocalDate => LocalDate =
      ld => {
        val delta = (signum * n).days
        ld + delta match {
          case wd if iwd(wd) =>
            wd
          case hd if sameMonth && (hd.month =!= ld.month) =>
            ld |> adjust(0)(-signum, false)
          case _ =>
            ld |> adjust(n - signum)(signum, sameMonth)
        }
      }
  }

  /** All civilized financial calendars have at least one holiday: domain invariant. */
  sealed abstract case class FixedHolidays(holidays: NonEmptySet[LocalDate])

  /** TODO: Initialize from a config file. */
  object FixedHolidays {

    /** */
    def apply(holidays: NonEmptySet[LocalDate]): FixedHolidays = new FixedHolidays(holidays) {}

    /** */
    def apply(first: LocalDate, rest: Seq[LocalDate]): FixedHolidays =
      apply(NonEmptySet(first, SortedSet(rest: _*)))
  }

  /** TODO: implement `Nearest` from Strata */
  object WorkDay extends Enum[WorkDay] {

    case object Next        extends WorkDay(signum = 1, sameMonth = false)
    case object BoundedNext extends WorkDay(signum = 1, sameMonth = true)
    case object Prev        extends WorkDay(signum = -1, sameMonth = false)
    case object BoundedPrev extends WorkDay(signum = -1, sameMonth = true)

    lazy val values = findValues
  }

  /** TODO: finish this */
  sealed trait WorkMonth extends EnumEntry {
    def adjust(ld: LocalDate): LocalDate = ???
    def isMonthBased: Boolean            = ??? // TODO: when / how does this matter?
  }

  /** TODO: finish this */
  object WorkMonth extends Enum[WorkMonth] {

    lazy val values = findValues

    case object FirstBusinessDay extends WorkMonth
    case object LastBusinessDay  extends WorkMonth
    case object LastDay          extends WorkMonth
  }
}
