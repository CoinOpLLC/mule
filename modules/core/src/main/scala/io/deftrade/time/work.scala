/** */
package io.deftrade
package time

import implicits._

import enumeratum._

import cats._
import cats.implicits._
import cats.data.NonEmptySet

import scala.collection.immutable.SortedSet

import java.time.DayOfWeek

package object work {

  import WorkDay._

  implicit def fixedHolidays: FixedHolidays = ???

  def workDay(ld: LocalDate): Boolean = { val is = implicitly[IsWorkDay]; is(ld) }
  // def workDaysBetween(start: LocalDate, end: LocalDate): Int
  // def isLastWorkDayOfMonth(date: LocalDate): Boolean
  //
  // def lastWorkDayOfMonth(date: LocalDate): LocalDate

  def next(ld: LocalDate): LocalDate       = nextOrSame(ld + 1.day)
  def nextOrSame(ld: LocalDate): LocalDate = plusWorkDays(0)(ld)
  // def nextSameOrLastInMonth(date: LocalDate): LocalDate
  // def previous(date: LocalDate): LocalDate
  // def previousOrSame(date: LocalDate): LocalDate

  // val iso: WeekFields        = WeekFields.ISO
  // val dow: jtt.TemporalField = iso.dayOfWeek

  def yesterday = today - 1.day
  def today     = localDate
  def tomorrow  = today + 1.day

  // stick with ISO and be rigorous about others
  // TODO: make use of locale?
  type WorkWeek = NonEmptySet[DayOfWeek]

  @annotation.tailrec
  final def plusWorkDays(nwds: Int)(ld: LocalDate)(implicit wd: IsWorkDay): LocalDate = {

    val signum = math.signum(nwds)

    val adjust: LocalDate => LocalDate = {
      val wd: WorkDay = signum match {
        case 0 | 1 => Next
        case -1    => Prev
      }
      wd.adjuster
    }

    def movedByOne = ld + signum.days |> adjust

    nwds match {
      case 0      => adjust(ld)
      case 1 | -1 => movedByOne
      case n      => plusWorkDays(n - signum)(movedByOne)(wd)
    }
  }
}

package work {

  /** */
  object IsDay {
    type Pred = LocalDate => Boolean
  }

  /** */
  sealed trait IsDay extends Any {
    def is: IsDay.Pred
    final def apply(ld: LocalDate): Boolean = is(ld)
  }
  final case class IsScheduledHoliday(val is: IsDay.Pred)   extends AnyVal with IsDay
  final case class IsUnscheduledHoliday(val is: IsDay.Pred) extends AnyVal with IsDay
  final case class IsWorkDay(val is: IsDay.Pred)            extends AnyVal with IsDay

  sealed abstract class WorkDay private (signum: Int, sameMonth: Boolean) extends EnumEntry {

    import WorkDay._

    final def adjuster: LocalDate => LocalDate = adjuster(implicitly)

    final def adjuster(isWd: IsWorkDay): LocalDate => LocalDate =
      signum match {
        case 0      => identity
        case 1 | -1 => adjust(0)(signum, sameMonth)(isWd)
        case _      => ??? // TODO: just use a trinary type here to be ironclad.
      }

    final def temporalAdjuster: TemporalAdjuster = TemporalAdjuster(adjuster)

    private def adjust(n: Int)(signum: Int, sameMonth: Boolean)(isWd: IsWorkDay): LocalDate => LocalDate =
      ld => {
        val delta = (signum * n).days
        ld + delta match {
          case wd if isWd(wd) =>
            wd
          case hd if sameMonth && (hd.month =!= ld.month) =>
            adjust(0)(-signum, false)(isWd)(ld)
          case _ =>
            adjust(n - signum)(signum, sameMonth)(isWd)(ld)
        }
      }
  }

  /** All civilized financial calendars have at least one holiday: domain invariant. */
// FIXME NonEmptySet impl artifact error; work around it
/// case class FixedHolidays(val holidays: NonEmptySet[LocalDate]) extends AnyVal
  case class FixedHolidays(val holidays: SortedSet[LocalDate]) extends AnyVal

  object WorkDay extends Enum[WorkDay] {

    case object Next        extends WorkDay(signum = 1, sameMonth = false)
    case object BoundedNext extends WorkDay(signum = 1, sameMonth = true)
    case object Prev        extends WorkDay(signum = -1, sameMonth = false)
    case object BoundedPrev extends WorkDay(signum = -1, sameMonth = true)
    // TODO: `Nearest` from Strata

    lazy val values = findValues

    implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]

    implicit lazy val workWeek: WorkWeek =
      NonEmptySet(DayOfWeek.MONDAY, SortedSet(DayOfWeek.values: _*) - DayOfWeek.SATURDAY - DayOfWeek.SUNDAY)

    implicit def sheduledHoliday(implicit fhs: FixedHolidays): IsScheduledHoliday =
      IsScheduledHoliday(fhs.holidays contains _)

    implicit lazy val unscheduledHolidays: IsUnscheduledHoliday =
      IsUnscheduledHoliday(Set.empty[LocalDate])

    implicit def isWorkDay(
        implicit workWeek: WorkWeek,
        sheduledHoliday: IsScheduledHoliday,
        unsheduledHoliday: IsUnscheduledHoliday
    ): IsWorkDay = IsWorkDay { ld =>
      (workWeek contains ld.dayOfWeek) && !sheduledHoliday(ld) && !unsheduledHoliday(ld)
    }

  }

// TODO: finish this
  sealed trait WorkMonth extends EnumEntry {
    def adjust(ld: LocalDate): LocalDate = ???
    def isMonthBased: Boolean            = ??? // TODO: when / how does this matter?
  }

  object WorkMonth extends Enum[WorkMonth] {

    lazy val values = findValues

    case object LastDay         extends WorkMonth
    case object LastBusinessDay extends WorkMonth
  }
}
