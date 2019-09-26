/** */
package io.deftrade
package time

import implicits._

import enumeratum._

import cats.implicits._
import cats.{ Order }
import cats.data.{ NonEmptySet }

import scala.collection.immutable.SortedSet

import java.time.DayOfWeek
import java.time.{ temporal => jtt }

package object work {

  import WorkDay._

  /**
    * stick with ISO for now
    * TODO: make use of locale?
    */
  lazy val weekFields = jtt.WeekFields.ISO

  type WorkWeek = NonEmptySet[DayOfWeek]

  lazy val WorkWeek: WorkWeek = NonEmptySet(
    weekFields.getFirstDayOfWeek,
    SortedSet((1L to 4L) map (weekFields.getFirstDayOfWeek plus _): _*)
  )

  implicit final class WorkDayOps(val ld: LocalDate) extends AnyVal {

    def workDay(implicit iwd: IsWorkDay): Boolean                 = iwd(ld)
    def isLastWorkDayOfMonth(implicit iwd: IsWorkDay): Boolean    = ???
    def next(implicit iwd: IsWorkDay): LocalDate                  = (ld + 1.day).nextOrSame
    def nextOrSame(implicit iwd: IsWorkDay): LocalDate            = ld plusWorkDays 0
    def nextSameOrLastInMonth(implicit iwd: IsWorkDay): LocalDate = ???
    def previous(implicit iwd: IsWorkDay): LocalDate              = ???
    def previousOrSame(implicit iwd: IsWorkDay): LocalDate        = ???

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

  def workDaysBetween(start: LocalDate, end: LocalDate): Int = ???
  def lastWorkDayOfMonth(date: LocalDate): LocalDate         = ???

  def yesterday = today - 1.day
  def today     = localDate
  def tomorrow  = today + 1.day

  implicit def fixedHolidays: FixedHolidays = ???

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

    // final def adjuster: LocalDate => LocalDate = adjuster(implicitly[IsWorkDay])
    // final def temporalAdjuster: TemporalAdjuster = TemporalAdjuster(adjuster)

    final def adjuster(iwd: IsWorkDay): LocalDate => LocalDate =
      signum match {
        case 0      => identity
        case 1 | -1 => adjust(0)(signum, sameMonth)(iwd)
        case _      => ??? // TODO: just use a trinary type here to be ironclad.
      }

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

    def isWorkDay(
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
