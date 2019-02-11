package io.deftrade
package time
package work

import enumeratum._
import cats._
import cats.implicits._
import scala.collection.SortedSet
import java.time.{ Month, DayOfWeek, temporal => jtt }
import jtt.{ Temporal /*, WeekFields */ }

sealed trait IsDay extends Any {
  def is: LocalDate => Boolean
  final def apply(ld: LocalDate): Boolean = is(ld)
}
final case class IsScheduledHoliday(val is: LocalDate => Boolean)   extends AnyVal with IsDay
final case class IsUnscheduledHoliday(val is: LocalDate => Boolean) extends AnyVal with IsDay
final case class IsWorkDay(val is: LocalDate => Boolean)            extends AnyVal with IsDay

trait Api {

  import WorkDay._

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

  // n.b.: all `jtt.Temporal`s are also `Comparable`
  implicit def temporalOrder[T <: Temporal with Comparable[T]]: Order[T]   = Order.fromComparable[T]
  implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]

  def yesterday = today - 1.day
  def today     = localDate
  def tomorrow  = today + 1.day

  // stick with ISO and be rigorous about others
  // TODO: make use of locale?
  type WorkWeek = SortedSet[DayOfWeek] // NonEmpty would be nice, too. Otherwise stay in bed.

  // TODO: add cats stuff to io.deftrade.time?
  implicit val monthOrder: Order[Month] = Order.fromComparable[Month]

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

object WorkDay extends Enum[WorkDay] {
  // FIXME: .conf this shit
  val fixedHolidays = Set(
    (1, 1),
    (7, 4),
    (12, 25)
  )

  implicit lazy val workWeek: WorkWeek =
    SortedSet.empty[DayOfWeek] ++ DayOfWeek.values - DayOfWeek.SATURDAY - DayOfWeek.SUNDAY

  implicit lazy val sheduledHoliday: IsScheduledHoliday =
    IsScheduledHoliday(fixedHolidays contains _.monthDay)

  implicit lazy val unscheduledHolidays: IsUnscheduledHoliday =
    IsUnscheduledHoliday(Set.empty[LocalDate])

  implicit def isWorkDay(
      implicit workWeek: WorkWeek,
      sheduledHoliday: IsScheduledHoliday,
      unsheduledHoliday: IsUnscheduledHoliday
  ): IsWorkDay = IsWorkDay { ld =>
    (workWeek contains ld.dayOfWeek) && !sheduledHoliday(ld) && !unsheduledHoliday(ld)
  }

  case object Next         extends WorkDay(signum = 1, sameMonth = false)
  case object ModifiedNext extends WorkDay(signum = 1, sameMonth = true)
  case object Prev         extends WorkDay(signum = -1, sameMonth = false)
  case object ModifiedPrev extends WorkDay(signum = -1, sameMonth = true)
  // TODO: `Nearest` from Strata

  lazy val values = findValues
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

//////

// type TqReader[R] = Reader[LocalDate, Option[R]]
// type TaReader[R] = Reader[LocalDate, R]

// // there is quite a bit to critique here - not very efficient.
// val sameMonthAdjustWorkDay: LocalDate => LocalDate =
//   (for {
//     twd <- Reader(identity: LocalDate => LocalDate)
//     nwd <- Reader(adjustWorkDay)
//     pwd <- Reader(prevWorkDay)
//   } yield if (twd.getMonth === nwd.getMonth) nwd else pwd).run

// need to define an ordering on DayOfWeek. Which would be great.
// TODO: when does the week roll around?
// TODO: what about that succ stuff from scalaz?

// `TemporalQuery` is the way to do the "is this a working day or not" thing.
// Just build an immutable list of `WorkWeek`s out as far as you can.