package io.deftrade
package time
package work

import enumeratum._
import cats._, implicits._, data._
import scala.collection.SortedSet
import java.time.{ Month, DayOfWeek, temporal => jtt }
import jtt.{ Temporal, WeekFields }

trait Api {

  val iso: WeekFields        = WeekFields.ISO
  val dow: jtt.TemporalField = iso.dayOfWeek

  // n.b.: all `jtt.Temporal`s are also `Comparable`
  implicit def temporalOrder[T <: Temporal with Comparable[T]]: Order[T]   = Order.fromComparable[T]
  implicit def dayOfWeekOrder[T <: DayOfWeek with Comparable[T]]: Order[T] = Order.fromComparable[T]

  def yesterday = today - 1.day
  def today     = localDate
  def tomorrow  = today + 1.day

  // stick with ISO and be rigorous about others
  // TODO: make use of locale?
  type WorkWeek = SortedSet[DayOfWeek]
  type Holidays = SortedSet[LocalDate]

  implicit val monthOrder: Order[Month] = Order.fromComparable[Month]

  type TqReader[R] = Reader[LocalDate, Option[R]]
  type TaReader[R] = Reader[LocalDate, R]

  def workDay(ld: LocalDate)(implicit wy: WorkYear): Boolean = wy workDay ld
  import SeekWorkDay._

  @annotation.tailrec
  final def moveByWorkDays(nwds: Int)(ld: LocalDate): LocalDate = nwds match {
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

// TODO: Make this conf
object WorkTimeConf {
  val daysOff = Seq(
    (12, 25),
    (1, 1),
    (7, 4)
  ) map { case (m, d) => (2018, m, d) }
}

/**
  * do some configure-with-code here...
  */
final case class WorkYear(workWeek: WorkWeek, holidays: Holidays) {
  def workDay(ld: LocalDate): Boolean =
    (workWeek contains ld.dayOfWeek) && !(holidays contains ld)
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

sealed abstract class SeekWorkDay private (delta: Period, sameMonth: Boolean) extends EnumEntry {

  final def temporalAdjuster: jtt.TemporalAdjuster = io.deftrade.time.TemporalAdjuster(adjuster)
  final def adjuster: LocalDate => LocalDate       = adjust(delta, sameMonth)

  private def adjust(d: Period, sameMonth: Boolean): LocalDate => LocalDate = { ld: LocalDate =>
    val dd = ld + d
    ld match {
      case ld if workDay(dd) && sameMonth =>
        if (dd.getMonth === ld.getMonth) dd else ld |> adjust(delta.negated, false)
      case _ if workDay(dd) => dd
      case ld =>
        val succDay = if (d.isNegative) d - 1.day else d + 1.day
        ld |> adjust(succDay, sameMonth)
    }
  }
}

object SeekWorkDay extends Enum[SeekWorkDay] {

  case object Next          extends SeekWorkDay(delta = 1.day, sameMonth = false)
  case object NextSameMonth extends SeekWorkDay(delta = 1.day, sameMonth = true)
  case object Prev          extends SeekWorkDay(delta = -1.day, sameMonth = false)
  case object PrevSameMonth extends SeekWorkDay(delta = -1.day, sameMonth = true)

  lazy val values = findValues
}
