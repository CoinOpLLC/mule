package io.deftrade
package time
package work

import cats._, implicits._, data._
import enumeratum._
import scala.collection.SortedSet

import java.time._, chrono.{ Chronology, IsoChronology }, format.DateTimeFormatter
import java.time.{ temporal => jtt }
import jtt._, ChronoUnit._
import jtt.WeekFields

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
