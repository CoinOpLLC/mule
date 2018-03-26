package io.deftrade
package time
package work

import enumeratum._
import cats._, implicits._ //, data._
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

  // TODO: add cats stuff to io.deftrade.time?
  implicit val monthOrder: Order[Month] = Order.fromComparable[Month]

  def workDay(wy: WorkYear): LocalDate => Boolean            = wy workDay _
  def workDay(ld: LocalDate)(implicit wy: WorkYear): Boolean = wy workDay ld

  /**
    * `adjust` parameter embeds both WorkYear and adjustment convention
    * N.B. Does *not* emulate the strata-basics DaysAdjustment design
    * (where two different adjustments can be embedded.)
    * Instead, do this explicitly through composition, _where necessary_.
    */
  @annotation.tailrec
  final def addWorkDays(nwds: Int, adjust: LocalDate => LocalDate)(ld: LocalDate): LocalDate = {
    def signum     = math.signum(nwds)
    def movedByOne = ld + signum.days |> adjust
    nwds match {
      case 0      => ld |> adjust
      case 1 | -1 => movedByOne
      case n      => addWorkDays(n - signum, adjust)(movedByOne)
    }
  }

  final def examplePlusTenWorkDays(wy: WorkYear): LocalDate => LocalDate =
    addWorkDays(10, WorkDay.Next adjuster (wy copy (holidays = wy.holidays + localDate(2019, 1, 1))))

  // type TqReader[R] = Reader[LocalDate, Option[R]]
  // type TaReader[R] = Reader[LocalDate, R]

  // // there is quite a bit to critique here - not very efficient.
  // val sameMonthAdjustWorkDay: LocalDate => LocalDate =
  //   (for {
  //     twd <- Reader(identity: LocalDate => LocalDate)
  //     nwd <- Reader(adjustWorkDay)
  //     pwd <- Reader(prevWorkDay)
  //   } yield if (twd.getMonth === nwd.getMonth) nwd else pwd).run

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

sealed abstract class WorkDay private (signum: Int, sameMonth: Boolean) extends EnumEntry {

  final def adjuster: LocalDate => LocalDate = adjuster(implicitly)

  final def adjuster(wy: WorkYear): LocalDate => LocalDate =
    signum match {
      case 0      => identity
      case 1 | -1 => adjust(0)(signum, sameMonth)(wy)
      case _      => ??? // TODO: just use a trinary type here to be ironclad.
    }

  final def temporalAdjuster: TemporalAdjuster = TemporalAdjuster(adjuster)

  private def adjust(n: Int)(signum: Int, sameMonth: Boolean)(wy: WorkYear): LocalDate => LocalDate =
    ld => {
      val delta = (signum * n).days
      ld + delta match {
        case wd if wy workDay wd                        => wd
        case hd if sameMonth && (hd.month =!= ld.month) => adjust(0)(-signum, false)(wy)(ld)
        case _                                          => adjust(n + 1)(signum, sameMonth)(wy)(ld)
      }
    }
}

object WorkDay extends Enum[WorkDay] {

  case object Next         extends WorkDay(signum = 1, sameMonth = false)
  case object ModifiedNext extends WorkDay(signum = 1, sameMonth = true)
  case object Prev         extends WorkDay(signum = -1, sameMonth = false)
  case object ModifiedPrev extends WorkDay(signum = -1, sameMonth = true)
  case object WorkEveryDay extends WorkDay(signum = 0, sameMonth = false)

  lazy val values = findValues
}

sealed trait WorkMonth extends EnumEntry {
  def adjust(ld: LocalDate): LocalDate = ???
  def isMonthBased: Boolean            = ??? // TODO: when / how does this matter?
}

object WorkMonth extends Enum[WorkMonth] {

  lazy val values = findValues

  case object LastDay         extends WorkMonth
  case object LastBusinessDay extends WorkMonth

}
