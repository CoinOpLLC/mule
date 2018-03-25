package io.deftrade
package time

import cats._, implicits._, data._
import enumeratum._
import scala.collection.SortedSet

import java.time._, chrono.{ Chronology, IsoChronology }, format.DateTimeFormatter
import java.time.{ temporal => jtt }
import jtt._, ChronoUnit._
import jtt.WeekFields

package object work {

  import jtt.WeekFields
  import cats._, implicits._, data._

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
