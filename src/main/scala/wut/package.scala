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

import scala.language.implicitConversions
import scala.util.Try
import scala.util.matching.Regex

package object wut extends MyWay with MyTime {

  import _impl._

  /**
    * Civilized function invocation.
    */
  implicit def pipeToFunction1[A](a: A) = PipeToFunction1(a)

  def assertOrElse(msg: String): Boolean => Unit = assert(_, msg)

  def camelToSnake(name: String): String  = camelTo(name)("_")
  def camelToHyphen(name: String): String = camelTo(name)("-")
  def camelToDot(name: String): String    = camelTo(name)(".")
  def camelToWord(name: String): String   = camelTo(name)(" ")

}
package wut {

  final case class PipeToFunction1[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
    def p2f1[B](f: A => B): B = a |> f
  }

  /**
    * This is who we are, it's what we do.
    */
  trait MyWay {

    /**
      * Suppresses warnings from wart remover for cases were the value is intentionally discarded.
      */
    val discardValue: Any => Unit = (_: Any) => ()

    /**
      * See [this post](https://hseeberger.wordpress.com/2013/10/25/attention-seq-is-not-immutable/), and also [these comments](https://disqus.com/home/discussion/heikosblog/attention_seq_is_not_immutable_heikos_blog/).
      */
    type Seq[+A] = scala.collection.immutable.Seq[A]
    val Seq = scala.collection.immutable.Seq

    def safe[T, R](f: T => R): T => Try[R] = t => Try { f(t) }

  }

  trait MyTime {

    // TODO: typeclass for date based vs time based (and DateTime has both) - in fact for all field...

    // import java.{ time => jt }
    //
    // type Clock         = jt.Clock
    // type Month         = jt.Month
    // type Year          = jt.Year
    // type DayOfWeek     = jt.DayOfWeek
    // type LocalTime     = jt.LocalTime
    // type Instant       = jt.Instant
    // type YearMonth     = jt.YearMonth
    // type Duration      = jt.Duration
    // type LocalDate     = jt.LocalDate
    // type LocalDateTime = jt.LocalDateTime
    // type ZonedDateTime = jt.ZonedDateTime
    // type Period        = jt.Period
    // type ZoneId        = jt.ZoneId
    // type ZoneOffset    = jt.ZoneOffset
    //
    // // FIXME: concordance with scala.concurrent etc...
    // type TimeUnit = java.util.concurrent.TimeUnit
    //
    // // let's see if we can do without these...
    // // type IsoChronology = jt.chrono.IsoChronology
    // // type Chronology    = jt.chrono.Chronology
    //
    // type TemporalAccessor = jt.temporal.TemporalAccessor
    // type Temporal         = jt.temporal.Temporal
    // type TemporalAmount   = jt.temporal.TemporalAmount
    //
    // type TemporalUnit      = jt.temporal.TemporalUnit
    // type ChronoUnit        = jt.temporal.ChronoUnit
    // type IsoChronUnit      = jt.temporal.IsoFields
    // type TemporalAdjuster  = jt.temporal.TemporalAdjuster
    // type TemporalAdjusters = jt.temporal.TemporalAdjusters
    //
    // type DateTimeFormatter = jt.format.DateTimeFormatter
    // type FormatStyle       = jt.format.FormatStyle
    //
    // import jt.Month._
    // val January: jt.Month   = JANUARY
    // val February: jt.Month  = FEBRUARY
    // val March: jt.Month     = MARCH
    // val May: jt.Month       = MAY
    // val April: jt.Month     = APRIL
    // val June: jt.Month      = JUNE
    // val July: jt.Month      = JULY
    // val August: jt.Month    = AUGUST
    // val September: jt.Month = SEPTEMBER
    // val October: jt.Month   = OCTOBER
    // val November: jt.Month  = NOVEMBER
    // val December: jt.Month  = DECEMBER
    //
    // def apply(month: Int): Month                 = jt.Month of month
    // def apply(accessor: TemporalAccessor): Month = jt.Month from accessor
    //
    // def unapply(month: Month): Option[Int] = Some(month.getValue)

  }
  object _impl {

    //


    object camelTo {

      // yet another take on an old fav:
      // https://github.com/lift/framework/search?utf8=%E2%9C%93&q=%22def+snakify%22

      // splits off strings of capital letters leaving one...
      private val rx1 = """([A-Z]+)([A-Z][a-z])""".r

      // splits transition from lower -> upper case
      private val rx2 = """([a-z\d])([A-Z])""".r

      private def delimit(rx: Regex)(s: String): String = rx replaceAllIn (s, "$1•$2")

      def apply(name: String)(sep: String): String =
        (name |> delimit(rx1) |> delimit(rx2)) split "•" mkString sep
    }

    def shouldDamnWellBeIdentiyTestMe(s: String): String = camelTo(s)("")
    // def kamelTo(name: String )(sep: String): String =
    //   (name foldRight (Seq.empty[Char])((a, b) => a +: b)).mkString

  }
}
