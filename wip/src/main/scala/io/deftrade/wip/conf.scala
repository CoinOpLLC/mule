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
package wip

//import java.time.LocalDate
import com.markatta.timeforscala.LocalDate

import cats.Eq
import cats.instances.int._
import cats.syntax.eq._
import cats.syntax.either._

object EnumeratumExamples {
  import enumeratum._
  import enumeratum.values._

  /*
   * Using `enumeratum`.
   */
  sealed trait Greeting extends EnumEntry
  object Greeting extends Enum[Greeting] {

    /*
       `findValues` is a protected method that invokes a macro to find all `Greeting` object declarations inside an `Enum`

       You use it to implement the `val values` member
     */
    val values = findValues

    case object Hello   extends Greeting
    case object GoodBye extends Greeting
    case object Hi      extends Greeting
    case object Bye     extends Greeting
    case object Shalom  extends Greeting
    case object Aloha   extends Greeting
    case object Peace   extends Greeting

    implicit val greetingEq = Eq.fromUniversalEquals[Greeting]

  }

  sealed abstract class LibraryItem(val value: Int, val name: String) extends IntEnumEntry

  case object LibraryItem extends IntEnum[LibraryItem] {

    case object Book extends LibraryItem(value = 1, name = "book")

    /*
      named params in non-canonic order mess up the macro and cause a unit statement. Still works, but wartremover complains... wild.
     */
    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    case object Movie extends LibraryItem(name = "movie", value = 2)

    case object Magazine extends LibraryItem(3, "magazine")

    case object CD extends LibraryItem(4, name = "cd")
    // case object Newspaper extends LibraryItem(4, name = "newspaper") <-- will fail to compile because the value 4 is shared

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    case object DVD extends LibraryItem(name = "dvd", value = 7)
    /*
      val five = 5
      case object Article extends LibraryItem(five, name = "article") <-- will fail to compile because the value is not a literal
     */

    val values = findValues

  }
  implicit val libEq = Eq.fromUniversalEquals[LibraryItem]

}

case class ConfDate(val date: LocalDate) // extends AnyVal
object ConfDate {
  implicit val confDateEq = Eq.fromUniversalEquals[ConfDate]
}

object RefinedExamples {

  /**
    * Building a configuration case class tree here – but still pure scala, independent of
    config library format or reader.
    */
  import eu.timepit.refined
  import refined.api.Refined
  import refined.W
  import refined.collection._
  import refined.numeric._

  type NonSystemPort = Int Refined Interval.Closed[W.`1024`.T, W.`65535`.T]

  type ApiKey = SyntaxHighlightKiller.ApiKey

  import EnumeratumExamples.Greeting

  type PositiveInt    = Int Refined Positive
  type NonNegativeInt = Int Refined NonNegative

  case class ScheduleSettings(
      initialDelaySeconds: NonNegativeInt,
      intervalMinutes: PositiveInt,
      startDate: ConfDate,
      greeting: Greeting
  )
  object ScheduleSettings {
    implicit val sheduleSettingsEq = Eq.fromUniversalEquals[ScheduleSettings]
  }

  case class Settings(
      name: String Refined NonEmpty,
      port: NonSystemPort,
      apiKey: ApiKey,
      schedule: ScheduleSettings
  )
  object Settings {
    implicit val settingsEq = Eq.fromUniversalEquals[Settings]
  }
}

object SpireExamples {

  import spire.syntax.{ literals => sslits }
  import sslits.literals // the implicit, explicitly. (sic)
  import sslits.si._    // .us and .eu also available
  import sslits.radix._ // imports the implicit

  // bytes and shorts
  val x    = b"100" // without type annotation!
  val y    = h"999"
  val mask = b"255" // unsigned constant converted to signed (-1)

  // rationals
  val n1 = r"1/3"
  val n2 = r"1599/115866" // simplified at compile-time to 13/942

  val a           = x2"10111" // binary
  val b           = x8"27" // octal
  val c           = x16"17" // hex
  val twentyThree = Seq(a, b, c) forall { _ === 23 }
  twentyThree |> assert

  // SI notation for large numbers

  val ww = i"1 944 234 123"                                 // Int
  val xx = j"89 234 614 123 234 772"                        // Long
  val yy = big"123 234 435 456 567 678 234 123 112 234 345" // BigInt
  val zz = dec"1 234 456 789.123456789098765"               // BigDecimal

  /**
    * Demonstrates that spire literals can be used with Refined.
    */
  import RefinedExamples.{ PositiveInt => PosInt }
  import eu.timepit.refined.auto._
  val rww: PosInt = i"47 618"

}

/**
  * Using [PureConfig](https://github.com/pureconfig/pureconfig) and friends.
  * TODO Note that the conf case classes can't be value classes... file issue?
  * not much of an issue as long as the use case remains squarely a _config_ use case

  * TODO
- enmesh Enumeratum (or equiv) with Refined and Spire types in a case class hierarchy
- create HOCON serialization for an instance thereof
- bake-off PureConfig against CaseClassy against ???
  */
object PureConfigExample {

  import java.{ time => jt }

  import spire.syntax.{ literals => sslits }
  import sslits.radix._ // imports the implicit
  import sslits.si._    //

  import com.typesafe.config.ConfigFactory
  import ConfigFactory.parseString

  import pureconfig.configurable.localDateConfigConvert
  import pureconfig.error.ConfigReaderFailures
  import pureconfig.loadConfig

  import pureconfig.module.enumeratum._
  import EnumeratumExamples._

  import eu.timepit.refined.auto._
  import eu.timepit.refined.pureconfig._
  import RefinedExamples._

  implicit val ldcc  = localDateConfigConvert(jt.format.DateTimeFormatter.ISO_DATE)
  implicit val crfEq = Eq.fromUniversalEquals[ConfigReaderFailures]
  // look ma no negs...
  val nn: NonNegativeInt = x16"2BADD00D"
  // val oo: Int Refined NonNegative = -42

  val confDate   = parseString(s"""{ date: 2011-12-03 }""")
  val configDate = loadConfig[ConfDate](confDate)

  configDate === ConfDate(LocalDate("2011-12-03")).asRight |> assertOrElse(configDate.toString)

  val root = "my.random.example"
  val config = ConfigFactory.parseString(
    s"""
      |$root {
      |  name = "My App"
      |  api-key = RacrqvWjuu4KVmnTG9b6xyZMTP7jnXy3 // intentionally out of order
      |  port = 1033
      |  schedule {
      |    initial-delay-seconds = 10
      |    interval-minutes = 120
      |    start-date = { date: 1979-07-04 }
      |    greeting = Aloha
      |  }
      |}
    """.stripMargin
  )

  // defined class Settings
  val cfg = loadConfig[Settings](config, root)

  val settings = Settings(
    "My App",
    1033,
    // "RacrqvWjuu4KVmnTG9b6xyZMTP7jnXyω",  // see what I did there? Check it.
    "RacrqvWjuu4KVmnTG9b6xyZMTP7jnXy3",
    ScheduleSettings(i"10", i"120", ConfDate(LocalDate("1979-07-04")), greeting = Greeting.Aloha)
  )

}

sealed trait Suit {
  def symbol: Char
  final override def toString: String = s"$symbol "
}

object Suit {
  val labels = 0 to 3 map (i => ('♠' + i).toChar)
}

object Spade   extends Suit { def symbol: Char = '♠' }
object Heart   extends Suit { def symbol: Char = '♡' }
object Diamond extends Suit { def symbol: Char = '♢' }
object Club    extends Suit { def symbol: Char = '♣' }

case class Card(value: Int, suit: Suit)

object DanceWithMeCirce {

  val card = Card(3, Club)

}

object SyntaxHighlightKiller {
  import eu.timepit.refined
  import refined.api.Refined
  import refined.string.MatchesRegex
  import refined.W
  type ApiKey = String Refined MatchesRegex[W.`"[a-zA-Z0-9]{25,40}"`.T]
}
