/*
 * Copyright 2017 Fairfax Technologies LLC
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

package wut

import eu.timepit.refined
import refined.api.Refined
import refined.collection._
import refined.numeric._

import spire.syntax.{ literals => sslits }
import sslits.literals // the implicit, explicitly. (sic)
import sslits.si._    // .us and .eu also available
import sslits.radix._ // imports the implicit

import enumeratum._
import enumeratum.values._

import cats.Eq
import cats.syntax.eq._
import cats.syntax.either._

import com.typesafe.config.{ Config, ConfigFactory }
import ConfigFactory.parseString

import pureconfig.configurable.localDateConfigConvert
import pureconfig.error.ConfigReaderFailures
import pureconfig.loadConfig

import eu.timepit.refined.pureconfig._

import classy.generic._
import classy.config._

// effectively, these are application classes... ;)
import java.time.LocalDate
import java.time.format.DateTimeFormatter

/**
  * Using [PureConfig](https://github.com/pureconfig/pureconfig) and friends.
  */
case class Conf(date: LocalDate)

object ConfStuff {
  implicit val localDateInstance = localDateConfigConvert(DateTimeFormatter.ISO_DATE)

  val conf   = parseString(s"""{ date: "2011-12-03" }""")
  val config = loadConfig[Conf](conf)

  implicit val confEq = Eq.fromUniversalEquals[Conf]
  implicit val failEq = Eq.fromUniversalEquals[ConfigReaderFailures]
  implicit val libEq  = Eq.fromUniversalEquals[LibraryItem]

  config === Conf(LocalDate.parse("2011-12-03")).asRight |> assert
  (LibraryItem withValue 1) === LibraryItem.Book |> assert

}

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

}

sealed abstract class LibraryItem(val value: Int, val name: String) extends IntEnumEntry

case object LibraryItem extends IntEnum[LibraryItem] {

  case object Book extends LibraryItem(value = 1, name = "book")

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements")) // wut? why?
  case object Movie extends LibraryItem(name = "movie", value = 2)

  case object Magazine extends LibraryItem(3, "magazine")

  case object CD extends LibraryItem(4, name = "cd")
  // case object Newspaper extends LibraryItem(4, name = "newspaper") <-- will fail to compile because the value 4 is shared

  /*
  val five = 5
  case object Article extends LibraryItem(five, name = "article") <-- will fail to compile because the value is not a literal
   */

  val values = findValues

}

object Spier {

  import cats.instances.int._

  val config = ConfigFactory.parseString(
    """
      |se.vlovgr.example {
      |  name = "My App"
      |  schedule {
      |    initial-delay-seconds = 10
      |    interval-minutes = 120
      |  }
      |}
    """.stripMargin
  )

  case class ScheduleSettings(
      initialDelaySeconds: Int Refined NonNegative,
      intervalMinutes: Int Refined Positive
  )
  // defined class ScheduleSettings

  case class Settings(
      name: String Refined NonEmpty,
      schedule: ScheduleSettings
  )
  // defined class Settings
  val cfg = loadConfig[Settings](config, "se.vlovgr.example")

  // bytes and shorts
  val x    = b"100" // without type annotation!
  val y    = h"999"
  val mask = b"255" // unsigned constant converted to signed (-1)

  // rationals
  val n1 = r"1/3"
  val n2 = r"1599/115866" // simplified at compile-time to 13/942

  val a = x2"10111" // binary
  val b = x8"27"    // octal
  val c = x16"17"   // hex
  Seq(a, b, c).forall((_: Int) === 23) |> assert

  // SI notation for large numbers

  val ww = i"1 944 234 123"                                 // Int
  val xx = j"89 234 614 123 234 772"                        // Long
  val yy = big"123 234 435 456 567 678 234 123 112 234 345" // BigInt
  val zz = dec"1 234 456 789.123456789098765"               // BigDecimal
}

object ClassyStuff {

  // Our configuration class hierarchy
  sealed trait Shape
  case class Circle(radius: Double)                                           extends Shape
  case class Rectangle(length: Double, width: Double /* Refined Positive */ ) extends Shape

  case class MyConfig(someString: Option[String], shapes: List[Shape])

  val decoder1 = deriveDecoder[Config, MyConfig]
  val shapes   = decoder1 fromString """shapes = []"""
  // res4: Either[classy.DecodeError,MyConfig] = Right(MyConfig(None,List()))

  val cfg = decoder1 fromString """
    someString = "hello"
    shapes     = []"""
  // res5: Either[classy.DecodeError,MyConfig] = Right(MyConfig(Some(hello),List()))

  val moarShapes = decoder1 fromString """shapes = [
    { circle    { radius: 200.0 } },
    { rectangle { length: 10.0, width: 20.0 } }
  ]"""
  // res6: Either[classy.DecodeError,MyConfig] = Right(MyConfig(None,List(Circle(200.0), Rectangle(10.0,20.0))))

  // mismatched config
  val badCfg = decoder1 fromString """shapes = [
    { rectangle { radius: 200.0 } },
    { circle    { length: 10.0, width: -20.0 } }
  ]"""
  // res: Either[classy.DecodeError,MyConfig] = Left(AtPath(shapes,And(AtIndex(0,Or(AtPath(circle,Missing),List(AtPath(rectangle,And(AtPath(length,Missing),List(AtPath(width,Missing))))))),List(AtIndex(1,Or(AtPath(circle,AtPath(radius,Missing)),List(AtPath(rectangle,Missing))))))))

  // error pretty printing
  val bad = badCfg fold (
    error => error.toPrettyString,
    conf => s"success: $conf"
  )
}
