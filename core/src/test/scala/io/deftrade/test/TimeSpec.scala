package io.deftrade
package test

import org.scalatest._, prop._

class TimeFlatSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import io.deftrade.time._

  "time" should "move forward" in {
    val today = localDateTime
    val later = 33.hours + 33.minutes + 33.seconds + 33.millis + 33.nanos
    (today + later) should be > today
  }

  it should "throw an exception if a bad date is parsed" in {
    a[DateTimeParseException] should be thrownBy {
      "144 Doritos" |> instant
    }
  }

  "The integers" should "generally behave as advertized" in {
    forAll { (n: Int) =>
      whenever(n > 1) { n / 2 should be > 0 }
    }
  }
}

class TimePropSpec extends PropSpec with GeneratorDrivenPropertyChecks {
// with TableDrivenPropertyChecks {
  // import org.scalacheck.Gen._
  // import java.time._
  // property("same as it ever was") {
  //   forAll { (ldt: LocalDateTime) =>
  //     whenever(true) {
  //       assert(ldt === ldt)
  //     }
  //   }
  // }
}

class CamelCasePropSpec extends PropSpec with GeneratorDrivenPropertyChecks {

  // Our Gold standard (for testing): yet another take on an old fav:
  // https://github.com/lift/framework/search?utf8=%E2%9C%93&q=%22def+snakify%22

  // splits off strings of capital letters leaving one...
  val rx1 = """([A-Z]+)([A-Z][a-z])""".r

  // splits transition from lower -> upper case
  val rx2 = """([a-z\d])([A-Z])""".r

  def delimit(rx: scala.util.matching.Regex)(s: String): String = rx replaceAllIn (s, "$1•$2")

  def goldCamelTo(sep: String)(name: String): String =
    (name |> delimit(rx1) |> delimit(rx2)) split "•" mkString sep

  property("CamelCase: verify the gold standard") {
    forAll { s: String =>
      whenever(true) {
        assert(goldCamelTo("")(s) === s)
      }
    }
  }
  property("CamelCase: test impl against gold standard") {
    forAll { s: String =>
      whenever(true) {
        assert(CamelTo("")(s) === s)
        assert(CamelTo("•")(s) === goldCamelTo("•")(s))
      }
    }
  }
}
