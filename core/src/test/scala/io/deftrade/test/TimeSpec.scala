package io.deftrade
package test

import org.scalatest._, prop._

class TimeFlatSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  import io.deftrade.time._

  "time" should "move forward" in {
    val today = localDateTime
    val later = 3.hours + 33.minutes + 33.seconds + 333.nanos
    (today + later) should be > today
  }

  it should "throw an exception if a bad date is parsed" in {
    a[Throwable] should be thrownBy {
      throw new Throwable {}
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
