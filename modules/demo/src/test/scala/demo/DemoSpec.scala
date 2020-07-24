package demo
package test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Unit tests for the integration demo.
  */
class DemoSpec extends AnyFlatSpec with Matchers {

  // "Demo" should "run to completion" in {
  //   Demo main Array.empty[String]
  // }

  import io.deftrade.time._
  import io.deftrade.time.work._

  import scala.concurrent.{ duration => scd }

  import demo.TimeExample._

  "io.deftrade.time examples" should "work, sorta" in {
    assert(1.year + 1.day === period(years = 1, days = 1, months = 0))
    // assert(t2fd === scd.Duration(t2fd.toString))

    import cats.implicits._
    assert(yesterday < today)
  }
}
