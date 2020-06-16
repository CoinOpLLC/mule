package demo
package test

import org.scalatest._

/**
  * Unit tests for the integration demo.
  */
class DemoSpec extends FlatSpec with Matchers {

  "Demo" should "run to completion" in {
    Demo main Array.empty[String]
  }

  "io.deftrade.time examples" should "work, sorta" in {
    import io.deftrade.time._
    import scala.concurrent.{ duration => scd }
    import demo.TimeExample._
    assert(1.year + 1.day === period(years = 1, days = 1, months = 0))
    assert(t2fd === scd.Duration(t2fd.toString))

    import io.deftrade.time.work._
    assert(yesterday < today)
  }
}
