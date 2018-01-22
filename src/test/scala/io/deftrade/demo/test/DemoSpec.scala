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
}
