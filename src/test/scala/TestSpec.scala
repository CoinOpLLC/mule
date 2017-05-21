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

/**
* Uses ideas from (this post)[https://medium.com/@shanielh/5-tips-for-scalatest-that-will-make-testing-great-again-58190df1df88].
*/
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{FlatSpec, Matchers}

object Calculator {
  def add(a: Int, b: Int): Long = a.toLong + b
}

class CalculatorSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {
  case class Case(a: Int, b: Int, expected: Long)

  private val cases = Seq(Case(1, 1, 2),
    Case(Int.MaxValue, Int.MaxValue, Int.MaxValue.toLong * 2))

  for (Case(a, b, expected) <- cases) {
    "add" should s"return $expected for $a and $b" in {
      val target = Calculator.add(a, b)
      target shouldEqual expected
    }
  }

  "add" should "return 5 for 3 and 2" in {
    val target = Calculator.add(3, 2)
    target should === (5L)
  }

}
