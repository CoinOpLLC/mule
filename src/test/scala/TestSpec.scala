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

package wut

/**
  * Uses ideas from (this post)[https://medium.com/@shanielh/5-tips-for-scalatest-that-will-make-testing-great-again-58190df1df88].
  */
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{ FlatSpec, Matchers }

object Calculator {
  def add(a: Int, b: Int): Long = a.toLong + b
}

class CalculatorSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {
  case class Case(a: Int, b: Int, expected: Long)

  private val cases = Seq(Case(1, 1, 2), Case(Int.MaxValue, Int.MaxValue, Int.MaxValue.toLong * 2))

  for (Case(a, b, expected) <- cases) {
    "add" should s"return $expected for $a and $b" in {
      val target = Calculator.add(a, b)
      target shouldEqual expected
    }
  }

  "add" should "return 5 for 3 and 2" in {
    val target = Calculator.add(3, 2)
    target should ===(5L)
  }

}

class MonadTransformerExampleSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {
  import MonadTransformerStuff._
  case class TacticalSituation(ally1: String, ally2: String, response: String)
  "tacticalReport" should "basically work and whatnot" in {
    tacticalReport("Hot Rod", "Bumblebee") should ===("Hot Rod and Bumblebee are ready to rock!")
    tacticalReport("Jazz", "Bumblebee") should ===("Jazz and Bumblebee need refractory respite!")
    tacticalReport("Hot Rod", "Bogodork") should ===("WTF: Bogodork power level unknown.")
  }

}

class TraverseSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {
  import Chapter7._
  "sequence" should "expand cartesian product in collections" in {
    import cats.instances.vector._
    // NO: why? remember _Cartesian_...
    //  listSequence(List(Vector(1, 2), Vector(3, 4))) should ===(Vector(List(1, 2), List(3, 4)))
    listSequence(List(Vector(1, 2), Vector(3, 4))) should ===(Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))

    listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) should ===(
      Vector(
        List(1, 3, 5),
        List(1, 3, 6),
        List(1, 4, 5),
        List(1, 4, 6),
        List(2, 3, 5),
        List(2, 3, 6),
        List(2, 4, 5),
        List(2, 4, 6)
      )
    )
  }
  "options" should "fail fast and fail hard" in {
    process(List(2, 4, 6)) should ===(Some(List(2, 4, 6)))
    process(List(2, 4, 6, 7)) should ===(None)
  }

  "validation" should "accumlate all the error things" in {

    import cats.data.Validated
    import cats.instances.list._ // Applicative[ErrorsOr] needs a Monoid[List]
    import cats.syntax.validated._

    vprocess(List(2, 4, 6)) should ===(List(2, 4, 6).valid)
    vprocess(List(1, 2, 3)) should ===(List("1 is not even", "3 is not even").invalid)
  }
}
/**
  * TODO: proper tests for the Writer Monad stuff
  */
