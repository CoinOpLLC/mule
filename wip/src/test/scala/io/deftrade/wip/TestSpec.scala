package io.deftrade
package wip

/**
  * Uses ideas from (this post)[https://medium.com/@shanielh/5-tips-for-scalatest-that-will-make-testing-great-again-58190df1df88].
  */
import org.scalatest.{ FlatSpec, Matchers }

class EnumeratumSpec extends FlatSpec with Matchers {
  "Enumeratum Greeting" should "work as advertised" in {
    import EnumeratumExamples.Greeting // look ma no import tax
    (Greeting withName "Hi") should ===(Greeting.Hi)
  }
  "Enumeratum LibraryItem" should "work as advertised" in {
    import EnumeratumExamples.LibraryItem // look ma no import tax
    (LibraryItem withValue 1) should ===(LibraryItem.Book)
    // (LibraryItem withName "book") should ===(LibraryItem.Book)
  }
}
class RunAndDoNothingSpec extends FlatSpec {
  "Main program" should "run and do nothing" in {
    Main.main(Array.empty[String]) |> discardValue
  }
}

class PureConfigSpec extends FlatSpec with Matchers {
  import PureConfigExample.{ cfg, settings }
  (cfg fold (_ => false, _ === settings)) shouldEqual true
}
object Calculator {
  def add(a: Int, b: Int): Long = a.toLong + b
}

class CalculatorSpec extends FlatSpec with Matchers {
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

class MonadTransformerExampleSpec extends FlatSpec with Matchers {
  import MonadTransformerStuff._
  case class TacticalSituation(ally1: String, ally2: String, response: String)
  "tacticalReport" should "basically work and whatnot" in {
    tacticalReport("Hot Rod", "Bumblebee") should ===("Hot Rod and Bumblebee are ready to rock!")
    tacticalReport("Jazz", "Bumblebee") should ===("Jazz and Bumblebee need refractory respite!")
    tacticalReport("Hot Rod", "Bogodork") should ===("WTF: Bogodork power level unknown.")
  }

}

class TraverseSpec extends FlatSpec with Matchers {
  import TraverseStuff._
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

    // import cats.data.Validated
    // import cats.instances.list._ // TODO I thought Applicative[ErrorsOr] needs a Monoid[List]
    import cats.syntax.validated._

    vprocess(List(2, 4, 6)) should ===(List(2, 4, 6).valid)
    vprocess(List(1, 2, 3)) should ===(List("1 can not even", "3 can not even").invalid)
  }

  // FIXME: test FormValidation
}
/**
  * TODO: proper tests for the Writer Monad stuff
  */
