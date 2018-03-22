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
class TimeSpec extends FlatSpec with Matchers {

  import io.deftrade.time._
  import java.time.Month
  import java.time.temporal.ChronoUnit, ChronoUnit.{ HOURS => Hours, MINUTES => Minutes }

  "j8 date/time factory methods" should "work" in {

    // java.time.Duration
    assert(minutes(20) - minutes(10) == minutes(10))
    assert(seconds(10) + seconds(10) == seconds(20))
    assert(duration(Minutes)(20) / 5 == duration(ChronoUnit.MINUTES)(4))
    assert(duration(Hours)(10) * 5 == duration(ChronoUnit.HOURS)(50))

    // java.time.Period
    assert(days(1) + days(1) == days(2))
    assert(months(2) - months(1) == months(1))

    // java.time.LocalTime
    assert(localTime(20, 30, 0) + minutes(5) == localTime(20, 35, 0))
    assert(localTime(20, 30, 0) - minutes(5) == localTime(20, 25, 0))

    // java.time.LocalDate
    assert(localDate(2015, Month.JANUARY, 1) + months(2) == localDate(2015, Month.MARCH, 1))
    assert(localDate(2015, Month.MARCH, 1) - months(2) == localDate(2015, Month.JANUARY, 1))
  }

  it should "work some more" in {
    import io.deftrade.wip.TimeExample._
    import scala.concurrent.{ duration => scd }
    assert(1.year + 1.day === period(years = 1, days = 1, months = 0))
    assert(t2fd === scd.Duration(t2fd.toString))

    import io.deftrade.time.work._
    assert(yesterday < today)

  }

  "domain Order stuff" should "work" in {
    // import cats.{ Eq, Monoid }
    import cats.instances.all._
    import cats.syntax.monoid._
    import model.Api._

    val o1 = Order.legacy(555.550001, 78345)
    val o2 = Order.legacy(168.020660, 186283)

    val o12 = o1 |+| o2

    assert(o12 === Order.legacy(BigDecimal(723.570661), 264628))

    val m1   = Map(1337 -> o1)
    val m1_a = Map(1337 -> o2)
    val m2   = Map(4958 -> Order.legacy(666.880033, 123456))
    val mmm  = m1 |+| m1_a |+| m2
    assert(
      mmm === Map(
        4958 -> Order.legacy(666.880033, 123456),
        1337 -> Order.legacy(723.570661, 264628)
      )
    )

  }

  "typeclass examples" should "work" in {
    import cats.{ Monoid }
    import cats.instances.all._
    import cats.syntax.{ monoid, option }, option._, monoid._

    import Kats._

    import Printable._

    import scala.Predef.{ assert => affirm }

    /*
     * Exercises 1.2.5:  Cat Show
     */
    //
    // another lame comment

    123 === 123 |> affirm

    assert(1.some !== None)

    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("b" -> 3, "d" -> 4)
    val mm   = map1 |+| map2
    mm === Map("a" -> 1, "b" -> (2 + 3), "d" -> 4) |> affirm
    Monoid[String].combine("foo", "bar") === "foobar" |> affirm

    // def sum[A: Monoid](xs: List[A]): A = xs.foldLeft(Monoid[A].empty)(_ |+| _)
    //
    // implicit val kittehShow = Show show [Kitteh] (_.format)
    // implicit val kittehEq   = Eq.fromUniversalEquals[Kitteh]
    // Note you get tuples for free.
    assert((maru === ara, maru !== ara) === ((false, true)))

    import Printable.format

    // Printable print "hello"
    format("hello") === "\"hello\""                                      |> affirm
    format(true) === "yes"                                               |> affirm
    format(Box(maru)) === "\"Box(Kitteh(Maru,9,Scottish Fold,Kibble))\"" |> affirm

    import Codec.{ decode, encode }
    encode(Box(123)) === "123"                |> affirm
    decode[Box[Int]]("618") === Box(618).some |> affirm
  }

}
