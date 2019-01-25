package io.deftrade
package money

import org.scalatest.{ prop, FlatSpec, PropSpec }, prop.GeneratorDrivenPropertyChecks

class MoneySpec extends FlatSpec {

  import cats.syntax.show._
  import cats.syntax.order.{ catsSyntaxOrder, catsSyntaxPartialOrder }

  import Monetary._

  "Money" should "be created elastically" in {

    val eur   = EUR
    val eurF  = eur[Double] _
    val eur20 = 20.0 |> eurF
    val e20   = EUR(20.00)

    val F = Financial[Double]
    import F._
    cats.kernel.Order[Money[Double, USD]]

    assert(eur20 === e20)

    assert(eur20 * 2.0 > e20)
    assert(eur20 + eur20 > e20)

    type Dollar = Money[Double, USD] // phantom type per `Currency`

    val d20: Dollar = USD(20.00)
    val d21: Dollar = USD(21.00)

    assert(d20 !== d21)
    assert(d20 < d21)
    assert((d20 max d21) === d21)

    def funge[C <: Currency](den: Monetary[C]): Money[Double, C] = den(19.47)
    // def funge[C[?] <: Currency[?]](den: Moneta[C]): C[Double] = den(19.47)

    assert(USD(19.47) === funge(USD))

    val usd          = USD
    val buck: Dollar = usd(1.0)
    assert(buck === USD(1.0))
    assert(buck + buck === USD(2.0))

    assert(buck.show === "USD  1.00 ")
    assert((-buck).show === "USD (1.00)")

    implicit def eurusdStaticPrice: EUR QuotedIn USD = QuotedIn.Spread(1.23, 1.22)

    lazy val eurusd = EUR / USD

    val dollarsRequired: Dollar = eurusd buy EUR(100.0)
    val dollarsReceived: Dollar = eurusd sell EUR(100.0)

    assert(dollarsReceived < dollarsRequired)

  }
}
class MoneyPropSpec extends PropSpec with GeneratorDrivenPropertyChecks {
// with TableDrivenPropertyChecks {
  property("unit is as unit does") {
    forAll { ewie: Unit =>
      assert(ewie === (()))
    }
  }
  property("doubles gonna double") {
    forAll { xs: List[Double] =>
      whenever(xs.nonEmpty) {
        import scala.language.postfixOps
        assert(math.sqrt(xs map (x => x * x) sum) >= 0)
      }
    }
  }
}
