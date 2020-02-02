package io.deftrade
package money

import implicits._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MoneySpec extends AnyFlatSpec {

  import cats.syntax.show._
  import cats.syntax.order.{ catsSyntaxOrder, catsSyntaxPartialOrder }

  import Currency.{ EUR, USD }

  "Money" should "be created elastically" in {

    val eur   = EUR
    val eurF  = eur[Double] _
    val eur20 = 20.0 |> eurF
    val e20   = EUR(20.00)

    assert(eur20 === e20)

    assert(eur20 * 2.0 > e20)
    assert(eur20 + eur20 > e20)

    type Dollar = model.Mny[USD] // phantom type per `Currency`

    val d20: Dollar = USD(20.00)
    val d21: Dollar = USD(21.00)

    assert(d20 !== d21)
    assert(d20 < d21)
    assert((d20 max d21) === d21)

    def funge[C](den: Currency[C]): Money[Double, C] = den(19.47)

    assert(USD(19.47) === funge(USD))

    val usd          = USD
    val buck: Dollar = usd(1.0)
    assert(buck === USD(1.0))
    assert(buck + buck === USD(2.0))

    assert(buck.show === "USD  1.00 ")
    assert((-buck).show === "USD (1.00)")

    import model._

    implicit def eurusdStaticPrice: EUR QuotedIn USD = QuotedIn(1.12, 1.13)

    lazy val eurusd: Rate[EUR, USD] = EUR / USD

    val dollarsRequired: Dollar = eurusd buy EUR(100.0)
    val dollarsReceived: Dollar = eurusd sell EUR(100.0)

    assert(dollarsReceived < dollarsRequired)
  }
}

class MoneyPropSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks {
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
