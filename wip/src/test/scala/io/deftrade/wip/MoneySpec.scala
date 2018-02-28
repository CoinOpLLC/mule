package io.deftrade
package wip

import org.scalatest.{ prop, FlatSpec, PropSpec }, prop.GeneratorDrivenPropertyChecks

// import ClassPerCurrency.Moneta
import PhantomTypePerCurrency.Monetary

class MoneySpec extends FlatSpec {

  import cats.syntax.show._
  import cats.syntax.order.{ catsSyntaxOrder, catsSyntaxPartialOrder }

  import Monetary._

  "Money" should "be created elastically" in {

    val eur = EUR // Moneta withName "EUR"
    // val eurF  = (d: Double) => eur(d)
    val eurF  = eur[Double] _
    val eur20 = 20.0 |> eurF
    val e20   = EUR(20.00)

    assert(eur20 === e20)

    assert(eur20 * 2 > e20)
    assert(eur20 + eur20 > e20)

    // import scala.language.higherKinds
    // type Dollar = USD[Double] // ClassPerCurrency
    type Dollar = Money[Double, USD] // PhantomTypePerCurrency

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

    implicit def eurusdStaticPrice: Pricing[BigDecimal, EUR, USD] = SimplePricing(1.23, 1.22)

    lazy val eurusd: Cross[EUR, USD, BigDecimal] = EUR / USD

    val buxRequired = eurusd buy eur(100.0)
    val buxReceived = eurusd sell eur(100.0)

    assert(buxReceived < buxRequired)

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
