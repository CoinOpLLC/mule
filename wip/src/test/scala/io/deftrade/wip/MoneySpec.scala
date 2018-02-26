package io.deftrade
package wip

import scala.language.higherKinds
import cats.syntax.show._

import org.scalatest.{ prop, FlatSpec, PropSpec }, prop.GeneratorDrivenPropertyChecks

import ClassPerCurrency.Moneta, Moneta._
// import PhantomTypePerCurrency._, Moneta._

class MoneySpec extends FlatSpec {

  "Money" should "be created elastically" in {

    import cats.syntax.order.{ catsSyntaxOrder, catsSyntaxPartialOrder }

    val eur = EUR // Moneta withName "EUR"
    // val eurF  = (d: Double) => eur(d)
    val eurF  = eur[Double] _
    val eur20 = 20.0 |> eurF
    val e20   = EUR(20.00)

    assert(eur20 === e20)

    assert(eur20 * 2 > e20)
    assert(eur20 + eur20 > e20)

    type Dollar = USD[Double]

    val d20: Dollar = USD(20.00)
    val d21: Dollar = USD(21.00)

    assert(d20 !== d21)
    assert(d20 < d21)
    assert((d20 max d21) === d21)

    // def funge[C <: Currency](den: Moneta[C]): Money[Double, C] = den(19.47)
    def funge[C[?] <: Currency[?]](den: Moneta[C]): C[Double] = den(19.47)

    assert(USD(19.47) === funge[USD](USD))

    val usd               = USD
    val buck: USD[Double] = usd(1.0)
    assert(buck === USD(1.0))
    assert(buck + buck === USD(2.0))

    assert(buck.show === "USD  1.00 ")

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
