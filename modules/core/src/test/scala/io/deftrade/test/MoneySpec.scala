package io.deftrade
package money

import implicits._, model._
import Currency.{ EUR, USD }

import cats.syntax.show._
import cats.syntax.order.{ catsSyntaxOrder, catsSyntaxPartialOrder }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class MoneySpec extends AnyFlatSpec {

  type Dollars = Money[USD]
  type Euros   = Money[EUR]

  "Mny" should "be created via fiat `Currency`s" in {

    val eur                   = EUR
    val eurF                  = eur[Double] _
    val eur20                 = 20.0 |> eurF
    val e20: Mny[Double, EUR] = EUR(20.00)

    assert(eur20 === e20)

    assert(eur20 * 2.0 > e20)
    assert(eur20 + eur20 > e20)
  }

  "Money" should "be usable from `model`" in {

    val buck: Dollars = USD(1.0)

    assert(buck === USD(1.0))
    assert(buck + buck === USD(2.0))

    assert(buck.show === "USD  1.00 ")
    assert((-buck).show === "USD (1.00)")

    val d20: Dollars = USD(20.00)
    val d21: Dollars = USD(21.00)

    assert(d20 !== d21)
    assert(d20 < d21)
    assert((d20 max d21) === d21)
  }

  "Money" should "be exchangeable with entropy" in {

    implicit def eurusdStaticPrice: EUR QuotedIn USD = QuotedIn(bid = 1.12, ask = 1.13)

    lazy val eurusd: Rate[EUR, USD] = EUR / USD

    val euro5C: Euros = EUR(500.0)

    val dollarsPaid     = eurusd buy euro5C
    val dollarsReceived = eurusd sell euro5C

    assert(dollarsReceived < dollarsPaid)
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
