package io.deftrade

import refinements._

import cats.implicits._
import eu.timepit.refined
import refined.refineV
import refined.auto._

import org.scalacheck._
// import org.scalacheck.cats.implicits._
// import org.scalacheck.ScalacheckShapeless._
// import Arbitrary.arbitrary

package object test {

  import deftrade.infiniteLazyList

    val isins = List(
      "USQ6188CAA47",
      "US0378331005",
      "US0373831005",
      "U50378331005",
      "AU0000XVGZA3",
      "AU0000VXGZA3",
      "FR0000988040"
    )
}

package test {
  
  object currencies {

    implicit def arbitraryMny[C: Currency]: Arbitrary[Money[C]] =
      Arbitrary {
        import Financial.Ops
        val fiat = Currency[C]

        for {
          amount <- arbitrary[Double]
        } yield fiat(amount.to[model.MonetaryAmount])
      }

    type Dollars = Money[USD]
    lazy val Dollars                                   = USD
    def dollars(amount: model.MonetaryAmount): Dollars = Dollars(amount)

    type Euros = Money[EUR]
    lazy val Euros                                 = EUR
    def euros(amount: model.MonetaryAmount): Euros = Euros(amount)
  }


  object Jt8Gen {

    import time._

    def durationGen: Gen[Duration]                           = ???
    def finiteDurationGen(range: Duration): Gen[Duration]    = ???
    def localDateTimeInPeriod(p: Period): Gen[LocalDateTime] = ???

    lazy val start                    = java.time.Instant.EPOCH
    lazy val end                      = ((start atZone ZoneIdZ) + 100.years).toInstant
    lazy val oneHundredYearsOfSeconds = end.toEpochMilli / 1000

    implicit def arbitraryInstantE10: Arbitrary[Instant] =
      Arbitrary {
        for {
          secs <- Gen.chooseNum(0L, oneHundredYearsOfSeconds)
        } yield java.time.Instant.ofEpochSecond(secs)
      }
  }
}
