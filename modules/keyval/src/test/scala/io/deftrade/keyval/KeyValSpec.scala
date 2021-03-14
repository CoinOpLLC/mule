package io.deftrade
package test

import keyval._

import cats.effect.IO

// import eu.timepit.refined
// import refined.scalacheck.any._

import fs2.{ Stream }

import io.chrisdavenport.fuuid
import fuuid.{ FUUID, FUUIDGen }

import io.chrisdavenport.cats.time._

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

// import io.circe.Json

/**
  */
object arbitraryMvt {

  import model._
  import mk._

  // import csv.implicits._
  // import io.chrisdavenport.cormorant
  // import cormorant.generic.semiauto._
  // import cormorant.refined._
  // import cormorant.implicits._

  lazy val Right(costsKVS)   = costs[IO]
  lazy val Right(contactsVS) = contacts[IO]

  def drift[A](aa: Gen[A]): Gen[Stream[IO, A]] =
    for (a <- aa) yield Stream eval (IO delay a)

  implicit def FIXME_Contact: Arbitrary[Contact] = ???
  implicit def FIXME_Cost: Arbitrary[Cost]       = ???
  implicit def FIXME_Dollars: Arbitrary[Dollars] = ???
  implicit def FIXME_FUUID: Arbitrary[FUUID]     = ???

  implicit def arbitraryProductStream: Arbitrary[Stream[IO, Product]] =
    Arbitrary {
      for {
        cs      <- drift(arbitrary[Cost])
        nuts    <- drift(arbitrary[Nut])
        contact <- drift(arbitrary[Contact])
      } yield Product.mkPipe(contactsVS, costsKVS)(nuts, cs, contact)
    }

  implicit def arbitraryCost: Arbitrary[IO[Costs.Id]] =
    Arbitrary {
      for {
        // amount <- arbitrary[Money[USD]]
        fuuid  <- arbitrary[FUUID]
        amount <- arbitrary[Dollars]
      } yield Cost.mk(costsKVS)(fuuid, amount)
    }
}

/**
  */
class KvesPropSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks {
  // import mvt._
  // import arbitraryMvt.arbitraryBar

  property("some property") {

    //   forAll { cost: Cost =>
    //     println(cost)
    //   }
    //
    //   forAll { cost: SADT =>
    //     val key = SADT.Key unsafe cost.hashCode.toLong
    //     val id  = costs upsert (key, cost)
    //     println(id -> (key -> cost))
    //   }
    //
    //   forAll { products: Product =>
    //     println(products)
    //   }
  }
}
