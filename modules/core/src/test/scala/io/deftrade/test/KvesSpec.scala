package io.deftrade
package test

import money._, model.{ Contact, Meta, Metas, Money }
import Currency.{ USD }
import currencies._

import cats.effect.IO

// import eu.timepit.refined
// import refined.scalacheck.any._

import fs2.{ Stream }

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

// import io.circe.Json

object arbitraryMvt {

  // import Jt8Gen._
  import mvt._

  def drift[A](aa: Gen[A]): Gen[Stream[IO, A]] =
    for (a <- aa) yield Stream eval (IO delay a)

  implicit def FIXME_0: Arbitrary[Meta]    = ???
  implicit def FIXME_1: Arbitrary[Contact] = ???
  implicit def FIXME_2: Arbitrary[Cost]    = ???

  def metas: Metas.ValueStore[IO]    = ???
  def costs: Costs.KeyValueStore[IO] = ???

  implicit def arbitraryProduct: Arbitrary[Stream[IO, Product]] =
    Arbitrary {
      for {
        bs       <- drift(arbitrary[Cost])
        nuts     <- drift(arbitrary[Nut])
        contacts <- drift(arbitrary[Contact])
        ms       <- drift(arbitrary[Meta])
      } yield Product.mkPipe(metas, costs)(nuts, bs, contacts, ms)
    }

  implicit def arbitraryCost: Arbitrary[IO[Cost]] =
    Arbitrary {
      for {
        amount <- arbitrary[Money[USD]]
        meta   <- arbitrary[Meta]
      } yield Cost.mk(metas)(amount, meta)
    }
}
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
