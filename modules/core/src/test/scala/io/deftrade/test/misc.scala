package io.deftrade
package test

import money._, keyval._, model._, refinements._
import Currency.{ EUR, USD }

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semi }
import cats.effect.{ ContextShift, Sync }

import enumeratum._

import eu.timepit.refined
import refined.refineV
import refined.api.Refined
import refined.numeric._
import refined.cats._
import refined.auto._

import org.scalacheck._
// import org.scalacheck.cats.implicits._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

/**
  */
sealed trait Nut extends EnumEntry with Serializable

/**
  */
object Nut extends DtEnum[Nut] {

  case object Peanut     extends Nut
  case object Hazelnut   extends Nut
  case object Almond     extends Nut
  case object Cashew     extends Nut
  case object Walnut     extends Nut
  case object Pecan      extends Nut
  case object Pistaschio extends Nut
  case object Brazil     extends Nut

  lazy val values = findValues
}

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

object invoices {

  import time._, keyval._
  import CsvImplicits._

  import io.chrisdavenport.cormorant
  // import cormorant.generic.semiauto._
  import cormorant.refined._
  import cormorant.implicits._

  import OpaqueKey._

  import currencies._

  sealed abstract case class Invoice private (
      asOf: Instant,
      nut: Nut,
      quantity: Int Refined Positive,
      from: Parties.Key,
      to: Parties.Key,
      amount: Dollars,
      memo: Label
  )

  object Invoice {

    def apply(
        asOf: Instant,
        nut: Nut,
        quantity: Int Refined Positive,
        from: Parties.Key,
        to: Parties.Key,
        amount: Dollars,
        memo: Label
    ): Invoice =
      new Invoice(asOf = instant, nut, quantity, from, to, amount, memo) {}

    def mk(
        nut: String,
        jars: Int,
        from: Parties.Key,
        to: Parties.Key,
        total: Double,
        instructions: String = ""
    ): Result[Invoice] = Result safe {

      val Right(quantity) = refineV[Positive](jars min 1)
      val Right(memo)     = refineV[IsLabel](s"special instructions: $instructions")
      val amount          = dollars(total)

      Invoice(asOf = instant, Nut withName nut, quantity, from, to, amount, memo)
    }

    implicit lazy val invoiceEq: Eq[Invoice]     = { import auto.eq._; semi.eq }
    implicit lazy val invoiceShow: Show[Invoice] = { import auto.show._; semi.show }
  }

  object Invoices extends KeyValueStores.KV[Long Refined Invoice, Invoice]

  def invoices[F[_]: Sync: ContextShift]: Result[Invoices.KeyValueStore[F]] =
    keyValueStore[F] at "target/invoices.csv" ofKeyChained Invoices
}
