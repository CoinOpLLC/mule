package io.deftrade
package test

import time._, money._, keyval._, model._, refinements._
import Currency.{ EUR, USD }
import IsUnitInterval._

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semiauto }
import cats.effect.{ ContextShift, IO, Sync }

import enumeratum._

import eu.timepit.refined
import refined.refineV
import refined.api.Refined
import refined.numeric._
import refined.cats._
import refined.auto._

import io.chrisdavenport.cormorant

import io.chrisdavenport.fuuid
import fuuid.{ FUUID, FUUIDGen }

import fs2.{ Stream }

import org.scalacheck._
// import org.scalacheck.cats.implicits._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

import scala.concurrent.ExecutionContext.Implicits.global

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

    implicit lazy val invoiceEq: Eq[Invoice]     = { import auto.eq._; semiauto.eq }
    implicit lazy val invoiceShow: Show[Invoice] = { import auto.show._; semiauto.show }
  }

  object Invoices extends KeyValueStores.KV[Long OpaqueKey Invoice, Invoice]

  def invoices[F[_]: Sync: ContextShift]: Result[Invoices.KeyValueStore[F]] = {
    import CsvImplicits._
    import io.chrisdavenport.cormorant
    import cormorant.generic.semiauto._
    import cormorant.refined._
    import cormorant.implicits._
    keyValueStore[F] at "target/invoices.csv" ofKeyChained Invoices
  }
}

/**
  */
object mvt {

  import currencies._

  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  /**
    */
  sealed abstract case class Product private (
      nut: Nut,
      cost: Costs.Key,
      name: Label,
      factor: Double Refined `[0,1)`,
      owner: Contacts.Id
  )

  /**
    */
  object Product {

    /**
      */
    def apply(
        nut: Nut,
        cost: Costs.Key,
        name: Label,
        factor: Double Refined `[0,1)`,
        owner: Contacts.Id
    ): Product =
      new Product(nut, cost, name, factor, owner) {}

    /**
      */
    def mk(
        metas: Metas.ValueStore[IO],
        costs: Costs.KeyValueStore[IO]
    )(
        nut: Nut,
        cost: Cost,
        owner: Contact,
        meta: Meta
    ): IO[Product] =
      for {
        key <- FUUIDGen[IO].random
        ret <- metas put meta
        _   <- costs.put(key, cost)
      } yield {
        val Right(factor) =
          refineV[`[0,1)`](cost.show.size / (cost.show.size + meta.show.size).toDouble)
        val Right(name) = refineV[IsLabel](s"${nut.show}::${key.show}:${ret._1.show}")
        Product(nut, key, name, factor, ret._1)
      }

    /** FIXME factor this shyte with applicative and zip and whatnot */
    def mkPipe(
        metas: Metas.ValueStore[IO],
        costs: Costs.KeyValueStore[IO]
    )(
        nuts: Stream[IO, Nut],
        bs: Stream[IO, Cost],
        contacts: Stream[IO, Contact],
        ms: Stream[IO, Meta]
    ): Stream[IO, Product] =
      (
        for {
          n <- nuts
          b <- bs
          c <- contacts
          m <- ms
        } yield (n, b, c, m)
      ) evalMap (mk(metas, costs) _).tupled

    implicit def productEq: Eq[Product]     = { import auto.eq._; semiauto.eq }
    implicit def productShow: Show[Product] = { import auto.show._; semiauto.show }
  }

  object Products extends KeyValueStores.KV[Label, Product]
  import CsvImplicits._
  import cormorant.generic.semiauto._
  import cormorant.refined._
  import cormorant.implicits._

  def products[F[_]: Sync: ContextShift]: Result[Products.KeyValueStore[F]] =
    keyValueStore[F] at "target/products.csv" ofKeyChained Products

  object Foos extends ValueStores.VS[Product]

  def foos[F[_]: Sync: ContextShift]: Result[Foos.ValueStore[F]] =
    valueStore[F] at "target/foos.csv" ofChained Foos

  /**
    */
  sealed abstract case class Cost private (
      asOf: Instant,
      amount: Dollars,
      mi: Metas.Id
  )

  /**
    */
  object Cost {

    /**
      */
    def apply(asOf: Instant, amount: Dollars, mi: Metas.Id): Cost =
      new Cost(asOf, amount, mi) {}

    /**
      */
    def mk(metas: Metas.ValueStore[IO])(amount: Dollars, meta: Meta): IO[Cost] =
      for {
        ret <- metas put meta
      } yield {
        val (mi, _) = ret
        Cost(instant, amount, mi)
      }

    def mkPipe(metas: Metas.ValueStore[IO])(
        amounts: Stream[IO, Dollars],
        metaStream: Stream[IO, Meta]
    ): Stream[IO, Cost] =
      for {
        amount <- amounts
        meta   <- metaStream
        cost   <- Stream eval mk(metas)(amount, meta)
      } yield cost

    implicit def barEq: Eq[Cost]     = { import auto.eq._; semiauto.eq }
    implicit def barShow: Show[Cost] = { import auto.show._; semiauto.show }
  }

  object Costs extends KeyValueStores.KV[FUUID, Cost]

  def costs[F[_]: Sync: ContextShift]: Result[Costs.KeyValueStore[F]] = {
    import CsvImplicits._
    keyValueStore[F] at "target/costs.csv" ofKeyChained Costs
  }
}
