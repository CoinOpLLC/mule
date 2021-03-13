package io.deftrade
package test

import keyval._

import shapeless.nat.{ _0, _1 }

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semiauto }
import cats.effect.{ ContextShift, IO, Sync }

import enumeratum._

import eu.timepit.refined
import refined.refineV
import refined.api.Refined
import refined.string._
import refined.numeric._
import refined.cats._
import refined.auto._

import io.chrisdavenport.cormorant

import io.chrisdavenport.fuuid
import fuuid.{ FUUID, FUUIDGen }

import io.chrisdavenport.cats.time._

import fs2.{ Stream }

import org.scalacheck._
// import org.scalacheck.cats.implicits._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

import scala.concurrent.ExecutionContext.Implicits.global

import java.time.Instant

/**
  */
sealed abstract case class Contact private (
    final val name: Contact.Name,
    final val address: Contact.USAddress,
    final val cell: Contact.USPhone,
    final val email: Contact.Email,
    final val url: Option[String Refined Url]
)

/**
  */
object Contact {

  import io.circe.{ Decoder, Encoder }
  import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
  // import io.circe.refined._

  implicit lazy val contactEq: Eq[Contact]     = { import auto.eq._; semiauto.eq }
  implicit lazy val contactShow: Show[Contact] = { import auto.show._; semiauto.show }

  implicit lazy val decoder: Decoder[Contact] = { import io.circe.refined._; deriveDecoder }
  implicit lazy val encoder: Encoder[Contact] = { import io.circe.refined._; deriveEncoder }

  /**
    */
  def apply(
      name: Contact.Name,
      address: Contact.USAddress,
      cell: Contact.USPhone,
      email: Contact.Email,
      url: Option[String Refined Url]
  ): Contact =
    new Contact(name, address, cell, email, url) {}

  /**
    */
  case class Name(
      first: Label,
      middle: Option[Label],
      last: Label
  )

  /**
    */
  object Name {

    implicit lazy val nameEq: Eq[Name]     = semiauto.eq
    implicit lazy val nameShow: Show[Name] = semiauto.show

    implicit lazy val decoder: Decoder[Name] = { import io.circe.refined._; deriveDecoder }
    implicit lazy val encoder: Encoder[Name] = { import io.circe.refined._; deriveEncoder }
  }

  /**
    */
  case class USAddress(
      street: Label,
      street2: Option[Label],
      city: Label,
      state: String Refined MatchesRegex["""[A-Z]{2}"""], // """
      zip: USZip
  )

  /**
    */
  object USAddress {

    implicit lazy val usAddressEq: Eq[USAddress]     = semiauto.eq
    implicit lazy val usAddressShow: Show[USAddress] = semiauto.show

    implicit lazy val decoder: Decoder[USAddress] = { import io.circe.refined._; deriveDecoder }
    implicit lazy val encoder: Encoder[USAddress] = { import io.circe.refined._; deriveEncoder }
  }

  private def digits(n: Int) = s"""[0-9]{${n.toString}}"""

  /**
    */
  final val TenDigit = digits(10)

  /**
    */
  final type IsUSPhone = MatchesRegex[TenDigit.type]

  /**
    */
  final type USPhone = String Refined IsUSPhone

  /**
    */
  final val Zip = s"${digits(5)}|${digits(5 + 4)}"

  /**
    */
  final type IsUSZip = MatchesRegex[Zip.type]

  /**
    */
  final type USZip = String Refined IsUSZip

  /** TODO: [[http://www.regular-expressions.info/email.html investigate further]] */
  final val IsEmail =
    """[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"""

  /**
    */
  final type Email = String Refined MatchesRegex[IsEmail.type]
}

/**
  */
case object Contacts extends ValueStores.SADT[Contact]

object email {

  /** TODO: [[http://www.regular-expressions.info/email.html investigate further]] */
  final val EmailRx =
    """[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"""

  /**
    */
  final type IsEmail = MatchesRegex[EmailRx.type]

  /**
    */
  final type Email = String Refined IsEmail
}

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

object invoices {

  import keyval._
  import OpaqueKey._

  import test.email._

  sealed abstract case class Party private (
      final val handle: Label,
      final val email: Email,
  )

  object Party {
    def apply(handle: Label, email: Email): Party =
      new Party(handle, email) {}
  }

  case object Parties extends KeyValueStores.KV[FUUID, Party]

  sealed trait Dollar
  final type Dollars = BigDecimal Refined Dollar

  sealed abstract case class Invoice private (
      final val asOf: Instant,
      final val nut: Nut,
      final val quantity: Int Refined Positive,
      final val from: Parties.Key,
      final val to: Parties.Key,
      final val amount: Dollars,
      final val memo: Label
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
      new Invoice(asOf = Instant.now, nut, quantity, from, to, amount, memo) {}

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
      val Right(amount)   = refineV[Dollar](BigDecimal(total))

      Invoice(asOf = Instant.now, Nut withName nut, quantity, from, to, amount, memo)
    }

    implicit lazy val invoiceEq: Eq[Invoice]     = { import auto.eq._; semiauto.eq }
    implicit lazy val invoiceShow: Show[Invoice] = { import auto.show._; semiauto.show }
  }

  case object Invoices extends KeyValueStores.KV[Long OpaqueKey Invoice, Invoice]

  def invoices[F[_]: Sync: ContextShift]: Result[Invoices.KeyValueStore[F]] = {
    import csv.implicits._
    import io.chrisdavenport.cormorant
    import cormorant.generic.semiauto._
    import cormorant.refined._
    import cormorant.implicits._
    csv.kvs[F] at "target/invoices.csv" ofKeyChained Invoices
  }
}

/**
  */
object mvt {

  import invoices._

  type `[0,1)` = Interval.ClosedOpen[_0, _1]

  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  /**
    */
  sealed abstract case class Product private (
      final val nut: Nut,
      final val cost: Costs.Key,
      final val name: Label,
      final val factor: Double Refined `[0,1)`, //  []
      final val owner: Contacts.Id
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
        factor: Double Refined `[0,1)`, //  []
        owner: Contacts.Id
    ): Product =
      new Product(nut, cost, name, factor, owner) {}

    /**
      */
    def mk(
        contacts: Contacts.ValueStore[IO],
        costs: Costs.KeyValueStore[IO]
    )(
        nut: Nut,
        cost: Cost,
        owner: Contact,
    ): IO[Product] =
      for {
        key <- FUUIDGen[IO].random
        o   <- contacts put owner
        _   <- costs.put(key, cost)
      } yield {
        val Right(factor) =
          refineV[`[0,1)`](cost.show.size / (cost.show.size + 42).toDouble)
        val Right(name) = refineV[IsLabel](s"${nut.show}::${key.show}")
        Product(nut, key, name, factor, o._1)
      }

    /** FIXME factor this shyte with applicative and zip and whatnot */
    def mkPipe(
        contactsVS: Contacts.ValueStore[IO],
        costsKVS: Costs.KeyValueStore[IO]
    )(
        nuts: Stream[IO, Nut],
        costs: Stream[IO, Cost],
        contacts: Stream[IO, Contact],
    ): Stream[IO, Product] =
      (
        for {
          n <- nuts
          d <- costs
          c <- contacts
        } yield (n, d, c)
      ) evalMap (mk(contactsVS, costsKVS) _).tupled

    implicit def productEq: Eq[Product]     = { import auto.eq._; semiauto.eq }
    implicit def productShow: Show[Product] = { import auto.show._; semiauto.show }
  }

  case object Products extends KeyValueStores.KV[Label, Product]
  import csv.implicits._
  import cormorant.generic.semiauto._
  import cormorant.refined._
  import cormorant.implicits._

  def products[F[_]: Sync: ContextShift]: Result[Products.KeyValueStore[F]] =
    csv.kvs[F] at "target/products.csv" ofKeyChained Products

  /**
    */
  sealed abstract case class Cost private (
      final val asOf: Instant,
      final val amount: Dollars,
  )

  /**
    */
  object Cost {

    /**
      */
    def apply(asOf: Instant, amount: Dollars): Cost =
      new Cost(asOf, amount) {}

    /**
      */
    def mk[F[_]: Sync](
        costs: Costs.KeyValueStore[F]
    )(
        fuuid: FUUID,
        amount: Dollars
    ): F[Costs.Id] =
      for {
        id <- costs put (fuuid, Cost(Instant.now, amount))
      } yield id

    def mkPipe[F[_]: Sync](
        costs: Costs.KeyValueStore[F]
    )(
        amounts: Stream[F, (FUUID, Dollars)]
    ): Stream[F, Costs.Id] =
      for {
        ka   <- amounts
        cost <- Stream eval mk(costs)(ka._1, ka._2)
      } yield cost

    implicit def barEq: Eq[Cost]     = { import auto.eq._; semiauto.eq }
    implicit def barShow: Show[Cost] = { import auto.show._; semiauto.show }
  }

  case object Costs extends KeyValueStores.KV[FUUID, Cost]

  def costs[F[_]: Sync: ContextShift]: Result[Costs.KeyValueStore[F]] = {
    import csv.implicits._
    import io.chrisdavenport.cormorant
    import cormorant.generic.semiauto._
    import cormorant.refined._
    import cormorant.implicits._

    // csv.kvs[F] at "target/costs.csv" ofKeyChained Costs
    ???
  }
}
