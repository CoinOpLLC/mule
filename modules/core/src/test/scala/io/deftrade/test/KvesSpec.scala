package io.deftrade
package test

import time._, money._, keyval._, model._
import Currency.{ USD }

import cats.implicits._

import eu.timepit.refined
import refined.{ refineV }
import refined.api.{ Refined }
import refined.collection.NonEmpty
import refined.numeric._
import refined.auto._

// import io.chrisdavenport.cormorant
// import cormorant._
// import cormorant.generic.auto._
// import cormorant.parser._
// import cormorant.refined._
// import cormorant.implicits._

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

object mvt {

  import currencies._
  import java.util.UUID
  import refinements.{ IsLabel, IsUnitInterval, Label }
  import IsUnitInterval._

  type UnitInterval = Double Refined `[0,1)`

  object Foo extends WithOpaqueKey[Long, Foo] {
    def mk(nut: Nut, s: String): Foo = refineV[IsLabel](s) match {
      case Left(_) => mk(nut, "baseline")
      case Right(label) =>
        val Right(factor) = refineV[`[0,1)`](label.value.length / 128.0)
        new Foo(
          nut,
          factor,
          label,
          Bar.Key.reserved,
          Zorp.Id(42)
        ) {}
    }

    implicit def arbitraryFoo: Arbitrary[Foo] =
      Arbitrary {
        for {
          nut <- arbitrary[Nut]
          str <- arbitrary[String]
        } yield mk(nut, str)
      }
  }

  /** */
  sealed abstract case class Foo(
      nut: Nut,
      factor: UnitInterval,
      label: Label,
      bar: Bar.Key,
      zorp: Zorp.Id
  )

  final case class Bar(i: Int, d: Double, b: Boolean, l: Long, x: BigDecimal, c: Char)
  object Bar extends WithOpaqueKey[Long, Bar]

  final case class Zorp(
      uuid: UUID,
      // z: Instant,
      amount: Dollars,
  )

  object Zorp extends WithId[Zorp] {
    implicit def arbitraryZorp: Arbitrary[Zorp] =
      Arbitrary {
        for {
          uuid   <- arbitrary[UUID]
          amount <- arbitrary[Money[USD]]
        } yield Zorp(uuid, amount)
      }
  }

  implicitly[Arbitrary[Foo]]
  implicitly[Arbitrary[Bar]]
  implicitly[Arbitrary[UUID]]
  // implicitly[Arbitrary[Instant]]
  implicitly[Arbitrary[Dollars]]
  implicitly[Arbitrary[Zorp]]
}

object invoices {

  import currencies._

  sealed trait Base
  case class BaseIntString(i: Int, s: String)         extends Base
  case class BaseDoubleBoolean(d: Double, b: Boolean) extends Base

  sealed abstract case class Invoice(
      asOf: Instant,
      nut: Nut,
      quantity: Int Refined Positive,
      from: Party.Key,
      to: Party.Key,
      amount: Dollars,
      memo: String Refined NonEmpty
  )

  object Invoice extends WithOpaqueKey[Long, Invoice] {

    def mk(
        nut: Nut,
        jars: Int,
        from: Party.Key,
        to: Party.Key,
        total: Double,
        instructions: String = ""
    ): Invoice = {

      val Right(quantity) = refineV[Positive](jars min 1)
      val Right(memo)     = refineV[NonEmpty](s"special instructions: $instructions")
      val amount          = dollars(total)

      new Invoice(asOf = instant, nut, quantity, from, to, amount, memo) {}
    }
  }
}

/** */
// object Repos extends WithOpaqueKey[Long, OMS[Id]] {
//
//   /** */
//   type LegalEntities = Party.Table
//
//   /** */
//   implicit def w00t: Fresh[Party.Key] = ??? // to compile duh
//   /** */
//   object LegalEntities extends SimplePointInTimeRepository[_root_.cats.Id, Party.Key, Party]
//
//   /**
//     * `CashInstruments`:
//     * - is a configuration parameter only.
//     * - is not as a repository, or store.
//     * - shall never have F[_] threaded through it.
//     *
//     * All `Currency` instances in scope are required to have a `CashInstruments` instance.
//     */
//   object CashInstruments {
//
//     def apply[C: Currency]: Wallet[C] = (cash get Currency[C]).fold(???) { x =>
//       Wallet apply [C] Folios(x)
//     }
//
//     private lazy val cash: Map[CurrencyLike, Folio.Key] = Map.empty
//   }
//
//   /** */
//   type CashInstruments = Wallet.Table
//
//   /** */
//   object Instruments extends MemInsertableRepository[_root_.cats.Id, Instrument.Key, Instrument]
//
//   /** */
//   type Instruments = Instrument.Table
//
//   implicit def eq: Eq[Folio] = ???
//
//   /** */
//   object Folios extends SimplePointInTimeRepository[_root_.cats.Id, Folio.Key, Folio] {
//     def apply(id: Folio.Key): Folio = get(id).fold(Folio.empty)(identity)
//   }
//
//   /** */
//   type Folios = Folio.Table
//
//   /** */
//   implicit def freshAccountNo: Fresh[Account.Key] = ??? // to compile duh
//
//   /** */
//   object Accounts extends SimplePointInTimeRepository[_root_.cats.Id, Account.Key, Account]
//
//   /** */
//   type Accounts = Account.Table
//
//   /** FIXME: this is just a placeholder - needs to reference [[Transaction]] */
//   type Transactions[F[_]] = Foldable[F]
//
//   /** FIME this becomes a stream like repo (???)      */
//   object Transactions {}
//
//   /** */
//   lazy val Markets: Repository[_root_.cats.Id, Market.Key, Market] =
//     SimplePointInTimeRepository[_root_.cats.Id, Market.Key, Market]()
//
//   /** */
//   type Markets = Markets.Table
//
//   /**
//     *
//     *  this is something of an abuse of the original PiT concept,
//     * which models slowly evolving entities *with identity (key) which survives updates.
//     *
//     *  `Orders` is exactly the opposite.
//     *
//     *  But the open date range for "current `Table`" models the "open orders" concept perfectly.
//     *
//     *  TODO: is this really worthwhile?
//     *
//     */
//   type Orders = model.Order.Table
//
//   /** */
//   object Orders extends SimplePointInTimeRepository[_root_.cats.Id, model.Order.Key, model.Order[USD]]
//
//   /**  nb `Exectutions` are recorded as [[Transactions]] this completing the life cycle */
//   type Executions = Executions.Table
//
//   /** */
//   object Executions extends MemAppendableRepository[_root_.cats.Id, Execution.Key, Execution]
// }

class KvesPropSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks {
  import mvt._
  property("some property about Foo") {
    forAll { foo: Foo =>
      // Ensure foo has the required property
    }
  }
}
