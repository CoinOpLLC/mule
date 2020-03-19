package io.deftrade
package test

import implicits._
import time._, money._, keyval._ // , model._
import Currency.{ USD }

import cats.{ Eq, Hash, Order, Show }
import cats.implicits._
import cats.derived._
import cats.effect.{ ContextShift, IO }

import fs2.Stream

import eu.timepit.refined
import refined.{ refineV }
import refined.api.{ Refined }
import refined.cats._
import refined.auto._
import refined.scalacheck.any._

import io.chrisdavenport.cormorant
import cormorant.generic.auto._
import cormorant.refined._
import cormorant.implicits._

import io.chrisdavenport.fuuid
import fuuid.FUUID

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

import currencies._
import refinements.{ IsLabel, IsUnitInterval, Label }
import IsUnitInterval._
import scala.concurrent.ExecutionContext.Implicits.global

import java.util.UUID

object mvt {

  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  /** */
  final case class Foo private (
      nut: Nut,
      factor: Double Refined `[0,1)`,
      label: Label,
      bar: Bar.Key,
      zorp: Zorp.Id,
  )

  /** */
  object Foo extends WithOpaqueKey[Long, Foo] {

    implicit def fooEq: Eq[Foo]     = { import auto.eq._; semi.eq }
    implicit def fooShow: Show[Foo] = { import auto.show._; semi.show }

    /** */
    def mk(nut: Nut, label: Label, zorp: Zorp): Stream[IO, Foo] = {
      val Right(factor) = refineV[`[0,1)`](label.value.length / 128.0)
      val Right(bar)    = Bar.Key(555L)
      for (zid <- zorpii put zorp) yield Foo(nut, factor, label, bar, zid)
    }
  }

  /** */
  lazy val foos = keyValueStore[IO] at "target/foos.csv" of Foo

  /** */
  sealed abstract case class Bar(label: Label)

  object Bar extends WithOpaqueKey[Long, Bar] {

    def apply(label: Label): Bar = new Bar(label) {}

    implicit def arbitraryBar: Arbitrary[Bar] =
      Arbitrary {
        for {
          label <- arbitrary[Label]
        } yield Bar(label)
      }

    implicit def barEq: Eq[Bar]     = { import auto.eq._; semi.eq }
    implicit def barShow: Show[Bar] = { import auto.show._; semi.show }
  }

  /** FIXME fuu */
  final case class Zorp(
      fuu: FUUID,
      z: Instant,
      amount: Dollars,
  )

  /** */
  object Zorp extends WithId[Zorp] {

    implicit def zorpEq: Eq[Zorp]     = { import auto.eq._; semi.eq }
    implicit def zorpShow: Show[Zorp] = { import auto.show._; semi.show }
  }

  lazy val Right(zorpii) = valueStore[IO] at "target/zorpii.csv" of Zorp
}

object arbitraryMvt {

  import model.Money

  import Jt8Gen._
  import mvt._

  implicit def arbitraryFoo: Arbitrary[Stream[IO, Foo]] =
    Arbitrary {
      for {
        nut   <- arbitrary[Nut]
        label <- arbitrary[Label]
        zorp  <- arbitrary[Zorp]
      } yield Foo mk (nut, label, zorp)
    }

  implicit def arbitraryZorp: Arbitrary[Zorp] =
    Arbitrary {
      for {
        uuid   <- arbitrary[UUID]
        z      <- arbitrary[Instant]
        amount <- arbitrary[Money[USD]]
      } yield Zorp(FUUID fromUUID uuid, z, amount)
    }
}

class KvesPropSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks {
  import mvt._
  import arbitraryMvt.arbitraryZorp

  lazy val Right(bars) = keyValueStore[IO] at "target/bars.csv" of Bar

  property("some property") {

    // forAll { foo: Foo =>
    //   // println(foo)
    //   true
    // }

    // forAll { bar: Bar =>
    //   val key = Bar.Key unsafe bar.hashCode.toLong
    //   val id  = bars upsert (key, bar)
    //   println(id -> (key -> bar))
    // }
    //
    forAll { zorp: Zorp =>
      println(zorp)
    }
  }
}
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
