package io.deftrade
package test

import syntax._
import time._, money._, keyval._
import Currency.{ USD }

import cats.implicits._
import cats.{ Eq, Hash, Order, Show }
import cats.effect.{ ContextShift, IO }

import fs2.{ Pipe, Stream }

import eu.timepit.refined
import refined.{ refineMV, refineV }
import refined.api.{ Refined }
import refined.cats._
import refined.auto._
// import refined.scalacheck.any._

import io.chrisdavenport.cormorant
import cormorant.generic.auto._
import cormorant.implicits._

import io.chrisdavenport.fuuid
import fuuid.{ FUUID, FUUIDGen }

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

import currencies._
import refinements.{ IsLabel, IsUnitInterval, Label }
import IsUnitInterval._
import scala.concurrent.ExecutionContext.Implicits.global

// import io.circe.Json

// import java.util.UUID

object mvt {

  import _root_.cats.derived.{ auto, semi }
  import cormorant.refined._

  import model.Meta

  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  /**
    *
    */
  sealed abstract case class Foo private (
      nut: Nut,
      bk: Bar.Key,
      label: Label,
      r: Double Refined `[0,1)`,
      mi: Meta.Id
  )

  /** */
  object Foo extends WithRefinedKey[String, IsLabel, Foo] {

    implicit def fooEq: Eq[Foo]     = { import auto.eq._; semi.eq }
    implicit def fooShow: Show[Foo] = Show show (_.label.value)

    def apply(nut: Nut, bk: Bar.Key, label: Label, r: Double Refined `[0,1)`, mi: Meta.Id): Foo =
      new Foo(nut, bk, label, r, mi) {}

    /** */
    def mk(nut: Nut, bar: Bar, meta: Meta): Stream[IO, Foo] = {
      val Right(r) =
        refineV[`[0,1)`](bar.show.size / (bar.show.size + meta.show.size).toDouble)
      for {
        bk <- Stream eval FUUIDGen[IO].random
        mi <- metas append meta
        _  <- bars insert (bk, bar)
      } yield {
        val Right(label) = refineV[IsLabel](s"${nut.show}::${bk.show}:${mi.show}")
        Foo(nut, bk, label, r, mi)
      }
    }

    /** FIXME factor this shyte with applicative and zip and whatnot */
    def mkPipe(
        nuts: Stream[IO, Nut],
        bars: Stream[IO, Bar],
        metas: Stream[IO, Meta]
    ): Stream[IO, Foo] =
      for {
        nut  <- nuts
        bar  <- bars
        meta <- metas
        foo  <- mk(nut, bar, meta)
      } yield foo
  }

  /**
    *
    */
  sealed abstract case class Bar private (z: Instant, amount: Dollars, mi: Meta.Id)

  /** */
  object Bar extends WithFuuidKey[Bar] {

    /** */
    def apply(z: Instant, amount: Dollars, mi: Meta.Id): Bar =
      new Bar(z, amount, mi) {}

    /** */
    def mk(amount: Dollars, meta: Meta): Stream[IO, Bar] =
      for {
        mi <- metas append meta
      } yield Bar(instant, amount, mi)

    def mkPipe(amounts: Stream[IO, Dollars], metas: Stream[IO, Meta]): Stream[IO, Bar] =
      for {
        amount <- amounts
        meta   <- metas
        bar    <- mk(amount, meta)
      } yield bar

    implicit def barEq: Eq[Bar]     = { import auto.eq._; semi.eq }
    implicit def barShow: Show[Bar] = { import auto.show._; semi.show }
  }

  lazy val Right(foos)  = keyValueStore[IO] at "target/foos.csv" ofChainAddressed Foo
  lazy val Right(bars)  = keyValueStore[IO] at "target/bars.csv" ofChainAddressed Bar
  lazy val Right(metas) = valueStore[IO] at "target/metas.csv" ofContentAddressed Meta
}

object arbitraryMvt {

  import model.{ Meta, Money }

  import Jt8Gen._
  import mvt._

  def drift[A](aa: Gen[A]): Gen[Stream[IO, A]] =
    for (a <- aa) yield Stream eval (IO delay a)

  implicit def FIXME: Arbitrary[Meta] = ???

  implicit def arbitraryFoo: Arbitrary[Stream[IO, Foo]] =
    Arbitrary {
      for {
        bars  <- arbitrary[Stream[IO, Bar]]
        nuts  <- drift(arbitrary[Nut])
        metas <- drift(arbitrary[Meta])
      } yield Foo mkPipe (nuts, bars, metas)
    }

  implicit def arbitraryBar: Arbitrary[Stream[IO, Bar]] =
    Arbitrary {
      for {
        amount <- arbitrary[Money[USD]]
        meta   <- arbitrary[Meta]
      } yield Bar mk (amount, meta)
    }
}

object ledger {
  import cormorant.refined._

  import model.{ capital, Folio, Meta, Trade, Transaction }, capital.Instrument

  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  val Right((instruments, trades, transactions, folios, metas)) = for {
    instruments  <- keyValueStore[IO] at "instruments.csv" ofChainAddressed Instrument
    trades       <- valueStore[IO] at "trades.csv" ofContentAddressed Trade
    transactions <- valueStore[IO] at "transactions.csv" ofChainAddressed Transaction
    folios       <- keyValueStore[IO] at "folios.csv" ofChainAddressed Folio
    metas        <- valueStore[IO] at "metas.csv" ofContentAddressed Meta
  } yield (instruments, trades, transactions, folios, metas)
}

class KvesPropSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks {
  import mvt._
  import arbitraryMvt.arbitraryBar

  property("some property") {

    //   forAll { bar: Bar =>
    //     println(bar)
    //   }
    //
    //   forAll { bar: Meta =>
    //     val key = Meta.Key unsafe bar.hashCode.toLong
    //     val id  = bars upsert (key, bar)
    //     println(id -> (key -> bar))
    //   }
    //
    //   forAll { foo: Foo =>
    //     println(foo)
    //   }
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
