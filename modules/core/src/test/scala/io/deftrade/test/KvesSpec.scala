package io.deftrade
package test

import time._, money._, keyval._
import Currency.{ USD }
import currencies._
import refinements.{ IsLabel, IsUnitInterval, Label }
import IsUnitInterval._

import cats.implicits._
import cats.{ Eq, Show }
import cats.effect.{ ContextShift, IO }

import eu.timepit.refined
import refined.{ refineV }
import refined.api.{ Refined }
import refined.cats._
import refined.auto._
// import refined.scalacheck.any._

import fs2.{ Stream }

import io.chrisdavenport.cormorant
import cormorant.generic.auto._
import cormorant.implicits._

import io.chrisdavenport.fuuid
import fuuid.{ FUUIDGen }

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

import scala.concurrent.ExecutionContext.Implicits.global

// import io.circe.Json

// import java.util.UUID

object mvt {

  import _root_.cats.derived.{ auto, semi }
  import cormorant.refined._

  import model.Contact

  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  /**
    */
  sealed abstract case class Foo private (
      nut: Nut,
      bar: Bar.Key,
      label: Label,
      r: Double Refined `[0,1)`,
      contact: Contact.Id
  )

  /**
    */
  object Foo extends WithRefinedKey[String, IsLabel, Foo] {

    implicit def fooEq: Eq[Foo] = { import auto.eq._; semi.eq }
    implicit def fooShow: Show[Foo] = { import auto.show._; semi.show }

    def apply(
        nut: Nut,
        bar: Bar.Key,
        label: Label,
        r: Double Refined `[0,1)`,
        contact: Contact.Id
    ): Foo =
      new Foo(nut, bar, label, r, contact) {}

    /**
      */
    def mk(nut: Nut, bar: Bar, contact: Contact, meta: Misc): Stream[IO, Foo] = {
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
        metas: Stream[IO, Contact]
    ): Stream[IO, Foo] =
      for {
        nut  <- nuts
        bar  <- bars
        meta <- metas
        foo  <- mk(nut, bar, meta)
      } yield foo
  }

  /**
    */
  sealed abstract case class Bar private (z: Instant, amount: Dollars, mi: Misc.Id)

  /**
    */
  object Bar extends WithFuuidKey[Bar] {

    /**
      */
    def apply(z: Instant, amount: Dollars, mi: Misc.Id): Bar =
      new Bar(z, amount, mi) {}

    /**
      */
    def mk(amount: Dollars, meta: Misc): Stream[IO, Bar] =
      for {
        mi <- metas append meta
      } yield Bar(instant, amount, mi)

    def mkPipe(amounts: Stream[IO, Dollars], metas: Stream[IO, Misc]): Stream[IO, Bar] =
      for {
        amount <- amounts
        meta   <- metas
        bar    <- mk(amount, meta)
      } yield bar

    implicit def barEq: Eq[Bar] = { import auto.eq._; semi.eq }
    implicit def barShow: Show[Bar] = { import auto.show._; semi.show }
  }

  import shapeless.labelled._

  lazy val Right(foos)  = keyValueStore[IO] at "target/foos.csv" ofChainAddressed Foo
  lazy val Right(bars)  = keyValueStore[IO] at "target/bars.csv" ofChainAddressed Bar
  lazy val Right(metas) = valueStore[IO] at "target/metas.csv" ofContentAddressed Misc
}

object arbitraryMvt {

  import keval.{ Misc }
  import model.{ Money }

  import Jt8Gen._
  import mvt._

  def drift[A](aa: Gen[A]): Gen[Stream[IO, A]] =
    for (a <- aa) yield Stream eval (IO delay a)

  implicit def FIXME: Arbitrary[Misc] = ???

  implicit def arbitraryFoo: Arbitrary[Stream[IO, Foo]] =
    Arbitrary {
      for {
        bars  <- arbitrary[Stream[IO, Bar]]
        nuts  <- drift(arbitrary[Nut])
        metas <- drift(arbitrary[Misc])
      } yield Foo mkPipe (nuts, bars, metas)
    }

  implicit def arbitraryBar: Arbitrary[Stream[IO, Bar]] =
    Arbitrary {
      for {
        amount <- arbitrary[Money[USD]]
        meta   <- arbitrary[Misc]
      } yield Bar mk (amount, meta)
    }
}

class KvesPropSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks {
  import mvt._
  import arbitraryMvt.arbitraryBar

  property("some property") {

    //   forAll { bar: Bar =>
    //     println(bar)
    //   }
    //
    //   forAll { bar: Misc =>
    //     val key = Misc.Key unsafe bar.hashCode.toLong
    //     val id  = bars upsert (key, bar)
    //     println(id -> (key -> bar))
    //   }
    //
    //   forAll { foo: Foo =>
    //     println(foo)
    //   }
  }
}
