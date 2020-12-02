package io.deftrade
package test

import time._, money._, keyval._, model.{ Contact, Contacts, Meta, Metas }
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
    def mk(
        metas: Metas.ValueStore[IO],
        bars: Bars.KeyValueStore[IO]
    )(
        nut: Nut,
        bar: Bar,
        contact: Contact,
        meta: Meta
    ): IO[Foo] =
      for {
        key <- FUUIDGen[IO].random
        ret <- metas.put(SADT from meta)
        _   <- bars.put(key, bar)
      } yield {
        val Right(r) =
          refineV[`[0,1)`](bar.show.size / (bar.show.size + meta.show.size).toDouble)
        val Right(label) = refineV[IsLabel](s"${nut.show}::${key.show}:${ret._1.show}")
        Foo(nut, key, label, r, ret._1)
      }

    /** FIXME factor this shyte with applicative and zip and whatnot */
    def mkPipe(
        metas: Metas.ValueStore[IO],
        bars: Bars.KeyValueStore[IO]
    )(
        nuts: Stream[IO, Nut],
        bs: Stream[IO, Bar],
        contacts: Stream[IO, Contact],
        ms: Stream[IO, Meta]
    ): Stream[IO, Foo] =
      (for {
        nut     <- nuts
        bar     <- bs
        contact <- contacts
        meta    <- ms
      } yield (nut, bar, contact, meta)) evalMap (mk(metas, bars) _).tupled
  }

  lazy val Foos = KeyValueStore(Foo, KeyValueStore.Param.V).deriveV[Foo]

  /**
    */
  sealed abstract case class Bar private (z: Instant, amount: Dollars, mi: Meta.Id)

  /**
    */
  object Bar extends WithFuuidKey[Bar] {

    /**
      */
    def apply(z: Instant, amount: Dollars, mi: Meta.Id): Bar =
      new Bar(z, amount, mi) {}

    /**
      */
    def mk(metas: Metas.ValueStore[IO])(amount: Dollars, meta: Meta): IO[Bar] =
      for {
        ret <- metas put (SADT from meta)
      } yield {
        val (mi, _) = ret
        Bar(instant, amount, mi)
      }

    def mkPipe(metas: Metas.ValueStore[IO])(
        amounts: Stream[IO, Dollars],
        metaStream: Stream[IO, Meta]
    ): Stream[IO, Bar] =
      for {
        amount <- amounts
        meta   <- metaStream
        bar    <- Stream eval mk(metas)(amount, meta)
      } yield bar

    implicit def barEq: Eq[Bar] = { import auto.eq._; semi.eq }
    implicit def barShow: Show[Bar] = { import auto.show._; semi.show }
  }

  lazy val Bars = KeyValueStore(Bar, KeyValueStore.Param.V).deriveV[Bar]
  // lazy val Right(foos) = keyValueStore[IO] at "target/foos.csv" ofChainAddressed Foo
  // lazy val Right(bars) = keyValueStore[IO] at "target/bars.csv" ofChainAddressed Bar
  // lazy val Right(metas) = ???
  // valueStore[IO] at "target/metas.csv" ofContentAddressed SADT
}

object arbitraryMvt {

  import keyval.{ WithSADT }
  import model.{ Money }

  import Jt8Gen._
  import mvt._

  def drift[A](aa: Gen[A]): Gen[Stream[IO, A]] =
    for (a <- aa) yield Stream eval (IO delay a)

  implicit def FIXME_0: Arbitrary[Meta]    = ???
  implicit def FIXME_1: Arbitrary[Contact] = ???
  implicit def FIXME_2: Arbitrary[Bar]     = ???

  def metas: Metas.ValueStore[IO]  = ???
  def bars: Bars.KeyValueStore[IO] = ???

  implicit def arbitraryFoo: Arbitrary[Stream[IO, Foo]] =
    Arbitrary {
      for {
        bs       <- drift(arbitrary[Bar])
        nuts     <- drift(arbitrary[Nut])
        contacts <- drift(arbitrary[Contact])
        ms       <- drift(arbitrary[Meta])
      } yield Foo.mkPipe(metas, bars)(nuts, bs, contacts, ms)
    }

  implicit def arbitraryBar: Arbitrary[IO[Bar]] =
    Arbitrary {
      for {
        amount <- arbitrary[Money[USD]]
        meta   <- arbitrary[Meta]
      } yield Bar.mk(metas)(amount, meta)
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
    //   forAll { bar: SADT =>
    //     val key = SADT.Key unsafe bar.hashCode.toLong
    //     val id  = bars upsert (key, bar)
    //     println(id -> (key -> bar))
    //   }
    //
    //   forAll { foo: Foo =>
    //     println(foo)
    //   }
  }
}
