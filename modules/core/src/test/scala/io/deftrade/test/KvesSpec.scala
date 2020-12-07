package io.deftrade
package test

import time._, money._, keyval._, model.{ Contact, Meta, Metas, Money }
import Currency.{ USD }
import currencies._
import refinements.{ IsLabel, IsUnitInterval, Label }
import IsUnitInterval._

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semi }
import cats.effect.{ ContextShift, IO, Sync }

import eu.timepit.refined
import refined.{ refineV }
import refined.api.{ Refined }
import refined.cats._
import refined.auto._
// import refined.scalacheck.any._

import fs2.{ Stream }

import shapeless.{ HList, LabelledGeneric, Lazy }

import io.chrisdavenport.cormorant
import cormorant._
// import cormorant.{ Get, LabelledRead, LabelledWrite, Put }
import cormorant.generic.semiauto._
// import cormorant.parser._
import cormorant.implicits._

import io.chrisdavenport.fuuid
import fuuid.{ FUUIDGen }

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck._
import org.scalacheck.ScalacheckShapeless._
import Arbitrary.arbitrary

import scala.concurrent.ExecutionContext.Implicits.global

import java.nio.file.{ Paths }

// import io.circe.Json

/**
  */
object mvt {

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

    implicit def fooEq: Eq[Foo]     = { import auto.eq._; semi.eq }
    implicit def fooShow: Show[Foo] = { import auto.show._; semi.show }

    /**
      */
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
        ret <- metas put meta
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
      (
        for {
          n <- nuts
          b <- bs
          c <- contacts
          m <- ms
        } yield (n, b, c, m)
      ) evalMap (mk(metas, bars) _).tupled
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
        ret <- metas put meta
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

    implicit def barEq: Eq[Bar]     = { import auto.eq._; semi.eq }
    implicit def barShow: Show[Bar] = { import auto.show._; semi.show }
  }

  sealed abstract case class Zorp private (i: Int, s: String)

  object Zorp extends WithFuuidKey[Zorp] {
    def apply(i: Int, s: String): Zorp =
      new Zorp(i, s) {}
    implicit def zorpEq: Eq[Zorp]     = { import auto.eq._; semi.eq }
    implicit def zorpShow: Show[Zorp] = { import auto.show._; semi.show }
  }

  lazy val Zorps = KeyValueStore(Zorp, KeyValueStore.Param.V).deriveV[Zorp]

  def zorps[F[_]: Sync: ContextShift]: Result[Zorps.KeyValueStore[F]] =
    Result safe {

      import Zorp._

      // implicit val lga = LabelledGeneric[Zorp]

      val p = "target/bars.csv"

      // implicit val llr: LabelledRead[Zorp]  = deriveLabelledRead
      // implicit val llw: LabelledWrite[Zorp] = deriveLabelledWrite

      // Get[io.chrisdavenport.fuuid.FUUID]

      import CsvImplicits.{ fuuidGet, fuuidPut }

      new impl.MemFileKeyValueStore(Zorp, Paths get p) with Zorps.KeyValueStore[F] {
        // import V._
        final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
      }
    }

  lazy val BarsDTT = KeyValueStore(Bar, KeyValueStore.Param.V)
  lazy val Bars    = BarsDTT.deriveV[Bar]
  def bars[F[_]: Sync: ContextShift]: Result[Bars.KeyValueStore[F]] =
    Result safe {

      // implicit val lga                     = LabelledGeneric[Bar]

      // implicit val llr: LabelledRead[Bar]  = deriveLabelledRead
      // implicit val llw: LabelledWrite[Bar] = deriveLabelledWrite

      ???
      // new impl.MemFileKeyValueStore(Bar, Paths get p) with Bars.KeyValueStore[F] {
      //   import V._
      //   final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
      // }
    }

  // keyValueStore[IO](KeyValueStore.Param.V) at "target/bars.csv" ofKeyChained Bars
  // want this syntax:
  // Bar keyValueStore[IO](KeyValueStore.Param.V) at "target/bars.csv" // chained is implied

  // lazy val Right(foos) = keyValueStore[IO] at "target/foos.csv" ofChainAddressed Foo
  // lazy val Right(metas) = ???
  // valueStore[IO] at "target/metas.csv" ofContentAddressed SADT
}

object arbitraryMvt {

  // import Jt8Gen._
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
  // import mvt._
  // import arbitraryMvt.arbitraryBar

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
