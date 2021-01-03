package io.deftrade
package test

import time._, money._, keyval._, model.{ Contact, Meta, Metas, Money }
import Currency.{ USD }
import currencies._
import refinements.{ IsLabel, IsUnitInterval, Label }
import IsUnitInterval._

import cats.implicits._
import cats.{ Eq, Order, Show }
import cats.derived.{ auto, semi }
import cats.effect.{ ContextShift, IO, Sync }

import eu.timepit.refined
import refined.{ refineV }
import refined.api.{ Refined }
import refined.cats._
import refined.auto._
// import refined.scalacheck.any._

import fs2.{ Stream }

import io.chrisdavenport.cormorant
import cormorant.generic.semiauto._
import cormorant.refined._
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
  sealed abstract case class Product private (
      nut: Nut,
      cost: Cost.Key,
      name: Label,
      factor: Double Refined `[0,1)`,
      owner: Contact.Id
  )

  /**
    */
  object Product extends WithRefinedKey[String, IsLabel, Product] {

    implicit def fooEq: Eq[Product] = { import auto.eq._; semi.eq }
    implicit def fooShow: Show[Product] = { import auto.show._; semi.show }

    /**
      */
    def apply(
        nut: Nut,
        cost: Cost.Key,
        name: Label,
        factor: Double Refined `[0,1)`,
        owner: Contact.Id
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
  }

  lazy val Products = KeyValueStore(Product, KeyValueStore.Param.V).deriveV[Product]

  def products[F[_]: Sync: ContextShift]: Result[Products.KeyValueStore[F]] =
    Result safe {

      // import Product._
      import CsvImplicits._

      val p = "target/products.csv"

      new impl.MemFileKeyValueStore(Product, Paths get p) with Products.KeyValueStore[F] {
        import V._
        final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
      }
    }

  /**
    */
  sealed abstract case class Cost private (
      z: Instant,
      amount: Dollars,
      mi: Meta.Id
  )

  /**
    */
  object Cost extends WithFuuidKey[Cost] {

    /**
      */
    def apply(z: Instant, amount: Dollars, mi: Meta.Id): Cost =
      new Cost(z, amount, mi) {}

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

    implicit def barEq: Eq[Cost] = { import auto.eq._; semi.eq }
    implicit def barShow: Show[Cost] = { import auto.show._; semi.show }
  }

  lazy val Costs = KeyValueStore(Cost, KeyValueStore.Param.V).deriveV[Cost]

  def costs[F[_]: Sync: ContextShift]: Result[Costs.KeyValueStore[F]] =
    Result safe {

      import Cost._
      import CsvImplicits._

      val p = "target/costs.csv"

      new impl.MemFileKeyValueStore(Cost, Paths get p) with Costs.KeyValueStore[F] {
        final protected lazy val fresh: Fresh[Id, Row] = Fresh.shaChain[Row]
      }
    }
  import io.chrisdavenport.cormorant.{ Get, LabelledRead, LabelledWrite, Put }
  import shapeless.{ HList, LabelledGeneric, Lazy }
  def kvs[F[
      _
  ]: Sync: ContextShift, K: Order: Get: Put, V: Show: Eq, K2: Order, V2: Eq, HV <: HList](
      V: WithKey.Aux[K, V]
  )(
      param: KeyValueStore.Param
  )(
      st: param.DependentTypeThunk[K, V]#SubThunk[K2, V2]
  )(implicit
      lgv: LabelledGeneric.Aux[V, HV],
      llr: Lazy[LabelledRead[HV]],
      llw: Lazy[LabelledWrite[HV]]
  ): Result[st.KeyValueStore[F]] =
    Result safe {
      import V._
      import CsvImplicits._
      val p = "target/fixme.csv"
      new impl.MemFileKeyValueStore[F, K, V, HV](V, Paths get p) with st.KeyValueStore[F] {
        final protected lazy val fresh: Fresh[Id, Row] = ??? // Fresh.shaChain[Row]
      }
    }

  // keyValueStore[IO](KeyValueStore.Param.V) at "target/costs.csv" ofKeyChained Costs
  // want this syntax:
  // Cost keyValueStore[IO](KeyValueStore.Param.V) at "target/costs.csv" // chained is implied

  // lazy val Right(products) = keyValueStore[IO] at "target/products.csv" ofChainAddressed Product
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
