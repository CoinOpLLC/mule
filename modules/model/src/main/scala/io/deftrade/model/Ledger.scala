package io.deftrade
package model

import time._
import money._

import opaqueid._

import repos._

import cats._
import cats.implicits._

import eu.timepit.refined
import refined.auto._

import io.circe.Json

import scala.language.higherKinds

/**
  * FIXME wtf is this even, besides eponymous
  */
sealed abstract class Fail extends Product with Serializable { def msg: String }
object Fail {
  final case class Impl(val msg: String) extends Fail
  def apply(msg: String): Fail = Impl(msg)
}

/**
  * `Instrument`s in the house.
  * TODO:
  * - use the XBRL definitions for these, a la OpenGamma
  * - see implementations in `Refine` library
  */
final case class Instrument(meta: Json)
object Instrument {
  type Id = InstrumentIdentifier
  val Id                          = InstrumentIdentifier
  implicit def eq: Eq[Instrument] = Eq by (_.meta)
}

/**
  * Tabulation of `Ledger`s of `Folio`s from `Transaction`s.
  *
  * This shall be the law: A `type Foo` may not depend on any kind of Foo key. (e.g. `type FooId`.)
  * This shall be another: only member names whose appearence cannot be helped may appear here.
  */
abstract class Ledger[Q: Financial] { self =>

  /** independent of type params - FIXME placeholder */
  type Signature = String

  /** independent of type params - FIXME placeholder */
  type Sha256 = Array[Byte]

  /** Domain specific tools for dealing with `Quantity`s */
  type Quantity = Q
  val Quantity          = Financial[Quantity]
  implicit lazy val QF  = Quantity.fractional
  implicit lazy val QCG = Quantity.commutativeGroup

  /** Working repository of `Instrument`s, for the working Trader. */
  object Instruments extends MemInsertableRepository[cats.Id, Instrument.Id, Instrument]
  type Instruments = Instruments.Table

  /**
    * Foundational instrument types to be provided via injection of some kind tbd
    * TODO: capture the fact that the sets of instruments are disjoint.
    * Cash instruments as a repository, or store, shall nevery have F[_] threaded through it.
    */
  type CashInstruments = Map[Currency[_], Set[Instrument.Id]]
  implicit def currencyInstruments: CashInstruments = ??? // this will go over big

  /**
    * How much of a given `Instrument` is held.
    * Can also be thought of as a `Leg` at rest.
    */
  type Position = (Instrument.Id, Quantity)
  object Position

  /**
    * A `Folio` is a set of `Position`s.
    * Can also be thought of as a `Trade` at rest.
    */
  (Eq[Quantity], Eq[Instrument.Id]) |> discardValue

  type Folio = Map[Instrument.Id, Quantity]
  object Folio extends IdC[Long, Folio] {
    def empty: Folio                        = Map.empty
    def apply(ps: Position*): Folio         = accumulate(ps.toList)
    def apply[C](pt: PricedTrade[C]): Trade = PricedTrade.normalize(pt)
  }

  object Folios extends SimplePointInTimeRepository[cats.Id, Folio.Id, Folio] {
    def apply(id: Folio.Id): Folio = get(id).fold(Folio.empty)(identity)
  }
  type Folios = Folios.Table

  /**
    * For `Ledger` changes, the `Transaction` is the concrete record of record, so to speak.
    */
  final case class Transaction(
      /**
        * A `LocalDateTime` is required of all `Recorded Transaction`s, assigned by the `Recorder`
        * - the transaction is provisional until dated, returned as a receipt
        * The exact semantics can vary depending on the higher level context
        * - (e.g. booking a trade vs receiving notice of settlement).
        */
      recorded: Option[LocalDateTime],
      /**
        * *Exactly* two parties to a `Transaction`.
        * - Use `AllOrNone` to compose multiparty `Transaction`s
        */
      parties: (Folio.Id, Folio.Id),
      /**
        * Note: cash payments are reified in currency-as-instrument.
        */
      trade: Trade,
      /**
        * In the `Ledger`, store the _cryptographic hash_ of whatever metadata there is.
        */
      metaSha: Sha256
  )
  object Transaction {
    implicit def order: Eq[Transaction] = Eq.fromUniversalEquals[Transaction]
  }

  /** Support for multiple contingent deal legs */
  final case class AllOrNone(xs: List[Transaction])

  /**
    */
  object Transactions {

    /** fold over me */
    final case class Entry(
        ax: AuthorizedTransaction,
        meta: Json
    )

    /**
      * ex nihilo, yada yada ... Make sure I can plug in fs2.Stream[cats.effect.IO, ?] etc here
      */
    def empty[F[_]: Monad: MonoidK: Foldable]: F[Entry] = MonoidK[F].empty[Entry]
  }

  def groupBy[F[_]: Foldable, A, K](as: F[A])(f: A => K): Map[K, List[A]] =
    as.foldLeft(Map.empty[K, List[A]]) { (acc, a) =>
      (acc get f(a)).fold(acc + (f(a) -> List(a))) { as =>
        acc + (f(a) -> (a +: as))
      }
    }

  def index[F[_]: Foldable, K, V](kvs: F[(K, V)]): Map[K, List[V]] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs map (_._2))
    }

  def accumulate[F[_]: Foldable, K, V: Monoid](kvs: F[(K, V)]): Map[K, V] =
    groupBy(kvs)(_._1) map {
      case (k, kvs) => (k, kvs foldMap (_._2))
    }

}
