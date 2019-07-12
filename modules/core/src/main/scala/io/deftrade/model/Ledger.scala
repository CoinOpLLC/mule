package io.deftrade
package model

import time._, money._, keyval._, capital.Instrument

import cats._
import cats.implicits._

import io.circe.Json

import scala.language.higherKinds

/**
  * Tabulation of `Ledger`s of `Folio`s from `Transaction`s.
  */
abstract class Ledger[Q: Financial] { self =>

  import io.deftrade.keyval.Fresh

  /** n.b. this is where fresh key policy is decided for the ledger */
  implicit def defaultFresh: Fresh[Folio.Key] = Fresh.zeroBasedIncr

  /** Domain specific tools for dealing with `Quantity`s */
  type Quantity = Q
  val Quantity = Financial[Quantity]

  /**
    * How much of a given `Instrument` is held.
    * Can also be thought of as a `Leg` at rest.
    */
  type Position = (Instrument.Key, Quantity)
  object Position

  /** `Leg` := `Position` in motion */
  type Leg = Position
  lazy val Leg = Position

  /**
    * A `Folio` is a set of `Position`s.
    * Can also be thought of as a `Trade` at rest.
    */
  type Folio = Map[Instrument.Key, Quantity]
  object Folio extends WithOpaqueKey[Long, Folio] {
    def empty: Folio                = Map.empty
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)
  }

  /** A `Trade` := `Folio` in motion. */
  type Trade = Folio
  lazy val Trade = Folio

  /**
    * Models ready cash per currency.
    * n.b. the `C` type parameter is purely phantom
    */
  sealed abstract case class Wallet[C] private (val folio: Folio)

  /**
    * Wallet folios are guarranteed non-empty. (TODO: is this worth a new type?)
    * Also, when creating `Wallet`s, the `C` type parameter is checked for `Currency` status.
    */
  object Wallet extends WithOpaqueKey[Long, Folio] {

    private[deftrade] def apply[C: Currency](folio: Folio): Wallet[C] = new Wallet[C](folio) {}

    def apply[C: Currency](p: Position, ps: Position*): Wallet[C] =
      new Wallet[C](Folio(p +: ps: _*)) {}
  }

  /**
    * For `Ledger` changes, the `Transaction` is the concrete record of record, so to speak.
    */
  sealed abstract case class Transaction(
      /**
        * A timestamp is required of all `Recorded Transaction`s, assigned by the `Recorder`
        * - the transaction is provisional until dated, returned as a receipt
        *
        * The exact semantics will depend on the higher level context
        * - (e.g. booking a trade vs receiving notice of settlement).
        *
        * n.b.: there is no currency field; cash payments are reified in currency-as-instrument.
        */
      recordedAt: Option[Instant],
      /**
        */
      debitFrom: Folio.Key,
      /**
        */
      creditTo: Folio.Key,
      /**
        */
      trade: Trade,
      /**
        * Store the _cryptographic hash_ of whatever metadata there is.
        */
      metaSha: Array[Byte]
  )

  /** */
  object Transaction extends WithOpaqueKey[Long, Transaction] {

    /** */
    type Meta = io.circe.Json // f'rinstance

    def apply(
        debitFrom: Folio.Key,
        creditTo: Folio.Key,
        instrument: Instrument.Key,
        amount: Quantity
    ): Transaction =
      new Transaction(
        none,
        debitFrom,
        creditTo,
        Trade(instrument -> amount),
        /** FIXME you see the problem */
        Array.empty[Byte]
      ) {}

    /**
      * ex nihilo, yada yada ... Make sure I can plug in fs2.Stream[cats.effect.IO, ?] etc here
      */
    def empty[F[_]: Monad: MonoidK: Foldable]: F[Transaction] = MonoidK[F].empty[Transaction]

    implicit def order: Eq[Transaction]  = Eq.fromUniversalEquals[Transaction]
    implicit def hash: Hash[Transaction] = Hash.fromUniversalHashCode[Transaction]
  }

  /** Support for multiple contingent deal legs */
  sealed abstract case class AllOrNone(xs: List[Transaction])
  object AllOrNone {}
}
