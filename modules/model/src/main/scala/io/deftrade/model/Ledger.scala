package io.deftrade
package model

import time._
import money._

import keyval._

import repos._

import cats._
import cats.data.NonEmptySet // which is also Sorted
import cats.implicits._

import eu.timepit.refined
import refined.auto._

import io.circe.Json

// import scala.collection.immutable.SortedSet
import scala.language.higherKinds

/**
  * `Instrument`s in the house.
  * TODO:
  * - use the XBRL definitions for these, a la OpenGamma
  * - see implementations in `Refine` library
  */
final case class Instrument(meta: Json)
object Instrument {
  type Key = keys.InstrumentIdentifier
  val Key                         = keys.InstrumentIdentifier
  implicit def eq: Eq[Instrument] = Eq by (_.meta)
}

/**
  * Tabulation of `Ledger`s of `Folio`s from `Transaction`s.
  */
abstract class Ledger[Q: Financial] { self =>

  import io.deftrade.keyval.Fresh

  /** n.b. this is where fresh key policy is decided for the ledger */
  implicit def defaultFresh: Fresh[Folio.Key] = Fresh.zeroBasedIncr

  /** independent of type params - FIXME placeholder */
  type Signature = String

  /** independent of type params - FIXME placeholder */
  type Sha256 = Array[Byte]

  /** Domain specific tools for dealing with `Quantity`s */
  type Quantity = Q
  val Quantity = Financial[Quantity]

  /** Working repository of `Instrument`s, for the working Trader. */
  object Instruments extends MemInsertableRepository[cats.Id, Instrument.Key, Instrument]
  type Instruments = Instruments.Table

  /** */
  type CashInstruments[C] = NonEmptySet[(Instrument.Key, Folio.Key)] // phantom type `C`
  object CashInstruments

  /**
    * `CashInstruments`:
    * - is a configuration parameter only.
    * - is not as a repository, or store.
    * - shall nevery have F[_] threaded through it.
    *
    * All `Currency` instances in scope are required to have a `CashInstruments` instance.
    *
    * TODO: capture the fact that the sets of instruments are disjoint.
    */
  implicit def cashInstruments[C: Currency]: CashInstruments[C] = cashInstrumentsMap(Currency[C])

  private lazy val cashInstrumentsMap: Map[Currency[_], CashInstruments[_]] = Map.empty // cfg!
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
  object Folio extends WithKey[Long, Folio] {
    def empty: Folio                = Map.empty
    def apply(ps: Position*): Folio = indexAndSum(ps.toList)
  }

  object Folios extends SimplePointInTimeRepository[cats.Id, Folio.Key, Folio] {
    def apply(id: Folio.Key): Folio = get(id).fold(Folio.empty)(identity)
  }
  type Folios = Folios.Table

  /** A `Trade` := `Folio` in motion. */
  type Trade = Folio
  lazy val Trade = Folio

  /**
    * For `Ledger` changes, the `Transaction` is the concrete record of record, so to speak.
    */
  final case class Transaction(
      /**
        * A timestamp is required of all `Recorded Transaction`s, assigned by the `Recorder`
        * - the transaction is provisional until dated, returned as a receipt
        * The exact semantics can vary depending on the higher level context
        * - (e.g. booking a trade vs receiving notice of settlement).
        */
      recordedAt: Option[Instant],
      /**
        * *Exactly* two parties to a `Transaction`.
        * Convention: (from, to)
        * - Use `AllOrNone` to compose multiparty `Transaction`s
        */
      parties: (Folio.Key, Folio.Key),
      /**
        * Note: cash payments are reified in currency-as-instrument.
        */
      trade: Folio,
      /**
        * In the `Ledger`, store the _cryptographic hash_ of whatever metadata there is.
        */
      metaSha: Sha256
  )
  object Transaction {

    type Meta = io.circe.Json // f'rinstance

    /**
      * ex nihilo, yada yada ... Make sure I can plug in fs2.Stream[cats.effect.IO, ?] etc here
      */
    def empty[F[_]: Monad: MonoidK: Foldable]: F[Transaction] = MonoidK[F].empty[Transaction]

    implicit def order: Eq[Transaction]  = Eq.fromUniversalEquals[Transaction]
    implicit def hash: Hash[Transaction] = Hash.fromUniversalHashCode[Transaction]
  }

  /** fold over me */
  final case class Entry(ax: Transaction, meta: Json)

  /** Support for multiple contingent deal legs */
  final case class AllOrNone(xs: List[Transaction])

  // FIXME this is a hack placeholder
  type Transactions[F[_]] = Foldable[F]

  /**
    */
  object Transactions { // FIME this becomes a stream like repo (???)

  }

}
