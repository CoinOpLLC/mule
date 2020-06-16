package demo

import io.deftrade._
import time._, money._, keyval._, model._, capital._

import cats.implicits._
import cats.Eq
import cats.effect.{ ContextShift, IO }

import eu.timepit.refined
import refined.{ refineMV, refineV }
import refined.api.{ Refined }
import refined.cats._
import refined.auto._

import io.chrisdavenport.cormorant
import cormorant.implicits._
import cormorant.generic.auto._
import cormorant.refined._

import io.chrisdavenport.fuuid
import fuuid.{ FUUID, FUUIDGen }

/** */
object ledgerz {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit def contextShiftIO: ContextShift[IO] = IO contextShift global

  import _root_.io.deftrade.syntax._

  import Quantity._

  Eq[Instrument.Key] |> discardValue
  Eq[Quantity]       |> discardValue
  Eq[Position]       |> discardValue

  // val Right((instruments, trades, folios, transactions, metas)) = for {
  // val tables = for {
  //   instruments  <- keyValueStore[IO] at "instruments.csv" ofChainAddressed Instrument
  //   trades       <- valueStore[IO] at "trades.csv" ofContentAddressed Trade
  //   folios       <- keyValueStore[IO] at "folios.csv" ofChainAddressed Folio
  //   transactions <- valueStore[IO] at "transactions.csv" ofChainAddressed Transaction
  //   metas        <- valueStore[IO] at "metas.csv" ofContentAddressed Meta
  // } yield (instruments, trades, folios, transactions, metas)
}
