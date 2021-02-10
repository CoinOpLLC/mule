package io.deftrade
package model
package augments

import keyval.{ csvKVS, csvVS, CsvImplicits, SADT }
import capital.{ Forms, Instruments, InstrumentsForms, Novations, Papers }
import model.layers._
import model.Metas

// import cats.implicits._
import cats.{ Applicative, Eq, Foldable, Order, SemigroupK, Show }
import cats.kernel.CommutativeGroup
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.effect.{ ContextShift, Sync }

import io.chrisdavenport.cormorant
import cormorant.generic.semiauto._
import cormorant.refined._
import cormorant.implicits._

import eu.timepit.refined.cats._

trait stdStores extends CsvImplicits { self: ModuleTypes with Ledger =>

  final val dataDir: String = """target/data"""

  lazy val metas: Metas.ValueStore[IO] = {
    val Right(ret: model.Metas.ValueStore[IO]) =
      for {
        metas <- csvVS[IO] at dataDir ofContentAddressed Metas
      } yield metas
    ret
  }

  lazy val people: People[IO] = {
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    val Right(ret: People[IO]) =
      for {
        // parties  <- csvKVS[IO] at dataDir ofKeyChained Parties
        contacts <- csvVS[IO] at dataDir ofContentAddressed Contacts
      } yield People(???, contacts)
    ret
  }

  lazy val papers: Papers[IO] = {
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    val Right(ret: Papers[IO]) =
      for {
        // instruments      <- csvKVS[IO] at dataDir ofKeyChained Instruments
        forms <- csvVS[IO] at dataDir ofChained Forms
        // instrumentsForms <- csvKVS[IO] at dataDir ofKeyChained InstrumentsForms
        novations <- csvVS[IO] at dataDir ofChained Novations
      } yield Papers(???, forms, ???, novations)
    ret
  }

  lazy val ledgers: Ledgers = {
    val Right(ls: Ledgers) =
      for {
        trades        <- csvVS[IO] at dataDir ofContentAddressed Trades
        folios        <- csvKVS[IO] at dataDir ofKeyChained Folios
        portfolios    <- csvVS[IO] at dataDir ofContentAddressed Portfolios
        transactions  <- csvVS[IO] at dataDir ofChained Transactions
        confirmations <- csvKVS[IO] at dataDir ofKeyChained Confirmations
      } yield Ledgers(trades, folios, portfolios, transactions, confirmations)
    ls
  }

  lazy val accounts = {
    val Right(ret) = for {
      accounts <- csvKVS[IO] at dataDir ofKeyChained Accounts
      rosters  <- csvVS[IO] at dataDir ofChained Rosters
    } yield (accounts, rosters)
    ret
  }
}
