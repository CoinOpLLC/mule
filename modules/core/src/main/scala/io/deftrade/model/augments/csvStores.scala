package io.deftrade
package model
package augments

import keyval.{ csvKVS, csvVS, SADT }
import capital._
import model.layers._

// import cats.implicits._
import cats.{ Applicative, Eq, Foldable, Order, SemigroupK, Show }
import cats.kernel.CommutativeGroup
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.effect.{ ContextShift, Sync }

import keyval.CsvImplicits._
import io.chrisdavenport.cormorant
import cormorant.generic.semiauto._
import cormorant.refined._
import cormorant.implicits._

import eu.timepit.refined.cats._

trait csvStores { self: ModuleTypes with Ledger =>

  // implicit def X: ContextShift[IO] = ???

  final val dataDir: String = """target/data"""

  lazy val ledgers: Ledgers = {
    implicit def X: ContextShift[IO] = ???
    val Right(ls: Ledgers) =
      for {
        trades       <- csvVS[IO] at dataDir ofContentAddressed Trades
        folios       <- csvKVS[IO] at dataDir ofKeyChained Folios
        portfolios   <- csvVS[IO] at dataDir ofContentAddressed Portfolios
        transactions <- csvVS[IO] at dataDir ofChained Transactions
        // memos         <- csvVS[IO] at dataDir ofContentAddressed Metas
        confirmations <- csvKVS[IO] at dataDir ofKeyChained Confirmations
      } yield Ledgers(trades, folios, portfolios, transactions, ???, confirmations)
    ls
  }

  lazy val papers: Papers[IO] = {
    import capital.{ Forms, Instruments, Novations }
    implicit def X: ContextShift[IO] = ???
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    val Right(ret: Papers[IO]) =
      for {
        // instruments <- csvKVS[IO] at dataDir ofKeyChained Instruments
        // forms       <- csvKVS[IO] at dataDir ofKeyChained Forms
        novations <- csvVS[IO] at dataDir ofChained Novations
      } yield
        Papers(
          // instruments,
          // forms,
          ???,
          ???,
          novations
        )
    ret
  }
// lazy val accounts {
//
//   val Right((accounts, rosters, contacts)) = for {
//     accounts <- csvKVS[IO] at dataDir ofChained Accounts
//     rosters  <- csvKVS[IO] at dataDir ofChained Rosters
//     parties  <- csvKVS[IO] at dataDir ofChained Parties
//     contacts <- csvVS[IO] at dataDir ofContentAddressed Contacts
//   } yield (accounts, rosters, parties, contacts)
// }

}
