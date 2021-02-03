package io.deftrade
package model
package augments

import keyval.{ keyValueStore, valueStore }
import capital._
import model.layers._

// import cats.implicits._
import cats.{ Applicative, Eq, Foldable, Order, SemigroupK, Show }
import cats.kernel.CommutativeGroup
import cats.data.{ NonEmptyList, NonEmptyMap }
import cats.effect.{ ContextShift, IO, Sync }

import eu.timepit.refined.cats._

trait csvStores {
  self: ModuleTypes with Ledger =>

  // implicit def X: ContextShift[IO] = ???

  lazy val ledgers: Ledgers = {
    // val Right(tables: Ledgers) =
    // for {
    //   trades        <- valueStore[IO] at "target/trades.csv" ofContentAddressed Trades
    //   folios        <- keyValueStore[IO] at "target/trades.csv" ofKeyChained Folios
    //   portfolios    <- valueStore[IO] at "target/trades.csv" ofContentAddressed Portfolios
    //   transactions  <- valueStore[IO] at "target/trades.csv" ofChainAddressed Transactions
    //   memos         <- valueStore[IO] at "target/trades.csv" ofContentAddressed Metas
    //   confirmations <- keyValueStore[IO] at "target/trades.csv" ofKeyChained Confirmations
    // } yield Ledgers(trades, folios, portfolios, transactions, memos, confirmations)
    // tables
    ???
  }

  lazy val papers: Papers[IO] = {
    ???
  }
  // object papers extends capital.Papers[IO] {
  //   import capital.{ Forms, Instruments, Novations }
  //   def instruments: Instruments.KeyValueStore[IO] = ???
  //   def forms: Forms.KeyValueStore[IO]             = ???
  //   def novations: Novations.ValueStore[IO]        = ???
  // }

// object Accounts {
//
//   val Right((accounts, rosters, contacts)) = for {
//     accounts <- keyValueStore[IO] at "accounts.csv" ofChained Account
//     rosters  <- keyValueStore[IO] at "rosters.csv" ofChained Roster
//     parties  <- keyValueStore[IO] at "parties.csv" ofChained Party
//     contacts <- valueStore[IO] at "contacts.csv" ofContentAddressed Contact
//   } yield (accounts, rosters, parties, contacts)
// }

}
