package io.deftrade
package model.augments

import keyval.csv
import csv.{ csvKVS, csvVS }
import model.layers._
import model.slices.Metas

import cats.implicits._
// import cats.{ Order, Show }
// import cats.kernel.CommutativeGroup
// import cats.data.{ NonEmptyList, NonEmptyMap }
// import cats.effect.{ ContextShift, Sync }

import eu.timepit.refined
import refined.cats._

import io.chrisdavenport.cormorant
import cormorant.implicits._
import cormorant.refined._
import cormorant.generic.semiauto._

sealed trait csvDomainSpecificImplicits extends csv.implicits {

  import cormorant._
  import money._

  /**
    */
  implicit def moneyGet[N: Financial, C: Currency]: Get[Mny[N, C]] =
    new Get[Mny[N, C]] {

      /**
        */
      def get(field: CSV.Field): Either[Error.DecodeFailure, Mny[N, C]] =
        Mny parse field.x leftMap toDecodeFailure
    }

  /**
    */
  implicit def moneyPut[N: Financial, C: Currency]: Put[Mny[N, C]] =
    stringPut contramap Mny.format[N, C]

  /**
    */
  implicit def financialGet[N](implicit N: Financial[N]): Get[N] =
    new Get[N] {

      def get(field: CSV.Field): Either[Error.DecodeFailure, N] =
        N parse field.x leftMap toDecodeFailure
    }

  /**
    */
  implicit def financialPut[N: Financial]: Put[N] =
    stringPut contramap (Financial[N] toString _)
}

/** The batteries we include.
  */
trait csvStores extends csvDomainSpecificImplicits {

  // self: ModuleTypes with Person with Paper with Ledger with Accounts =>
  self: ModuleTypes with Person with Paper with Ledger =>

  final val dataDir: String = """target/data"""

  lazy val metas: Metas.ValueStore[IO] = {
    val Right(ret) =
      for {
        metas <- csvVS[IO] at dataDir ofContentAddressed Metas
      } yield metas
    ret
  }

  // lazy val people: People = {
  //   @SuppressWarnings(Array("org.wartremover.warts.Any"))
  //   val Right(ret) =
  //     for {
  //       parties  <- csvKVS[IO] at dataDir ofKeyChained Parties
  //       contacts <- csvVS[IO] at dataDir ofContentAddressed Contacts
  //     } yield People(parties, contacts)
  //   ret
  // }

  lazy val papers: Papers = {
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    val Right(ret) =
      for {
        instruments      <- csvKVS[IO] at dataDir ofKeyChained Instruments
        forms            <- csvVS[IO] at dataDir ofChained Forms
        instrumentsForms <- csvKVS[IO] at dataDir ofKeyChained InstrumentsForms
        novations        <- csvVS[IO] at dataDir ofChained Novations
      } yield Papers(instruments, forms, instrumentsForms, novations)
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

  // lazy val accounts = {
  //   val Right(ret) = for {
  //     accounts <- csvKVS[IO] at dataDir ofKeyChained Accounts
  //     rosters  <- csvVS[IO] at dataDir ofChained Rosters
  //   } yield (accounts, rosters)
  //   ret
  // }
}
