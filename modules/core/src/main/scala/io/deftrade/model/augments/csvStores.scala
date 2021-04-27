package io.deftrade
package model.augments

import keyval.csv
import csv.{ kvs, vs }
import model.layers._
import model.slices.{ Metas }
import model.slices.keys.IsUSIN

import cats.implicits._
// import cats.{ Order, Show }
// import cats.kernel.CommutativeGroup
import cats.data.{ NonEmptyList }
// import cats.effect.{ ContextShift, Sync }

import money._

import eu.timepit.refined
import refined.refineV
import refined.api.Validate
import refined.cats._

import io.chrisdavenport.cormorant
import cormorant._
import cormorant.implicits._
import cormorant.refined._
import cormorant.generic.semiauto._

sealed trait csvDomainSpecificImplicits extends csv.implicits {
  self: ModuleTypes with Paper =>

  /**
    */
  implicit def moneyGet[N: Financial, C: Currency]: Get[Mny[N, C]] =
    new Get[Mny[N, C]] {

      /**
        */
      def get(field: CSV.Field): Either[Error.DecodeFailure, Mny[N, C]] =
        Mny parse [N, C] field.x leftMap toDecodeFailure
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

  /**
    */
  implicit def instrumentKeyGet[IsP: Validate[String, *]]: Get[ContractKey[IsP]] =
    new Get[ContractKey[IsP]] {
      def get(field: CSV.Field): Either[Error.DecodeFailure, ContractKey[IsP]] =
        refineV[IsP](field.x) map ContractKey.apply leftMap { reason =>
          Error.DecodeFailure(NonEmptyList one reason)
        }
    }

  /**
    */
  implicit def instrumentKeyPut[IsP: Validate[String, *]]: Put[ContractKey[IsP]] =
    stringPut contramap (_.key.value)
}

/**
  */
trait csvStores extends csvDomainSpecificImplicits {

  // self: ModuleTypes with Person with Paper with Ledger with Accounts =>
  self: ModuleTypes with Person with Paper with Ledger =>

  final val dataDir: String = """target/data"""

  lazy val metas: Metas.ValueStore[IO] = {
    val Right(ret) =
      for {
        metas <- vs[IO] at dataDir ofContentAddressed Metas
      } yield metas
    ret
  }

  // lazy val people: People = {
  //   @SuppressWarnings(Array("org.wartremover.warts.Any"))
  //   val Right(ret) =
  //     for {
  //       parties  <- kvs[IO] at dataDir ofKeyChained Parties
  //       contacts <- vs[IO] at dataDir ofContentAddressed Contacts
  //     } yield People(parties, contacts)
  //   ret
  // }

  lazy val papers: Papers = {
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    val Right(ret) =
      for {
        instruments      <- kvs[IO] at dataDir ofKeyChained Instruments
        forms            <- vs[IO] at dataDir ofChained Forms
        instrumentsForms <- kvs[IO] at dataDir ofKeyChained InstrumentsForms
        novations        <- vs[IO] at dataDir ofChained Novations
      } yield Papers(instruments, forms, instrumentsForms, novations)
    ret
  }

  lazy val ledgers: Ledgers = {
    val Right(ls: Ledgers) =
      for {
        trades        <- vs[IO] at dataDir ofContentAddressed Trades
        folios        <- kvs[IO] at dataDir ofKeyChained Folios
        portfolios    <- vs[IO] at dataDir ofContentAddressed Portfolios
        transactions  <- vs[IO] at dataDir ofChained Transactions
        confirmations <- kvs[IO] at dataDir ofKeyChained Confirmations
      } yield Ledgers(trades, folios, portfolios, transactions, confirmations)
    ls
  }

  // lazy val accounts = {
  //   val Right(ret) = for {
  //     accounts <- kvs[IO] at dataDir ofKeyChained Accounts
  //     rosters  <- vs[IO] at dataDir ofChained Rosters
  //   } yield (accounts, rosters)
  //   ret
  // }
}
