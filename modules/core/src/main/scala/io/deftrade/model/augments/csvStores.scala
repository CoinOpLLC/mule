package io.deftrade
package model.augments

import keyval.csv.{ kvs, vs }
import model.layers._
import model.slices.{ Metas }
import model.slices.keys.IsUSIN

import cats.implicits._
// import cats.{ Order, Show }
// import cats.kernel.CommutativeGroup
import cats.data.{ NonEmptyList }
// import cats.effect.{ Sync }

import money._

import eu.timepit.refined
import refined.refineV
import refined.api.Validate
import refined.cats._

import io.chrisdavenport.cormorant
import cormorant.{ CSV, Error, Get, Put }
import cormorant.implicits._
import cormorant.refined._
import cormorant.generic.semiauto._

sealed trait csvDomainSpecificImplicits extends keyval.csv.implicits {
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
}

/**
  */
trait csvStores extends csvDomainSpecificImplicits {

  // self: ModuleTypes with Person with Paper with Ledger with Accounts =>
  self: ModuleTypes with Person with Paper with Ledger with Accounts =>

  final val dataDir: String = """target/data"""

  lazy val metas: Metas.ValueStore[IO] = {
    val Right(ret) =
      for {
        metas <- vs[IO] at dataDir ofContentAddressed Metas
      } yield metas
    ret
  }

  lazy val people: People = {
    val Right(ret: People) = (
      kvs[IO] at dataDir ofKeyChained Parties,
      vs[IO] at dataDir ofContentAddressed Contacts,
    ) mapN People
    ret
  }

  lazy val papers: Papers = {
    val Right(ret: Papers) = (
      kvs[IO] at dataDir ofKeyChained Instruments,
      vs[IO] at dataDir ofChained Forms,
      kvs[IO] at dataDir ofKeyChained InstrumentsForms,
      vs[IO] at dataDir ofChained Novations,
    ) mapN Papers
    ret
  }

  lazy val ledgers: Ledgers = {
    val Right(ls: Ledgers) = (
      vs[IO] at dataDir ofContentAddressed Trades,
      kvs[IO] at dataDir ofKeyChained Folios,
      vs[IO] at dataDir ofContentAddressed Portfolios,
      vs[IO] at dataDir ofChained Transactions,
      kvs[IO] at dataDir ofKeyChained Confirmations,
    ) mapN Ledgers
    ls
  }

  lazy val participants: Participants = {
    val Right(ret: Participants) = (
      kvs[IO] at dataDir ofKeyChained Accounts,
      vs[IO] at dataDir ofChained Rosters,
    ) mapN Participants
    ret
  }
}
