/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.deftrade
package model

import keyval._

import enumeratum.{ EnumEntry }

import cats.implicits._

/**
  * There are a finite enumeration of roles which [[Party]]s may take on with respect to
  * [[layers.Accounts.Account]]s.
  *
  * Contextual note: each `Role` is mapped to a [[Party]] via a [[layers.Accounts.Roster]].
  */
sealed trait Role extends EnumEntry

/**
  * Enumerated `Role`s.
  */
object Role extends DtEnum[Role] {

  /** */
  sealed trait Principal extends Role

  /**
    * That [[Party]] which is the market participant
    * responsible for establishing the [[layers.Accounts.Account]].
    *
    * Semantics for `Principal` are conditioned on the status of account, for examples:
    * - beneficial owner for an asset
    * - responsible party for a liability
    * - shareholder for equity
    * - business unit chief for revenue and expenses
    *
    * `Principal`s have authority to add or remove [[Agent]]s.
    *  A `Princple` is their own `Agent` unless otherwise specified.
    */
  case object Principal extends Principal {

    /**
      * A test for all `Role`s ''other than'' `Princple`.
      */
    def unapply(role: Role): Option[Principal] = role match {
      case p: Principal => p.some
      case _            => none
    }
  }

  /** */
  sealed trait NonPrincipal extends Role

  /** */
  object NonPrincipal {

    /**
      * A test for all `Role`s ''other than'' `Princple`.
      */
    def unapply(role: Role): Option[NonPrincipal] = role match {
      case Principal        => none
      case np: NonPrincipal => np.some
    }
  }

  /**
    * The primary delegate selected by a `Principal`.
    *
    * `Agents` have authortity to add or remove [[Manager]]s.
    * An `Agent` is their own `Manager` unless otherwise specified.
    */
  case object Agent extends NonPrincipal

  /**
    * The primary delegate selected by the `Agent`.
    * `Party`(s) with responsibility for, and authority over,
    * the disposition of assets in the `Account`. In particular, `Manager`s may initiate actions
    * which will result in `Transaction`s settling to the `Account`.
    *
    */
  case object Manager extends NonPrincipal

  /**
    * `Auditor`s are first class participants, with a package of rights and responsibilities.
    *
    * There are a finite enumeration of [[Role]]s.
    * Every `Role` is mapped to a [[Party]] via a [[layers.Accounts.Roster]]
    * which is situation and juristiction specific.
    *
    * Practically, what this means is that `Auditor`s will have a (possibly limited) view
    * into the state of the `Ledger`,
    * and (possibly) the ability to block the settlement of `Transaction`s to the `Ledger`
    * or even intitiate `Transaction`s.
    *
    * Actions of the `Auditor` may include the publishing of specific summaries of its views
    * into the `Ledger` to establish common knowledge for participants in `Ledger` `Transaction`s.
    *
    * N.B.: the `Auditor` need not be a regulatory entity; in particular this role might
    * be suited eg to a Risk Manager, operating in the context of a hedge fund.
    */
  case object Auditor extends NonPrincipal

  /** The `findValues` macro collects all `value`s in the order written. */
  lazy val values = findValues

  /** */
  lazy val nonPrincipals = values collect { case NonPrincipal(np) => np }
}
