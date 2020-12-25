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

  /**
    */
  sealed trait Principal extends Role

  /**
    * That [[Party]] which is the market participant
    * responsible for establishing the [[layers.Accounts.Account]].
    */
  case object Principal extends Principal {

    /**
      * A test for all `Role`s ''other than'' `Princple`.
      */
    def unapply(role: Role): Option[Principal] =
      role match {
        case p: Principal => p.some
        case _            => none
      }
  }

  @inline final def principal: Role = Principal

  /**
    */
  sealed trait NonPrincipal extends Role

  /**
    */
  object NonPrincipal {

    /**
      * Extractor for all `Role`s ''other than'' `Princple`.
      */
    def unapply(role: Role): Option[NonPrincipal] =
      role match {
        case Principal        => none
        case np: NonPrincipal => np.some
      }
  }

  /**
    * The primary delegate selected by a `Principal`.
    */
  case object Agent extends NonPrincipal

  /**
    * The primary delegate selected by the `Agent`.
    */
  case object Manager extends NonPrincipal

  /**
    * `Auditor`s are first class participants, with a package of rights and responsibilities.
    */
  case object Auditor extends NonPrincipal

  /** The `findValues` macro collects all `value`s in the order written. */
  lazy val values = findValues

  /**
    */
  lazy val nonPrincipals = (values collect { case NonPrincipal(np) => np }).toList
}
