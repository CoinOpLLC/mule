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

import keyval._, refinements._

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semiauto }

import eu.timepit.refined

import io.chrisdavenport.fuuid.FUUID

/** Models financial market participants.
  */
sealed abstract class Party {
  def name: Label
  def taxNo: Tax.No
  def contact: Contacts.Id
}

/** Players that are recognized by the system (ours).
  */
object Party {

  /**
    */
  def apply(name: Label, taxNo: Tax.No, contact: Contacts.Id): Party =
    taxNo match {
      case Tax.SSN(ssn) => NaturalPerson(name, ssn, contact)
      case Tax.EIN(ein) => LegalEntity(name, ein, contact)
    }

  implicit def partyEq: Eq[Party]     = { import auto.eq._; semiauto.eq }
  implicit def partyShow: Show[Party] = { import auto.show._; semiauto.show }
}

object Parties extends KeyValueStores.KV[FUUID, Party]

/** `NaturalPerson`s are `Party`s.
  */
sealed abstract case class NaturalPerson(
    name: Label,
    ssn: Tax.SSN,
    contact: Contacts.Id
) extends Party {

  /**
    */
  final def taxNo: Tax.No = { import refined.auto._; ssn }
}

/**
  */
object NaturalPerson {

  /**
    */
  def apply(name: Label, ssn: Tax.SSN, contact: Contacts.Id): NaturalPerson =
    new NaturalPerson(name, ssn, contact) {}

  import refined.cats._

  implicit def naturalPersonEq: Eq[NaturalPerson]     = { import auto.eq._; semiauto.eq }
  implicit def naturalPersonShow: Show[NaturalPerson] = { import auto.show._; semiauto.show }
}

/**
  */
object NaturalPersons extends KeyValueStores.KV[FUUID, NaturalPerson]

/**
  */
sealed abstract case class LegalEntity private (
    name: Label,
    ein: Tax.EIN,
    contact: Contacts.Id
) extends Party {

  /**
    */
  final def taxNo: Tax.No = { import refined.auto._; ein }
}

/**
  */
object LegalEntity {

  /**
    */
  def apply(name: Label, ein: Tax.EIN, contact: Contacts.Id): LegalEntity =
    new LegalEntity(name, ein, contact) {}

  import refined.cats._

  implicit def legalEntityEq: Eq[LegalEntity]     = { import auto.eq._; semiauto.eq }
  implicit def legalEntityShow: Show[LegalEntity] = { import auto.show._; semiauto.show }
}

object LegalEntities extends KeyValueStores.KV[FUUID, LegalEntity]
