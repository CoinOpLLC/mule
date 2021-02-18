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
package model.layers

import keyval._, refinements._
import model.pillars.{ Metas, Tax }

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semiauto }

import eu.timepit.refined

import io.chrisdavenport.fuuid.FUUID

trait Person { module: ModuleTypes =>

  final type Contact = model.augments.Contact

  final lazy val Contacts = model.augments.Contacts

  /** Models financial market participants.
    */
  sealed trait Party {
    def name: Label
    def taxNo: Tax.No
    def contact: Contacts.Id
    def meta: Metas.Id
  }

  /** Players that are recognized by the system (ours).
    */
  object Party {

    /**
      */
    def naturalPerson(ssn: Tax.SSN): IO[NaturalPerson] =
      ???
    // extract name from contact before persisting it

    /**
      */
    def legalEntity(ein: Tax.EIN): IO[LegalEntity] =
      // extract name from contact before persisting it
      ???

    /**
      */
    def apply(name: Label, taxNo: Tax.No, contact: Contacts.Id, meta: Metas.Id): Party =
      taxNo match {
        case Tax.SSN(ssn) => NaturalPerson(name, ssn, contact, meta)
        case Tax.EIN(ein) => LegalEntity(name, ein, contact, meta)
      }

    import refined.cats._
    implicit def partyEq: Eq[Party]     = { import auto.eq._; semiauto.eq }
    implicit def partyShow: Show[Party] = { import auto.show._; semiauto.show }

    sealed abstract case class Value private (
        name: Label,
        taxNo: Tax.No,
        contact: Contacts.Id,
        meta: Metas.Id
    ) extends Party

    object Value {

      def apply(name: Label, taxNo: Tax.No, contact: Contacts.Id, meta: Metas.Id): Value =
        new Value(name, taxNo, contact, meta) {}

      implicit def partyEq: Eq[Party.Value]     = { import auto.eq._; semiauto.eq }
      implicit def partyShow: Show[Party.Value] = { import auto.show._; semiauto.show }
    }
  }

  /**
    */
  case object Parties
      extends KeyValueStores.SimpleCodec[FUUID, Party, Party.Value](
        party => { import party._; Party.Value(name, taxNo, contact, meta) },
        value => { import value._; Party.apply(name, taxNo, contact, meta) }
      )

  /** `NaturalPerson`s are `Party`s.
    */
  sealed abstract case class NaturalPerson private (
      name: Label,
      ssn: Tax.SSN,
      contact: Contacts.Id,
      meta: Metas.Id
  ) extends Party {

    final def taxNo: Tax.No = { import refined.auto._; ssn }
  }

  /**
    */
  object NaturalPerson {

    def apply(name: Label, ssn: Tax.SSN, contact: Contacts.Id, meta: Metas.Id): NaturalPerson =
      new NaturalPerson(name, ssn, contact, meta) {}

    import refined.cats._

    implicit lazy val naturalPersonEq: Eq[NaturalPerson]     = { import auto.eq._; semiauto.eq }
    implicit lazy val naturalPersonShow: Show[NaturalPerson] = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object NaturalPersons extends KeyValueStores.KV[FUUID, NaturalPerson]

  /**
    */
  sealed abstract case class LegalEntity private (
      name: Label,
      ein: Tax.EIN,
      contact: Contacts.Id,
      meta: Metas.Id
  ) extends Party {

    final def taxNo: Tax.No = { import refined.auto._; ein }
  }

  object LegalEntity {

    def apply(name: Label, ein: Tax.EIN, contact: Contacts.Id, meta: Metas.Id): LegalEntity =
      new LegalEntity(name, ein, contact, meta) {}

    import refined.cats._

    implicit lazy val legalEntityEq: Eq[LegalEntity]     = { import auto.eq._; semiauto.eq }
    implicit lazy val legalEntityShow: Show[LegalEntity] = { import auto.show._; semiauto.show }
  }

  /**
    */
  case object LegalEntities extends KeyValueStores.KV[FUUID, LegalEntity]

  /**
    */
  case class People(
      parties: Parties.KeyValueStore[IO],
      contacts: Contacts.ValueStore[IO]
  )
}
