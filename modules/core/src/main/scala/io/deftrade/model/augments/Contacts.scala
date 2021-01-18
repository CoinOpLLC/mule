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
package augments

import keyval._, refinements._

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semiauto }
import cats.effect.Sync

import eu.timepit.refined
import refined.api.{ Refined }
import refined.string.{ MatchesRegex, Url }

import refined.cats._

import io.circe.{ Decoder, Encoder }
import io.circe.refined._
import io.circe.generic.semiauto._

/** The House Contact class.
  */
trait Contacts {

  /**
    */
  sealed abstract case class Contact private (
      name: Contact.Name,
      address: Contact.USAddress,
      cell: Contact.USPhone,
      email: Contact.Email,
      url: Option[String Refined Url]
  ) {

    /** FIXME: `Accounts.contacts` dependency is unmapped */
    def naturalPerson[F[_]: Sync](ssn: Tax.Ssn): F[NaturalPerson] =
      ???
    // extract name from contact before persisting it

    /** FIXME: `Accounts.contacts` dependency is unmapped */
    def legalEntity[F[_]: Sync](ein: Tax.Ein): F[LegalEntity] =
      // extract name from contact before persisting it
      ???
  }

  implicit lazy val contactEq: Eq[Contact]     = { import auto.eq._; semiauto.eq }
  implicit lazy val contactShow: Show[Contact] = { import auto.show._; semiauto.show }

  /**
    */
  object Contact {

    /**
      */
    def apply(
        name: Contact.Name,
        address: Contact.USAddress,
        cell: Contact.USPhone,
        email: Contact.Email,
        url: Option[String Refined Url]
    ): Contact =
      new Contact(name, address, cell, email, url) {}

    implicit lazy val decoder: Decoder[Contact] = deriveDecoder
    implicit lazy val encoder: Encoder[Contact] = deriveEncoder

    /**
      */
    case class Name(
        first: Label,
        middle: Option[Label],
        last: Label
    )

    /**
      */
    object Name {

      implicit lazy val nameEq: Eq[Name]     = semiauto.eq
      implicit lazy val nameShow: Show[Name] = semiauto.show

      implicit lazy val decoder: Decoder[Name] = deriveDecoder
      implicit lazy val encoder: Encoder[Name] = deriveEncoder
    }

    /**
      */
    case class USAddress(
        street: Label,
        street2: Option[Label],
        city: Label,
        state: Alpha2,
        zip: USZip
    )

    /**
      */
    object USAddress {

      implicit lazy val usAddressEq: Eq[USAddress]     = semiauto.eq
      implicit lazy val usAddressShow: Show[USAddress] = semiauto.show

      implicit lazy val decoder: Decoder[USAddress] = deriveDecoder
      implicit lazy val encoder: Encoder[USAddress] = deriveEncoder
    }

    private def digits(n: Int) = s"""[0-9]{${n.toString}}"""

    /**
      */
    final val TenDigit = digits(10)

    /**
      */
    final type IsUSPhone = MatchesRegex[TenDigit.type]

    /**
      */
    final type USPhone = String Refined IsUSPhone

    /**
      */
    final val Zip = s"${digits(5)}|${digits(5 + 4)}"

    /**
      */
    final type IsUSZip = MatchesRegex[Zip.type]

    /**
      */
    final type USZip = String Refined IsUSZip

    /** TODO: [[http://www.regular-expressions.info/email.html investigate further]] */
    final val IsEmail =
      """[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"""

    /**
      */
    final type Email = String Refined MatchesRegex[IsEmail.type]
  }

  /**
    */
  object Contacts extends ValueStores.SADT[Contact]
}

/**
  */
object Contacts extends Contacts
