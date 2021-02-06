package io.deftrade
package model

import keyval._
import refinements.{ Label, SHA }

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semiauto }

import eu.timepit.refined
import refined.api.Refined
import refined.string.{ Url }

import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

/** Root is unconstrained, enables disjunctive evolution.
  */
sealed abstract class Meta

/**
  */
sealed abstract case class Memo private (final val memo: Label) extends Meta

/**
  */
object Memo {
  def apply(memo: Label): Memo = new Memo(memo) {}
}

/**
  */
sealed abstract case class Reference private (
    final val display: Label,
    final val href: Reference.URL,
    final val sha: SHA
) extends Meta

/**
  */
object Reference {

  /**
    */
  type URL = String Refined Url

  /**
    */
  def apply(display: Label, href: URL, sha: SHA): Reference =
    new Reference(display, href, sha) {}
}

/**
  */
object Meta {

  import refined.cats._

  implicit lazy val metaEq: Eq[Meta]     = { import auto.eq._; semiauto.eq }
  implicit lazy val metaShow: Show[Meta] = { import auto.show._; semiauto.show }

  implicit lazy val decoder: Decoder[Meta] = { import io.circe.refined._; deriveDecoder }
  implicit lazy val encoder: Encoder[Meta] = { import io.circe.refined._; deriveEncoder }
}

case object Metas extends ValueStores.SADT[Meta]
