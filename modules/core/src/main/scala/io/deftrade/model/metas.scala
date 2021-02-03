package io.deftrade
package model

import keyval._
import refinements.Label

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semiauto }

import eu.timepit.refined

import io.circe.{ Decoder, Encoder };

/** Root is unconstrained, enables disjunctive evolution.
  */
sealed abstract class Meta

/** TODO: evolve to something less trival
  */
sealed abstract case class Memo private (memo: Label) extends Meta

/**
  */
object Memo { def apply(memo: Label): Memo = new Memo(memo) {} }

/**
  */
object Meta {

  import refined.cats._

  implicit lazy val metaEq: Eq[Meta]     = { import auto.eq._; semiauto.eq }
  implicit lazy val metaShow: Show[Meta] = { import auto.show._; semiauto.show }

  import io.circe.refined._
  import io.circe.generic.semiauto._

  implicit lazy val decoder: Decoder[Meta] = deriveDecoder
  implicit lazy val encoder: Encoder[Meta] = deriveEncoder
}

case object Metas extends ValueStores.SADT[Meta]
