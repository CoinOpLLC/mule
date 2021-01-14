package io.deftrade
package model
package augments

import keyval._, layers._
import refinements.Label

import cats.implicits._
import cats.{ Eq, Show }
import cats.derived.{ auto, semi }

import eu.timepit.refined
import refined.cats._

import io.circe.{ Decoder, Encoder };
import io.circe.refined._

/** House defaults.
  */
trait DefaultMetas { self: ModuleTypes with Ledger =>

  /** Root is unconstrained, enables disjunctive evolution.
    */
  sealed abstract class Meta

  /** TODO: evolve to something less trival
    */
  sealed abstract case class Memo private (memo: Label) extends Meta

  /**
    */
  object Memo { def apply(memo: Label): Memo = new Memo(memo) {} }

  implicit lazy val metaEq: Eq[Meta] = { import auto.eq._; semi.eq }
  implicit lazy val metaShow: Show[Meta] = { import auto.show._; semi.show }

  /**
    */
  object Meta {

    import io.circe.generic.semiauto._

    implicit lazy val decoder: Decoder[Meta] = deriveDecoder
    implicit lazy val encoder: Encoder[Meta] = deriveEncoder
  }

  final override object Metas extends ValueStores.SADT[Meta]
}
