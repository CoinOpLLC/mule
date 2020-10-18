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

/**
  * IRS Form 1065 Schedule L ontology: partnerships and LLC's taxed as partnerships.
  */
trait DefaultMetas { self: ModuleTypes with Ledger =>

  sealed abstract class Meta

  /** TODO: evolve to something less trival */
  final case class Memo(memo: Label) extends Meta

  implicit lazy val metaEq: Eq[Meta]     = { import auto.eq._; semi.eq }
  implicit lazy val metaShow: Show[Meta] = { import auto.show._; semi.show }

  /**
    */
  final override object Meta extends WithSADT[Meta] {

    import io.circe.generic.semiauto._

    implicit lazy val decoder: Decoder[Meta] = deriveDecoder
    implicit lazy val encoder: Encoder[Meta] = deriveEncoder
  }
}
