package io.deftrade
package contracts

import cats.implicits._

/**
  * In what do we price things?
  *
  */
sealed trait Numéraire

/**
  */
object Numéraire {

  /** Consideration which is fully fungible, highly frangible, and self-pricing.
    *
    * Non-sealed extension point.
    */
  trait InCoin extends Numéraire

  /**
    */
  object InCoin {

    /**
      */
    def unapply(n: Numéraire): Option[InCoin] = n match {
      case c: InCoin => c.some
      case _         => none
    }
  }

  /** All other consideration. Non-sealed extension point.
    */
  trait InKind extends Numéraire

  /** Defined as ''whatever isn't `InCoin`''.
    *
    * Non fungability is achieved by default (`key`s are unique).
    */
  object InKind {

    /**
      */
    def unapply(n: Numéraire): Option[InKind] = n match {
      case k: InKind => k.some
      case _         => none
    }
  }
}
