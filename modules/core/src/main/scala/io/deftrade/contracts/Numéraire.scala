package io.deftrade
package contracts

import money.Currency
import model.capital.Instrument

import cats.implicits._

/**
  * In what do we price things?
  *
  */
sealed trait Numéraire

/**
  */
object Numéraire {

  /** non-sealed extension point
    */
  trait InCoin extends Numéraire

  /**
    */
  object InCoin {

    /**
      */
    def unapply(n: Numéraire): Option[InCoin] = n match {
      case Currency(c) => c.some
      case _           => none
    }
  }

  /** non-sealed extension point
    */
  trait InKind extends Numéraire

  /**
    */
  object InKind {

    /**
      */
    def unapply(n: Numéraire): Option[InKind] = n match {
      case instrument @ Instrument(_, _, _) => instrument.some
      case _                                => none
    }
  }
}
