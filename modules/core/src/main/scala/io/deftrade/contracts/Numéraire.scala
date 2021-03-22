package io.deftrade
package contracts

import cats.implicits._
import cats.{ Defer, Monad }

/**
  * In what do we price things?
  *
  */
sealed trait Numéraire {

  /** Each instance governed by an explicit [[contracts.Contract contract]].
    */
  def contract[F[_]: Monad: Defer]: F[Contract]
}

/**
  */
object Numéraire {

  /** Non-sealed extension point for consideration which is
    * fully fungible, highly frangible, and self-pricing.
    */
  trait InCoin extends Numéraire { self =>

    /** As reified abstractions, all coins are immutably and identically governed.
      */
    final def contract[F[_]: Monad: Defer]: F[Contract] =
      unitOf(self).pure[F]
  }

  /** Non-sealed extension point for all other consideration: ''whatever isn't `InCoin`''.
    *
    * Governing contract must be specified by a sublcass.
    */
  trait InKind extends Numéraire
}
