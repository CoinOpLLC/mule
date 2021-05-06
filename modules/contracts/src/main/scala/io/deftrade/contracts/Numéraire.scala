package io.deftrade
package contracts

import cats.Id

/**
  * In what do we price things?
  *
  */
sealed abstract class Numéraire[+F[_]] {

  /** Each instance governed by an explicit [[contracts.Contract contract]].
    */
  def contract: F[Contract]
}

/**
  */
object Numéraire {

  /** Non-sealed extension point for consideration which is
    * fully fungible, highly frangible, and self-pricing.
    */
  abstract class InCoin extends Numéraire[Id] { self =>

    /** As reified abstractions, all coins are immutably and identically governed.
      */
    final def contract: Id[Contract] =
      unitOf[Id](self)
  }

  /** Non-sealed extension point for all other consideration: ''whatever isn't `InCoin`''.
    *
    * Governing contract must be specified by a sublcass.
    */
  abstract class InKind[F[_]] extends Numéraire[F]
}
