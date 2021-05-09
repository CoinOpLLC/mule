package io.deftrade
package contracts

import cats.{ Eval }

/**
  * In what do we price things?
  *
  */
sealed abstract class Numéraire {

  /** Each instance governed by an explicit [[contracts.Contract contract]].
    */
  val contract: Eval[Contract]
}

/**
  */
object Numéraire {

  /** Non-sealed extension point for consideration which is
    * fully fungible, highly frangible, and self-pricing.
    */
  abstract class InCoin extends Numéraire { self =>

    /** As reified abstractions, all coins are immutably and identically governed.
      */
    final val contract: Eval[Contract] =
      Eval now unitOf(self)
  }

  /** Non-sealed extension point for all other consideration: ''whatever isn't `InCoin`''.
    *
    * Governing contract must be specified by a sublcass.
    */
  abstract class InKind extends Numéraire
}
