package io.deftrade
package keyval

import spire.math.Integral
import spire.syntax.field._

/** */
final case class Fresh[K](init: K, next: K => K)

/**
  * Defines how to create a fresh '''globally unique''' key which
  * is suitable to be persisted.
  */
/** */
object Fresh {

  def apply[K: Fresh] = implicitly[Fresh[K]]

  /**
    * Equivalent to `autoincrement` or `serial` from SQL.
    *
    * TODO: PRNG version.
    */
  def zeroBasedIncr[K: Integral, P]: Fresh[OpaqueKey[K, P]] = {

    val K = Integral[K]; import K._

    Fresh(
      OpaqueKey(zero),
      key => OpaqueKey(key.value + one)
    )
  }
}
