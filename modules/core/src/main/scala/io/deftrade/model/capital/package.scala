package io.deftrade
package model

import keyval._

/**
  * Capital instruments which we are competent to specify and model.
  */
package object capital {

  /** */
  lazy final val Novations = ValueStore of Novation

  /** */
  lazy final val Instruments = KeyValueStore of Instrument

  /** */
  lazy final val Forms = KeyValueStore of Form
}
