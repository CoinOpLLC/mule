package io.deftrade
package model

import capital.layers._

/**
  * Capital instruments which we are competent to specify.
  */
package object capital
    extends PrimaryCapital  // nececssary
    with VanillaDerivatives // fun
    with Lending // as one does
// with Fx                 // WIP
// with Exotics            // for hedge funds
// with Ibor               // for banks
