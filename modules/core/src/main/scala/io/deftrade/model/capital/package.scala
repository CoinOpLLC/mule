package io.deftrade
package model

import capital.layers._

/**
  * Capital instruments which we are competent to specify and model.
  */
package object capital
    extends PrimaryCapital  // nececssary
    with VanillaDerivatives // fun
    with Lending // as one does
// with Fx                 // WIP
// with Exotics            // primarily for hedge funds
// with Ibor               // primariy for banks
