package io.deftrade
package model
package layers

import money.{ Financial, Money }

/**
  * Module level abstract quantities and monetary amounts, which may be distinct types.
  *
  * Note both type params `MA` and `Q` are needed to deal with case
  * where [[MonetaryAmount]] and [[Quantity]]
  * are distinct types (e.g. [[scala.BigDecimal]] and [[scala.Double]], respectively.)
  *
  * Modules parameterized like this may create `Money[MA, C] <=> (MI, Q)` codecs via a table of
  * [[capital.Instrument]]s which function as stable, denominated currency (e.g. a bank account, or
  * a money market fund instrument.)
  *
  */
trait ModuleTypes {

  /** */
  type Quantity

  /** */
  implicit val Quantity: Financial[Quantity]

  /** */
  type MonetaryAmount

  /**  */
  implicit val MonetaryAmount: Financial[MonetaryAmount]

  /** */
  final type Mny[C] = Money[MonetaryAmount, C]
}

/** */
object ModuleTypes {

  /** */
  abstract class Aux[MA, Q]()(
      implicit val MonetaryAmount: Financial[MA],
      implicit val Quantity: Financial[Q]
  ) extends ModuleTypes {

    /** */
    final type MonetaryAmount = MA

    /** */
    final type Quantity = Q
  }
}
