/*
 * Copyright 2017 CoinOp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
