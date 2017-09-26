/*
 * Copyright 2017 47 Degrees, LLC. <http://www.47deg.com>
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

package wut

import cats.{ Eq, Monoid }

case class Order(totalCost: BigDecimal, quantity: Long)
object Order {

  import cats.instances.{ bigDecimal, long, option, tuple }
  import bigDecimal._, long._, option._, tuple._

  import cats.syntax.semigroup._

  implicit val eq = Eq.fromUniversalEquals[Order]
  implicit val monoid = new Monoid[Order] {

    def combine(a: Order, b: Order): Order = {

      /**
        * Objective here was to try to be as "generic" as possible.
        * I can start to see the motivation for shapless...
        */
      val l :: r :: Nil = List(a, b) map (Order unapply _)
      val Some(o)       = l |+| r
      (Order.apply _) tupled o
    }

    lazy val empty: Order = (Order.apply _) tupled Monoid[(BigDecimal, Long)].empty
  }
}
