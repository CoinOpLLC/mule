/*
 * Copyright 2017 Fairfax Technologies LLC
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

package wut;

import cats.Cartesian
import cats.instances.int._
import cats.instances.string._
import cats.instances.boolean._
import cats.instances.tuple._
import cats.instances.option._
import cats.syntax.option._
import cats.syntax.cartesian._
import cats.syntax.eq._

object SixMain {

  val p = Cartesian tuple3 (23.some, "oh hai".some, true.some)
  val q = 23.some |@| "oh hai".some |@| true.some

  p === q.tupled |> assert

  import scala.language.higherKinds
import cats.Monad

def product[M[_]: Monad, A, B](
  fa: M[A],
  fb: M[B]
): M[(A, B)] = ???

}
