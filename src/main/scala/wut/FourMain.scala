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

package wut

import scala.language.higherKinds

import cats.Functor

object FourMain {
  trait Monad[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    final override def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value){ a => pure[B](func(a))
  }
}
}
