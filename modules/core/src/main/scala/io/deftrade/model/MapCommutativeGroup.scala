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

import cats.kernel.CommutativeGroup
import cats.kernel.instances.MapMonoid

final class MapCommutativeGroup[K, V: CommutativeGroup]
    extends MapMonoid[K, V]
    with CommutativeGroup[Map[K, V]] {
  def inverse(a: Map[K, V]): Map[K, V] =
    a.foldLeft(Map.empty[K, V]) { case (my, (k, x)) => my.updated(k, CommutativeGroup inverse x) }
}
