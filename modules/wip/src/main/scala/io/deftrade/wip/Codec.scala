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
package wip

/**
  * Codec is a type class. Let's give it its own file, because we're feeling generous.
  */
trait Codec[A] { self =>
  def encode(a: A): String
  def decode(sa: String): Option[A]

  final def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    override def encode(b: B): String          = b  |> enc |> self.encode
    override def decode(sb: String): Option[B] = sb |> self.decode map dec
  }
}
object Codec {

  def apply[A: Codec]: Codec[A] = implicitly[Codec[A]]

  def encode[A: Codec](value: A): String         = Codec[A] encode value
  def decode[A: Codec](value: String): Option[A] = Codec[A] decode value

  implicit val int = new Codec[Int] {
    override def encode(value: Int): String         = value.toString
    override def decode(value: String): Option[Int] = scala.util.Try(value.toInt).toOption
  }
}
