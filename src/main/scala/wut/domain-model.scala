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

import cats.Eq

/**
  * Domain model class. The only domain that matters tbh.
  *
  * In a better example, these would be in their own file(s).
  */
final case class Kitteh(name: String, age: Int, color: String)
object Kitteh {
  implicit val printable = new Printable[Kitteh] {
    override def format(k: Kitteh) = {
      import k._
      s"OH HAI ${name} DESU HAZ ${age} YAERZ AM ${color} K THX BYE"
    }
  }
}

/** Distinguished cats! */
object Kats {
  val maru = Kitteh(name = "Maru", color = "Scottish Fold", age = 9)
  val ara  = Kitteh("Ara", 8, "Tuxedo")
}

final case class Box[A](value: A)
object Box {
  implicit def eq[A: Eq] = Eq.fromUniversalEquals[Box[A]]

  implicit def boxPrintable[A: Printable] = Printable[String].contramap { (b: Box[A]) =>
    b.toString
  }
  implicit def boxCodec[A: Codec]: Codec[Box[A]] = Codec[A] imap (Box[A], _.value)
}
