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

package io

package object deftrade extends deftrade.Api /*with MyTime */ {

  /**
    * Civilized function invocation.
    */
  // implicit class PipeToFunction1[A](val a: A) extends AnyVal {
  //   def |>[B](f: A => B): B = f(a)
  //   def p2f1[B](f: A => B): B = a |> f
  // }

}

package deftrade {

  final case class SweetString(val s: String) extends AnyVal {
    def noSpaces: String = s filterNot (" \n\r\t" contains _)
  }

}
