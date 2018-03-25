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

import scala.language.implicitConversions

package io {
  package object deftrade extends Api /*with MyTime */ {

    /**
      * Civilized function invocation.
      */
    implicit def pipeToFunction1[A](a: A) = PipeToFunction1(a)

    def assertOrElse(msg: String): Boolean => Unit = assert(_, msg)

    def camelToSnake(name: String): String  = camelTo(name)("_")
    def camelToHyphen(name: String): String = camelTo(name)("-")
    def camelToDot(name: String): String    = camelTo(name)(".")
    def camelToWord(name: String): String   = camelTo(name)(" ")

  }

  package deftrade {

    final case class PipeToFunction1[A](val a: A) extends AnyVal {
      def |>[B](f: A => B): B = f(a)
      def p2f1[B](f: A => B): B = a |> f
    }

    final case class SweetString(val s: String) extends AnyVal {
      def noSpaces: String = s filterNot (" \n\r\t" contains _)
    }

  }
}
