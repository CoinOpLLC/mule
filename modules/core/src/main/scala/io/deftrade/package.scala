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

import cats.data.{ NonEmptyChain, Validated }

/** House rules. */
package object deftrade {

  /**
    * `Result` types
    */
  type Result[T]     = Either[Fail, T]
  type ResultV[T]    = Validated[Fail, T]
  type ResultVnec[T] = Validated[NonEmptyChain[Fail], T]

  /**
    * Informs wart remover that the value is intentionally discarded.
    * Useful for checking whether a thing compiles at all. Hard to miss on a code review.
    */
  val discardValue: Any => Unit = (_: Any) => ()

  /**
    * Bind a message to an assertion function.
    * Handy for development. If you write trading algos, this is basically "forever".
    */
  def assertOrElse(msg: String): Boolean => Unit = assert(_, msg)

  /**
    * I hear nice things about OCaml. So I stole something from it.
    */
  implicit final class PipeToFunction1[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }

  /**
    * Make `Seq` immutable. See:
    * - [this post](https://hseeberger.wordpress.com/2013/10/25/attention-seq-is-not-immutable/),
    * and also
    * - [these comments](https://disqus.com/home/discussion/heikosblog/attention_seq_is_not_immutable_heikos_blog/).
    */
  type Seq[+A] = scala.collection.immutable.Seq[A]
  val Seq = scala.collection.immutable.Seq

  /** IndexedSeq is also made immutable */
  type IndexedSeq[+A] = scala.collection.immutable.IndexedSeq[A]
  val IndexedSeq = scala.collection.immutable.IndexedSeq
}