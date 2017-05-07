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

package object wut extends MyWay {}

/**
  * This is who we are, it's what we do.
  */
trait MyWay {

  trait Forceable {
    def force(): Unit = ()
  }

  /**
    * See [this post](https://hseeberger.wordpress.com/2013/10/25/attention-seq-is-not-immutable/), and also [these comments](https://disqus.com/home/discussion/heikosblog/attention_seq_is_not_immutable_heikos_blog/).
    */
  type Seq[+A] = scala.collection.immutable.Seq[A]
  val Seq = scala.collection.immutable.Seq

  /**
    * Civilized function invocation.
    */
  implicit class AnyPipeToFunction1[T](val v: T) {
    def |>[U](f: T ⇒ U): U = f(v)
  }
}
