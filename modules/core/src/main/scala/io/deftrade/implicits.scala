package io.deftrade

import cats.implicits._
import cats.Foldable

import scala.language.higherKinds

object implicits {

  /**
    * I hear nice things about OCaml.
    *
    * So I stole something from it. `|>` pairs nicely with [[discardValue]].
    */
  implicit final class PipeToFunction1[A](val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }

  /** Add convenience methods to qualifying "column" type constructors. */
  implicit final class SweetColumn[C[_], V](val column: C[V]) {

    import io.deftrade.money._

    def total(
        implicit C: Foldable[C],
        V: Financial[V]
    ): V = column.foldLeft(V.zero)(V.plus)

    def tally[CCY](
        implicit C: Foldable[C],
        V: Financial[V],
        CCY: Currency[CCY]
    ): Money[V, CCY] =
      total |> CCY.apply[V]
  }

  /** Add convenience methods to qualifying `Map`s.*/
  implicit final class SweetMap[K, V](val m: Map[K, V]) extends AnyVal {

    def getWithZero(k: K)(implicit V: Fractional[V]): V = (m get k).fold(V.zero)(identity)

    def sumValues(implicit V: Fractional[V]): V = m.map(_._2).fold(V.zero)(V.plus)
  }

}
