package io.deftrade

import money._

import cats.implicits._
import cats.{ Foldable }
import cats.kernel.CommutativeGroup

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

  /**
    * Add convenience methods to qualifying "column" type constructors.
    *
    * TODO: let's see this evolve if it's useful.
    */
  implicit final class ColumnOps[C[_], V](val column: C[V]) extends AnyVal {

    /** */
    def total(implicit C: Foldable[C], V: Financial[V]): V = column fold V.additive

    /** */
    def total(implicit C: Foldable[C], V: CommutativeGroup[V]): V = column fold V
  }

  /** Add convenience methods to qualifying `Map`s.*/
  implicit final class MapOps[K, V](val m: Map[K, V]) extends AnyVal {

    /**
      * Works for any [[money.Financial]] amount or quantity,
      * or for [[money.Money]] of any [[money.Currency]].
      */
    def getWithZero(k: K)(implicit V: CommutativeGroup[V]): V = (m get k).fold(V.empty)(identity)

    /**
      * Totals values for any [[money.Financial]] amount or quantity,
      * or for [[money.Money]] of any [[money.Currency]].
      */
    def total(implicit V: CommutativeGroup[V]): V = m.map(_._2).fold(V.empty)(V.combine)

    /**
      * Filters a map by narrowing the scope of the keys contained.
      *
      * TODO: Revisit. This is awkward, but DRY and reflection free... needs to evolve.
      *
      * @param subKey Easily provided via an extractor.
      * @return A map containing those entries whose keys match a subclassing pattern.
      * @see [[keyval.DtEnum]]
      *
      */
    def collectKey[L <: K](subKey: K => Option[L]): Map[L, V] =
      m collect (Function unlift { case (k, v) => subKey(k) map (l => (l, v)) })
  }
}
