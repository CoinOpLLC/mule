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
  implicit final class SweetColumn[C[_], V](val column: C[V]) {

    /** */
    def total(implicit C: Foldable[C], V: Financial[V]): V = column fold V.additive
  }

  /** Add convenience methods to qualifying `Map`s.*/
  implicit final class SweetMap[K, V](val m: Map[K, V]) extends AnyVal {

    /** works for any [[money.Financial]] amount or quantity */
    def getWithZero(k: K)(implicit V: Financial[V]): V = (m get k).fold(V.zero)(identity)

    /** works for [[money.Money]] */
    def getWithZero(k: K)(implicit V: CommutativeGroup[V]): V = (m get k).fold(V.empty)(identity)

    /** works for any [[money.Financial]] amount or quantity */
    def total(implicit V: Financial[V]): V = m.map(_._2).fold(V.zero)(V.plus)

    /** works for [[money.Money]] */
    def total(implicit V: CommutativeGroup[V]): V = m.map(_._2).fold(V.empty)(V.combine)

    /**
      * TODO: This is awkward, but DRY and reflection free... needs to evolve.
      */
    def collectKey[L <: K](subKey: K => Option[L]): Map[L, V] = {
      val of: ((K, V)) => Option[(L, V)] = {
        case (k, v) => subKey(k) map (l => (l, v))
      }
      val pf: PartialFunction[(K, V), (L, V)] = Function unlift of
      m collect pf
    }
  }

}
