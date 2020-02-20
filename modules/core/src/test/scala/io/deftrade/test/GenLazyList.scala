package org.scalacheck

object deftrade {

  /** Generates an infinite LazyList. */
  def infiniteLazyList[T](g: => Gen[T]): Gen[LazyList[T]] =
    Gen gen { (p, seed0) =>
      new Gen.R[LazyList[T]] {
        override val result =
          Option((LazyList unfold seed0)(s => Some(g.pureApply(p, s) -> s.next)))
        override val seed = seed0.next
      }
    }
}
