package feralcats

import cats.kernel.CommutativeGroup
import cats.kernel.instances.MapMonoid

class MapCommutativeGroup[K, V: CommutativeGroup] extends MapMonoid[K, V] with CommutativeGroup[Map[K, V]] {
  def inverse(a: Map[K, V]): Map[K, V] =
    a.foldLeft(Map.empty[K, V]) { case (my, (k, x)) => my updated (k, CommutativeGroup inverse x) }
}

object instances {
  implicit def catsFeralStdCommutativeGroup[K, V: CommutativeGroup]: CommutativeGroup[Map[K, V]] =
    new MapCommutativeGroup[K, V]
}
