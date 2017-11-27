package wip

import scala.language.higherKinds
import cats.{ Functor }

object FreeShit {

  /**
    */
  case class Done[F[_]: Functor, A](a: A)             extends Free[F, A]
  case class More[F[_]: Functor, A](k: F[Free[F, A]]) extends Free[F, A]

  class Free[F[_]: Functor, A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Done(a) => f(a)
      case More(k) => More(Functor[F].map(k)(_ flatMap f))
    }
    def map[B](f: A => B): Free[F, B] = flatMap(x => Done(f(x)))
  }

}

object KVSadt {

  /**
    * We leave an `A` shaped hole in our commands...
    * ...and by `A` we mean _continuation_.
    */
  sealed trait KVS[A]
  case class Put[A](key: String, value: String, a: A) extends KVS[A]
  case class Get[A](key: String, h: String => A)      extends KVS[A]
  case class Delete[A](key: String, a: A)             extends KVS[A]

  /** Don't forget the `Functor` instance */
  object KVS {
    implicit val kvsFunctor: Functor[KVS] = new Functor[KVS] {
      def map[A, B](a: KVS[A])(f: A => B) = a match {
        case Put(k, v, a) => Put(k, v, f(a))
        case Get(k, h)    => Get(k, x => f(h(x)))
        case Delete(k, a) => Delete(k, f(a))
      }
    }
  }
}

object KVSmachine {
  import KVSadt._
  import FreeShit._

  /** Primatives */
  def put(k: String, v: String): Free[KVS, Unit] = More(Put(k, v, Done(())))
  def get(k: String): Free[KVS, String]          = More(Get(k, v => Done(v)))
  def delete(k: String): Free[KVS, Unit]         = More(Delete(k, Done(())))

  /** compounded */
  def modify(k: String, f: String => String): Free[KVS, Unit] =
    for {
      v <- get(k)
      _ <- put(k, f(v))
    } yield ()

  def swap(k1: String, k2: String) =
    for {
      t1 <- get(k1)
      t2 <- get(k2)
      _  <- modify(k1, _ => t2)
      _  <- modify(k2, _ => t1)
    } yield ()

  @annotation.tailrec
  def run[A](prog: Free[KVS, A], data: Map[String, String]): Map[String, String] = prog match {
    case More(Put(k, v, a)) => run(a, data + (k -> v))
    case More(Get(k, f))    => run(f(data(k)), data)
    case More(Delete(k, a)) => run(a, data - k)
    case Done(_)            => data
  }

}

object KVSexample {

  import cats.syntax.eq._
  import cats.instances.map._
  import cats.instances.string._

  import KVSmachine._

  val incr: String => String = s => (s.toInt + 1).toString

  val skrew = for {
    _ <- swap("name.first", "name.last")
    _ <- modify("id", incr)
  } yield ()

  val rando = Map("name.first" -> "Hannover", "name.last" -> "Fist", "id" -> "6789")

  val skrewedRando = run(skrew, rando)

  Map("name.first" -> "Fist", "name.last" -> "Hannover", "id" -> "6790") === skrewedRando |> assert
}
