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

package wut

import scala.language.higherKinds

import cats.{ Eq, Eval, Functor, Id, Monad, Monoid }

import cats.syntax.eq._
import cats.syntax.option._

import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._

import cats.instances.int._
import cats.instances.string._
import cats.instances.tuple._

import cats.instances.option._
import cats.instances.vector._

object FourMain {
  trait MuhMonad[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    override def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(_ |> func |> pure)
  }

  val muhIdMonadInstance = new MuhMonad[Id] {
    override def pure[A](a: A): Id[A]                                 = a
    override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = value |> func
    override def map[A, B](value: Id[A])(func: A => B): Id[B]         = value |> func // |> pure
  }

  1.pure[Option] === 1.some |> assert

  for {
    l <- 1: Id[Int]
    s <- 20: Id[Int]
    d <- 12: Id[Int]
  } yield l * s * d === 240 |> assert

  val foo = Eval.now((0xfeedface * 0x2badbabe + 7) % 13)
  // println(foo)

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail => Eval defer foldRight(tail, fn(head, acc))(fn)
      case Nil          => Eval now acc
    }

  val n        = 1000
  val longList = List.fill(n)(1)
  foldRight(longList, 0) { (a, b) =>
    b + a
  }.value === n |> assert

  import cats.data.Writer
  // type Writer[W, A] = WriterT[Id, W, A]
  type Logged[A] = Writer[Vector[String], A]

  Monoid[String] |> discardValue

  val loggedInt = 618.pure[Logged]

  import cats.syntax.writer._

  val dogMsgs = Vector(
    "me: _Hello_",
    "dog: _Yes this is dog_",
    "me: *Woof*"
  )

  val loggedDogMsgsUnit: Writer[Vector[String], Unit] = dogMsgs.tell

  val loggedDogMsgsDouble: Writer[Vector[String], Double] = 6.18 writer dogMsgs

  val exNihiloNihiloFit: Writer[Vector[String], Unit] = () writer Vector.empty[String]

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  type LI = Logged[Int]

  // my version. which is lame.
  def factoRoyale(ln: LI): LI = ln flatMap { n =>
    slowly {
      (if (n === 0) 1.pure[Logged]
       else factoRoyale(ln map (_ - 1)) map (n * _)) mapBoth { (log, ans) =>
        (log :+ s"fact $n $ans", ans)
      }
    }
  }

  def faktoreal(n: Int): Logged[Int] =
    for {
      ans <- n match {
              case 0 => 1.pure[Logged]
              case _ => slowly { (n - 1 |> faktoreal) map (_ * n) }
            }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  def factorial(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly { (n - 1 |> factorial) map (_ * n) }
      _   <- Vector(s"fact $n $ans").tell
    } yield ans

  RaederMoanad |> discardValue
  TreeStuff    |> discardValue
  StaetMoanad  |> discardValue

}

object RaederMoanad {
  import cats.data.Reader

  case class Kitteh(naem: String, favFood: String)

  val kittehNaem: Reader[Kitteh, String] = Reader(k => k.naem)

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  implicit val dbEq = Eq.fromUniversalEquals[Db]

  type DbReader[R] = Reader[Db, R]

  val usernameReader: DbReader[Map[Int, String]] = Reader { db =>
    db.usernames
  }

  val passwordsReader: DbReader[Map[String, String]] = Reader { db =>
    db.passwords
  }

  def findUsername(userId: Int): DbReader[Option[String]] = Reader { db =>
    db.usernames.get(userId)
  }

  def findYoozername(userId: Int): DbReader[Option[String]] =
    for {
      uns <- usernameReader
    } yield uns get userId

  def checkPassword(
      username: String,
      password: String
  ): DbReader[Boolean] =
    for {
      pws <- passwordsReader
    } yield (pws get username) === password.some

  def checkLogin(
      userId: Int,
      password: String
  ): DbReader[Boolean] =
    for {
      uns <- usernameReader
      pws <- passwordsReader
    } yield (uns get userId flatMap (un => pws get un)) contains password

  val db = Db(
    Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    ),
    Map(
      "dade"  -> "zerocool",
      "kate"  -> "acidburn",
      "margo" -> "secret"
    )
  )
  db === Db(Map(1 -> "dade", 2 -> "kate", 3 -> "margo"), Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")) |> assert

  checkLogin(1, "zerocool").run(db) |> assert
// res8: cats.Id[Boolean] = true

  !checkLogin(4, "davinci").run(db) |> assert
// res9: cats.Id[Boolean] = false

}

object StaetMoanad {
  import cats.data.State

  val a = State[Int, String] { state =>
    (state, s"The state $state is the enemy.")
  }

  val inspektDemo = State.inspect[Int, String](state => s"The state $state is your friend.")

  inspektDemo.run(13).value === ((13, s"The state ${12 + 1} is your friend.")) |> assert

  type CalcState[A] = State[List[Int], A]

  val SadAssIntLexer = """(\d+)""".r

  type Op = (Int, Int) => Int
  def opState(op: Op): CalcState[Int] = State[List[Int], Int] {
    case top :: bis :: rest =>
      val res = op(top, bis)
      //println(res :: rest)
      (res :: rest, res)
    case _ => ???
  }
  def valState(ds: String): CalcState[Int] = State[List[Int], Int] {
    case stack =>
      val value = ds.toInt
      //println(value :: stack)
      (value :: stack, value)
  }
  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => opState(_ + _)
    case "*" => opState(_ * _)
    case ds  => valState(ds)
  }

  evalOne("42").runA(Nil).value === 42 |> assert

  def evalAll(syms: List[String]): CalcState[Int] =
    syms.foldLeft(0.pure[CalcState]) { (acc, head) =>
      acc flatMap { _ =>
        head |> evalOne
      }
    }

  val program = evalAll(List("1", "2", "+", "3", "*"))
  program.runA(Nil).value === 9 |> assert

  val notherProg = for {
    _   <- evalAll(List("1", "2", "+"))
    _   <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  notherProg.runA(Nil).value === 21 |> assert
}

object TreeStuff {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A)                        extends Tree[A]

  implicit def treeEq[T <: Tree[_]] = Eq.fromUniversalEquals[T]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  val (ll, rl) = (leaf(22), leaf(33))

  val tr = branch(ll, rl)
  tr === Branch(Leaf(22), Leaf(33)) |> assert

  implicit val treeMonad = new Monad[Tree] {
    def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] = tree match {
      case Branch(l, r) => Branch(flatMap(l)(fn), flatMap(r)(fn))
      case Leaf(a)      => a |> fn
    }

    def pure[A](a: A): Tree[A] = Leaf(a)

    // @annotation.tailrec
    def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] = fn(a) match {
      case Branch(l, r) =>
        Branch(
          flatMap(l) {
            case Left(a)  => tailRecM(a)(fn)
            case Right(b) => b |> pure
          },
          flatMap(r) {
            case Left(a)  => tailRecM(a)(fn)
            case Right(b) => b |> pure
          }
        )
      case Leaf(Left(a1)) => tailRecM(a1)(fn)
      case Leaf(Right(b)) => Leaf(b)
    }
  }

  val tr7 = tr map (_ + 7)

  val trtr = for {
    i <- tr
    j <- tr7
  } yield i + j

  trtr === Branch(Branch(Leaf(51), Leaf(62)), Branch(Leaf(62), Leaf(73))) |> assert
}
