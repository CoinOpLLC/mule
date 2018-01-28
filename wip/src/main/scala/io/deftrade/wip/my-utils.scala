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

package io.deftrade
package wip

/**
  * We are of the opinion that `This is fine.`
  * (For a `utils` collection here.)
  */
import scala.language.higherKinds

import cats.{ Eq, Functor, Id, Monad }
import cats.syntax.all._

/**
  * This might be of general use.
  */
case class Db(usernames: Map[Int, String], passwords: Map[String, String])

object Db {

  import cats.data.Reader
  type DbReader[R] = Reader[Db, R]

  implicit lazy val eq = Eq.fromUniversalEquals[Db]

  lazy val usernameReader: DbReader[Map[Int, String]]     = Reader(_.usernames)
  lazy val passwordsReader: DbReader[Map[String, String]] = Reader(_.passwords)

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(_.usernames get userId)

  /**
    * Yes, this is an unususally phat interface. For instructional purposes only.
    */
  def findYoozername(userId: Int): DbReader[Option[String]] =
    for {
      uns <- usernameReader
    } yield uns get userId

  import cats.instances.string._
  import cats.instances.option._
  import cats.syntax.eq._

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    for { pws <- passwordsReader } yield pws get username contains password

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      ou <- findUsername(userId)
      ok <- ou.fold(false.pure[DbReader])(checkPassword(_, password))
    } yield ok
}

/**
  * Almost certainly of pedagogic interest only.
  */
sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A)                        extends Tree[A]

object Tree {

  implicit def eq[T <: Tree[_]] = Eq.fromUniversalEquals[T]

  implicit lazy val functor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(left map f, right map f)
      case Leaf(value)         => Leaf(f(value))
    }
  }

  implicit lazy val treeMonad = new Monad[Tree] {
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

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

}

/**
  * Of pedagogic interest only.
  */
trait MuhMonad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  /**
    * Making [[map]] `final` would preclude optimization. This could be desirable, or not.
    */
  override def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(_ |> func |> pure)
}
object MuhMonadInstances {
  implicit val id = new MuhMonad[Id] {
    override def pure[A](a: A): Id[A]                                 = a
    override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = value |> func

    /**
      * Note we don't need to override here; but for Id we can optimize...
      */
    override def map[A, B](value: Id[A])(func: A => B): Id[B] = value |> func // |> pure
  }
}

object LogosGeometry {
  import spire.math.{ Complex, Real }

  type Point = Complex[Real]

  case class Circle(center: Point, radius: Real)

  def spawn(c: Circle): Seq[Circle] =
    (Complex rootsOfUnity[Real] 6) map (root => c.copy(center = c.center + root))

    // (spawn(c0) flatMap spawn).size
    // res32: Int = 36
    //
    // scala> (spawn(c0) flatMap spawn).toSet.size
    // res33: Int = 19

}

object LogosNames {

  val coexist = "☥✡☤☬ॐ☯"

  val greeks = "αβγδεζηθικλμνξοπρστυφχψω" // n.b. only one sigma

  val blackDirections: Seq[Char] = "▶▲◀▼"
  val whiteDirections: Seq[Char] = "▷△◁▽"

  val fisheye                                 = '◉' // '\u25C9'
  val bullseye                                = '◎' // '\u25CE'
  val whiteSquareContainingBlackSmallSquare   = '▣' // '\u25A3'
  val whiteUpPointingTriangleWithDot          = '◬' // '\u25EC'
  val whiteDiamondContainingBlackSmallDiamond = '◈' // '\u25C8'

  val pentagons = List('\u2b1f', '\u2b20', '\u2B53', '\u2B54', '\u2BC2')


  val whiteCircle  = '○' // '\u25CB'
  val blackCircle  = '●' // '\u25CF'
  val blackSquare  = '■' // '\u25A0'
  val whiteSquare  = '□' // '\u25A1'
  val blackDiamond = '◆' // '\u25C6'
  val whiteDiamond = '◇' // '\u25C7'

  val largeWhiteCircle = '◯' // '\u25EF
  val largeBlackCircle = '⬤' // '\u2B24
  val whiteFlag        = '⚐' // '\u2690'
  val blackFlag        = '⚑' // '\u2691'
  val blackStar        = '★' // '\u2605'
  val whiteStar        = '☆' // '\u2606'
  val blackSunWithRays = '☀' // '\u2600'
  val whiteSunWithRays = '☼' // '\u263C'

  val earth   = '♁' // '\u2641'	Antimony
  val moon    = '☽' // '\u263D'	silver
  val mercury = '☿' // '\u263F'	mercury
  val venus   = '♀' // '\u2640'	copper
  val sun     = '☉' // '\u2609'	gold
  val mars    = '♂' // '\u2642'	iron
  val jupiter = '♃' // '\u2643'	Tin
  val saturn  = '♄' // '\u2644'	Lead
  val uranus  = '♅' // '\u2645'
  val neptune = '♆' // '\u2646'
  val pluto   = '♇' // '\u2647'

  val spheres = "♁☽☿♀☉♂♃♄"

  val aries       = '♈' // '\u2648'
  val taurus      = '♉' // '\u2649'
  val gemini      = '♊' // '\u264A'
  val cancer      = '♋' // '\u264B'
  val leo         = '♌' // '\u264C'
  val virgo       = '♍' // '\u264D'
  val libra       = '♎' // '\u264E'
  val scorpius    = '♏' // '\u264F'
  val sagittarius = '♐' // '\u2650'
  val capricorn   = '♑' // '\u2651'
  val aquarius    = '♒' // '\u2652'
  val pisces      = '♓' // '\u2653'

  val zodiac = "♈♉♊♋♌♍♎♏♐♑♒♓"

  val icYang        = '⚊' // '\u268A'
  val icYin         = '⚋' // '\u268B'
  val icGreaterYang = '⚌' // '\u268C'	&#9868;
  val icLesserYin   = '⚍' // '\u268D'	&#9869;
  val icLesserYang  = '⚎' // '\u268E'	&#9870;
  val icGreaterYin  = '⚏' // '\u268F'	&#9871;
  val icHeaven      = '☰' // '\u2630'
  val icLake        = '☱' // '\u2631'
  val icFire        = '☲' // '\u2632'
  val icThunder     = '☳' // '\u2633'
  val icWind        = '☴' // '\u2634'
  val icWater       = '☵' // '\u2635'
  val icMountain    = '☶' // '\u2636'
  val icEarth       = '☷' // '\u2637'
  val icHexagrams   = ('\u4DC0' to '\u4DFF').mkString
// ䷀䷁䷂䷃䷄䷅䷆䷇䷈䷉䷊䷋䷌䷍䷎䷏䷐䷑䷒䷓䷔䷕䷖䷗䷘䷙䷚䷛䷜䷝䷞䷟䷠䷡䷢䷣䷤䷥䷦䷧䷨䷩䷪䷫䷬䷭䷮䷯䷰䷱䷲䷳䷴䷵䷶䷷䷸䷹䷺䷻䷼䷽䷾䷿

  val lastQuarterMoon = '☾' // '\u263E'
  val atomSymbol      = '⚛' // '\u269B'	Nuclear installation
  val radioactiveSign = '☢' // '\u2622'	toxic hazard, nuclear fallout
  val biohazardSign   = '☣' // '\u2623'	disease, epidemic, pandemic
// http://unicode.org/charts/nameslist/n_2600.html
}

// Additional[edit]
// Other epithets included:
//
// chthonius – at the festival Athenia Chytri sacrifices are made to this visage of the god only.[86][87]
// cyllenius, born on Mount Kyllini
// epimelios, guardian of flocks[42]
// koinos[88]
// kriophoros, "ram-bearer"[89]
// ploutodotes, giver of wealth (as inventor of fire)[90]
// /ndw - note the connection with stropholous of hekate? also the liminal aspect...
// proopylaios, "before the gate", "guardian of the gate",[91] Pylaios, "doorkeeper"[92]
// strophaios, "standing at the door post"[56][93]
// Stropheus, "the socket in which the pivot of the door moves" (Kerényi in Edwardson) or "door-hinge". Protector of the door (that is the boundary), to the temple[54][94][95][96][97]
// patron of gymnasia[98]
