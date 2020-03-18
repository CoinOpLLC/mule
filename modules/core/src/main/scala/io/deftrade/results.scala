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

import cats.implicits._

import cats.data.{ EitherT, NonEmptyChain, Validated }

import scala.util.Try

/** Package mixin. */
object results {

  /** */
  trait mixin {

    /** Fast-fail, invariant. */
    type Result[T] = Either[Fail, T]

    /** Applicative flavor. */
    type ResultV[T] = Validated[Fail, T]

    /** Monad transformer flavor. */
    type ResultT[F[_], T] = EitherT[F, Fail, T]

    /**
      * Choosing [[https://typelevel.org/cats/datatypes/chain.html Chain]] for our
      * monoidal [[Fail]] accumulator.
      */
    type ResultVnec[T] = Validated[NonEmptyChain[Fail], T]

    /** Formats a general `message` from the argument, and records that argument as the `cause`. */
    private[deftrade] lazy val throw2fail: Throwable => Fail =
      x => Fail(s"${x.getClass.toString}: ${x.getMessage}", x)
  }
}

/** Impedence matching utilities, basically. */
object Result {

  /** */
  def apply[R](o: Option[R]): Result[R] = o.fold(fail[R]("not found"))(_.asRight)

  /** */
  def apply[R](t: Try[R]): Result[R] = t.toEither leftMap throw2fail

  /** */
  def of[T](t: T): Result[T] = t.asRight

  /** And by safe we mean "will not `throw`" . */
  def safe[T](thunk: => T): Result[T] = Result(Try(thunk))

  /** A failure message, stylized at the type level. */
  def fail[T](message: String): Result[T] = Fail(message).asLeft

  /** */
  val Ok: Result[Unit] = safe(())

  /** */
  val Nope: Result[Nothing] = fail[Nothing]("Nope.")

  /** Trivial; for completeness. */
  def toOption[T](result: Result[T]): Option[T] = result.toOption

  /** Made trivial because `Fail <~< Throwable`. */
  def toTry[T](result: Result[T]): Try[T] = result.toTry

  /** */
  object implicits {

    implicit class OptionResult[R](val o: Option[R]) extends AnyVal {
      def asResult: Result[R] = apply(o)
    }

    implicit class TryResult[R](val t: Try[R]) extends AnyVal {
      def asResult: Result[R] = t.toEither leftMap throw2fail
    }
  }
}

/** */
object ResultV {

  /** */
  def apply[T](unsafe: => T): ResultV[T] =
    Validated catchNonFatal unsafe leftMap throw2fail
}

/**
  * Lightweight immutable Throwable used as a container for an error `message`
  *  (`String`) and, optionally, a `scala.util.control.NonFatal` underlying
  * `cause` (`Throwable`) of failure.
  *
  * Immutability is achieved by:
  *   - disabling suppressed `Throwable`s from being set, and by
  *   - disabling stack traces from being filled in after construction.
  *
  * Interoperability with [[scala.Option]] and [[scala.util.Try]] is provided.
  *
  * (Subclassing `Throwable` for the `Fail` type is "handy"; eg conversion to `Try`
  * becommes trivial.)
  *
  * About the name: current global usage feels consistent with both ''noun'' and ''verb'' for
  * this word. We exploit the resulting semantic flexibility.
  */
@SuppressWarnings(Array("org.wartremover.warts.Null")) // trustMeIKnowWhatImDoing.gif
sealed abstract case class Fail private (
    message: String,
    cause: Option[Throwable],
) extends Throwable(
      message,
      cause getOrElse null,             // `null` per `Throwable` ctor spec; it is what it is.
      /* enableSuppression  = */ false, // don't (bother to) report suppressed `Throwable`s
      /* writableStackTrace = */ false //  don't (bother to) fill in stack trace
    )

/** */
object Fail {

  def fromThrowable(s: String): Throwable => Fail = t => Fail(s, t)

  lazy val fromString: String => Fail = s => Fail(s)

  /** */
  def apply(message: String): Fail = new Fail(message, none) {}

  /** */
  def apply(message: String, cause: Throwable): Fail = new Fail(message, cause.some) {}

  /** */
  def unapply(fail: Fail): Option[(String, Option[Throwable])] = (fail.message, fail.cause).some
}
