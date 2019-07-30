package io.deftrade

import cats.implicits._
import cats.data.Validated

import cats.data.{ NonEmptyChain, Validated }

import scala.util.Try

trait results {

  /**
    * `Result` type is invariant.
    */
  type Result[T]     = Either[Fail, T]
  type ResultV[T]    = Validated[Fail, T]
  type ResultVnec[T] = Validated[NonEmptyChain[Fail], T]

  def fail[T](message: String): Result[T] = Fail(message).asLeft
}

/** Impedence matching utilities, basically. */
object Result {

  /** */
  def safe[T](thunk: => T): Result[T] = apply(Try(thunk))

  /** */
  def apply[R](t: Try[R]): Result[R] = t.toEither leftMap throw2fail

  /** */
  def apply[R](o: Option[R]): Result[R] = o.fold(fail[R]("not found"))(_.asRight)

  /** */
  def fail[T](message: String): Result[T] = Fail(message).asLeft

  /** Made trivial because `Fail <:< Throwable`. */
  def toTry[T](result: Result[T]): Try[T] = result.toTry

  /** Formats a general `message` from the argument, and records that argument as the `cause`. */
  lazy val throw2fail: Throwable => Fail = x => Fail(s"${x.getClass}: ${x.getMessage}", x)

  /** */
  val Ok: Result[Unit] = safe(())

  /** */
  val Nope: Result[Nothing] = fail[Nothing]("Nope.")

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

  import Result.throw2fail

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
  * Subclassing `Throwable` for the `Fail` type is "handy"; eg conversion to `Try`
  * becommes trivial.
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
  def apply(message: String): Fail                   = new Fail(message, none)       {}
  def apply(message: String, cause: Throwable): Fail = new Fail(message, cause.some) {}
}
