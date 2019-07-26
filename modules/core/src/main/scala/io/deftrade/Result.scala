package io.deftrade

import cats.implicits._
import scala.util.Try
import cats.data.Validated

object Result {

  private lazy val throw2fail: Throwable => Fail = x => Fail(s"${x.getClass}: ${x.getMessage}")

  def fail[T](msg: String): Result[T] = Fail(msg).asLeft

  val Ok: Result[Unit] = Result(())

  def apply[T](unsafe: => T): Result[T] =
    (Try apply unsafe).toEither.left map throw2fail

  def apply[R](o: Option[R]): Result[R] = o.fold(fail[R]("not found"))(_.asRight)

  def validated[T](unsafe: => T): ResultV[T] =
    Validated catchNonFatal unsafe leftMap throw2fail

  object implicits {

    implicit class OptionResult[R](val o: Option[R]) extends AnyVal {
      def asResult: Result[R] = apply(o)
    }
  }
}

sealed abstract class Fail extends Serializable
object Fail {
  private final case class Impl(msg: String, cause: Option[Throwable]) extends Fail
  def apply(msg: String): Fail                   = Impl(msg, None)
  def apply(msg: String, cause: Throwable): Fail = Impl(msg, Some(cause))
}
