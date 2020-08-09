:load target/fs2.scala
import cats.data.OneAnd
type NEFS[F[_], A] = OneAnd[Stream[F, *], A]
type NEFS[F[_], A] = OneAnd[Stream[F, *], A]
