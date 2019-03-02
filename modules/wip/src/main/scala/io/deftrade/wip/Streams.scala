import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.{io, text}
import java.nio.file.Paths
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

// Files.readAllLines(Paths.get(openChooser.getSelectedFile().getPath())))

// TODO: Should we eliminate the dependence on FS2 and just use IO?

object Converter extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    val inPath = "testdata/fahrenheit.txt"
    val outPath = "testdata/celsius.txt"

    def fahrenheitToCelsius(f: Double): Double = (f - 32.0) * (5.0/9.0)

    val blockingEC = ExecutionContext fromExecutorService (Executors newFixedThreadPool 2)

    io.file.readAll[IO](Paths get inPath, blockingEC, 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(s => !s.trim.isEmpty && !s.startsWith("//"))

      .map(line => fahrenheitToCelsius(line.toDouble).toString)
      // assuming FlatMap is OK here

      .intersperse("\n")
      .through(text.utf8Encode)
      .through(io.file writeAll Paths.get(outPath), blockingEC)
      .compile.drain
      .as(ExitCode.Success)
  }
}
