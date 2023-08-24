package catsEffectByExample.section3

import cats.effect.{IO, IOApp}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.*

object Resources extends IOApp.Simple {
  import catsEffectByExample.utils.*
  // use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open(): IO[String] = IO(s"Opening connection to $url").debug
    def close(): IO[String] = IO(s"Closing connection to $url").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("google.com")).open() >> IO
      .sleep(
        (Int.MaxValue).seconds
      )
      .start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield () // problem: leaking resources

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("google.com"))
    fib <- conn.open() >> IO
      .sleep(
        (Int.MaxValue).seconds
      )
      .onCancel(conn.close().void)
      .start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // bracket pattern: someIO.bracket(useResourceCb)(releaseResourceCb)
  // bracket is equivalent to try-catches (pure FP)
  val bracketFetchUrl = IO(new Connection("google.com"))
    .bracket(conn => conn.open() >> IO.sleep(Int.MaxValue.seconds))(conn =>
      conn.close().void
    )
  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.seconds) >> fib.cancel
  } yield ()

  /** Excersize : read the file withe bracket pattern
    *   - open a scanner
    *   - read the file line by line, every 100 millis -close the scanner -if
    *     cancelled/throws error, close the scanner
    */

  def openFileScanner(path: String): IO[Scanner] = IO(
    new Scanner(new FileReader(new File(path)))
  )


  def bracketReadFile(path: String): IO[Unit] = for {
    fib <- (openFileScanner(path)
      .bracket(scanner =>
        (IO(scanner.next()).debug >> IO.sleep(100.millis)).foreverM
      )(scanner => IO(scanner.close()).void))
      .start
    _ <- IO.sleep(5.seconds) >> fib.cancel
  } yield ()

  override def run: IO[Unit] = {
    // asyncFetchUrl.void
    // correctAsyncFetchUrl.void
    // bracketProgram
    bracketReadFile(
      "/Users/sabuj/Downloads/cats-effect-by-examples/src/main/scala/catsEffectByExample/section3/Resources.scala"
    )
  }

}
