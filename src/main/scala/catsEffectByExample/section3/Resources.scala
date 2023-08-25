package catsEffectByExample.section3

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}

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

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine)
      IO(scanner.nextLine()).debug >> IO.sleep(100.millis) >> readLineByLine(
        scanner
      )
    else IO.unit

  def bracketReadFile(path: String): IO[Unit] = for {
    fib <- (openFileScanner(path)
      .bracket(scanner => readLineByLine(scanner))(scanner =>
        IO(s"Closing the file at $path").debug >> IO(scanner.close()).void
      ))
      .start
    _ <- IO.sleep(15.seconds) >> fib.cancel
  } yield ()

  def bracketReadFile_v2(
      path: String
  ): IO[Unit] = // no need for fiber, bracket do it automatically
    openFileScanner(path)
      .bracket(scanner => readLineByLine(scanner))(scanner =>
        IO(s"Closing the file at $path").debug >> IO(scanner.close()).void
      )

  /** Resources
    */
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket { scanner =>
        // acuire a connection
        IO(new Connection(scanner.nextLine())).bracket { conn =>
          conn.open().debug >> IO.never
        }(conn => conn.close().void)
      }(scanner => IO("closing file").debug >> IO(scanner.close()))
  // nesting resources are tedious

  val connectionResource =
    Resource.make(IO(new Connection("google.com")))(conn => conn.close().void)

  // ... at a later part of your code
  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(5.seconds) >> fib.cancel
  } yield ()

  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string =>
    IO(s"using the string: $string").debug
  val releaseResource: String => IO[Unit] = string =>
    IO(s"finalising the string: $string").debug.void

  val usingResourceWithBracket =
    simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource =
    Resource.make(simpleResource)(releaseResource).use(usingResource)

  /** Exercise : read a test file with one line every 100 millis, using Resource
    */
  def resourceReadFile(
      path: String
  ): IO[Unit] = {
    val scannerResource: IO[Scanner] = openFileScanner(path)
    val releaseScanner: Scanner => IO[Unit] = scanner =>
      IO(s"Closing the file at $path").debug >> IO(scanner.close()).void
    val useScanner: Scanner => IO[Unit] = scanner => readLineByLine(scanner)
    Resource.make(scannerResource)(releaseScanner).use(useScanner)
  }

  def cancelReadFile(path: String) = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(10.seconds) >> fib.cancel
  } yield ()

  // nested resources
  def connFromConfResource(path: String) =
    Resource
      .make(IO("Openining file").debug >> openFileScanner(path))(scanner =>
        IO("closing file").debug >> IO(scanner.close())
      )
      .flatMap(scanner =>
        Resource.make(IO(new Connection(scanner.nextLine())))(conn =>
          conn.close().void
        )
      )

  def connFromConfResourceClean(path: String) = for {
    scanner <- Resource
      .make(IO("Openining file").debug >> openFileScanner(path))(scanner =>
        IO("closing file").debug >> IO(scanner.close())
      )
    conn <-
      Resource.make(IO(new Connection(scanner.nextLine())))(conn =>
        conn.close().void
      )
  } yield conn

  val openConnection = connFromConfResourceClean(
    "/Users/sabuj/Downloads/cats-effect-by-examples/src/main/scala/catsEffectByExample/section3/Resources.scala"
  ).use(conn => conn.open() >> IO.never)

  val canceledConnection = for {
    fib <- openConnection.start
    _ <- IO.sleep(3.seconds) >> IO("cancelling").debug >> fib.cancel
  } yield ()

  // finalizer to regular IOs
  val ioWithFinalizer =
    IO("some resource").debug.guarantee(IO("freeing resource").debug.void)

  val ioWithFinalizer_v2 = IO("some resources").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap((result => IO(s"releasing resource: $result").debug)).void
    case Errored(e) => IO("nothing to release").debug.void
    case Canceled() => IO("resource got canceled, releasing what's left").debug.void
  }
  override def run: IO[Unit] = {

    // asyncFetchUrl.void
    // correctAsyncFetchUrl.void
    // bracketProgram
//    bracketReadFile_v2(
//      "/Users/sabuj/Downloads/cats-effect-by-examples/src/main/scala/catsEffectByExample/section3/Resources.scala"
//    )

    // resourceFetchUrl

    // resourceReadFile( "/Users/sabuj/Downloads/cats-effect-by-examples/src/main/scala/catsEffectByExample/section3/Resources.scala")
//    cancelReadFile(
//      "/Users/sabuj/Downloads/cats-effect-by-examples/src/main/scala/catsEffectByExample/section3/Resources.scala"
//    )

    // openConnection.void

   // canceledConnection
   ioWithFinalizer.void
  }

}
