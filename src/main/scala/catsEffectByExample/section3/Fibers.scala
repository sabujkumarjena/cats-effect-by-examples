package catsEffectByExample.section3

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp, Outcome}
import scala.concurrent.duration._

object Fibers extends IOApp.Simple {
  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("scala")

  import catsEffectByExample.utils._

  def sameThreadIOs() = for {
    _ <- meaningOfLife.debug
    _ <- favLang.debug
  } yield ()

  // introduce the Fibre
  def createFibre: Fiber[IO, Throwable, String] =
    ??? // almost impossible to create fibers manually

//the fiber is not actually started, but the allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favLang.debug
  } yield ()

  // joing a fiber
  def runOnSomeotherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result
  /*
  IO[ResultType of fib.join]
  fib.join = OutCome[IO, Throwable, A]
   */
  /*
  possible outcomes:
  - success with an IO
  - failure with an exception
  - cancelled
   */

  val someIOOnAnotherThread = runOnSomeotherThread(meaningOfLife)

  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e)        => IO(0)
    case Canceled()        => IO(0)

  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException(" no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task =
      IO("starting").debug >> IO(Thread.sleep(1000)) >> IO("done").debug
    // onCancel is a "finalizer", allowing you to free up resources in case you get cancelled
    val taskWithCancellationHandler =
      task.onCancel(IO("I am being cancelled!").debug.void)
    for {
      // fib <- task.start // on a separate thread
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO(
        "cancelling"
      ).debug // running on the calling thread
      _ <- fib.cancel // from calling thread
      result <- fib.join // from calling thread
    } yield result
  }
  override def run: IO[Unit] = {
    // sameThreadIOs()
    // differentThreadIOs()
//    runOnSomeotherThread(meaningOfLife) // IO(Succeeded(IO(42)))
//      .debug.void
    // throwOnAnotherThread().debug.void
    testCancel().debug.void
  }
}
