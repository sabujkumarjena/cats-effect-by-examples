package catsEffectByExample.section3

import cats.effect.{IO, IOApp}
import catsEffectByExample.utils.*

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.Try

object AsyncIOs extends IOApp.Simple {
  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool = Executors.newFixedThreadPool(8)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMOL(): Int = {
    Thread.sleep(1000)
    println(
      s"[${Thread.currentThread().getName}] computing MOL on some other thread"
    )
    45
  }
  def computeMOLEither(): Either[Throwable, Int] = Try {
    computeMOL()
  }.toEither

  def computeMolOnThreadPool(): Unit = {
    threadPool.execute(() => computeMOL())
  }

  // lift computation to an IO
  // async is a FFI (Foreign Function Interface)
  val asyncMolIO: IO[Int] = IO.async_ {
    cb => // CE thread blocks (semantically) until this cb is invoked (by some other thread)
      threadPool.execute { () => // computation not managed by CE
        val res = computeMOLEither()
        cb(res) // CE thread is notified with the result
      }
  }

  /** Exercise
    */

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_[A] { (cb: Callback[A]) =>
      Future(computation())(ec).onComplete(x => cb(x.toEither))(ec)
    }

  val asyncMolIO_v2 = asyncToIO(computeMOL)(ec)

  /** Exercise : lift an async computation as a Future, to an IO
    */

  lazy val molFuture: Future[Int] = Future {
    computeMOL()
  }(ec)

  val asyncMolIO_v3 = IO.async_ { cb =>
    molFuture.onComplete(x => cb(x.toEither))(ec)

  }

  def convertFutureToIO[A](future: => Future[A]): IO[A] =
    IO.async_ { (cb: Callback[A]) =>
      future.onComplete(x => cb(x.toEither))(ec)
    }

  val asyncMolIO_v4: IO[Int] = IO.fromFuture(IO(molFuture))

  /** a never-ending IO
    */
  val neverEndingIO: IO[Int] =
    IO.async_[Int](_ => ()) // no call back, no finish
  val neverEndingIO_v2: IO[Int] = IO.never
  /*
FULL ASYNC Call
   */
  def demoAsncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      /*
      finalizer in case computation gets cancelled.
      finalizers are of type IO[Unit]
      not specifying finalizer => Option[IO[Unit]]
      creating option is an effect => IO[Option[IO[Unit]]]
       */
      // return IO[Option[IO[Unit]]]
      IO {
        threadPool.execute { () =>
          val result = computeMOLEither()
          cb(result)
        }
      }.as(Some(IO("Cancelled").debug.void))
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("Cancelling...").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = {
    // asyncMolIO.debug >> IO(threadPool.shutdown())
    //asyncMolIO_v2.debug.void
    demoAsncCancellation().debug >> IO(threadPool.shutdown())
  }
}
