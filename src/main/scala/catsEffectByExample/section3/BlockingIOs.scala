package catsEffectByExample.section3

import cats.effect.{IO, IOApp}
import catsEffectByExample.utils.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
object BlockingIOs extends IOApp.Simple {
  val someSleeps = for {
    _ <- IO.sleep(1.second).debug //SEMANTIC BLOCKING
    _ <- IO.sleep(1.second).debug
  } yield ()
  //really blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    44
  } // will evaluate on  thread from ANOTHER thread pool specific for blocking calls

  //yielding
  val iosOnManyThreads = for {
    _ <- IO("first").debug
    _ <- IO.cede // a signal to yield control over the thread
    _ <- IO("second").debug
    _ <- IO.cede
    _ <- IO("third").debug
  } yield()

  val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val thousandCedes = ( 1 to 100).map(IO.pure). reduce(_.debug >> IO.cede >> IO.sleep(100.millis) >> _.debug).evalOn(ec)
  override def run: IO[Unit] = {
    //aBlockingIO.void
   // iosOnManyThreads
   thousandCedes.void
  }
}
