package catsEffectByExample.section2

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {

  // IOs are usually sequential
  val io1 = IO(s"[${Thread.currentThread().getName}]io1")
  val io2 = IO(s"[${Thread.currentThread().getName}]io2")
  val composedIO = for {
    i1 <- io1
    i2 <- io2
  } yield s"$i1 and $i2 love cats"

  // debug extension methods
  import catsEffectByExample.utils._
  // mapN extension method
  import cats.syntax.apply._
  val meaningOfLife: IO[Int] = IO(42)
  val favLang: IO[String] = IO("Scala")

  val goalOfLife = (meaningOfLife.debug, favLang.debug).mapN((num, string) =>
    s"my goal in life is $num and $string"
  )

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)
  import cats.effect.implicits._
  val goalOfLifeParallel = (parIO1, parIO2).mapN((num, string) =>
    s"my goal in life is $num and $string"
  )
  // turn back to sequential
  val goalOfLife_v2 = Parallel[IO].sequential(goalOfLifeParallel)

  // shorthand:
  import cats.syntax.parallel._

  val goalOfLife_v3 =
    (meaningOfLife.debug, favLang.debug).parMapN((num, string) =>
      s"my goal in life is $num and $string"
    )

  // regarding failure
  val aFailure: IO[String] =
    IO.raiseError(new RuntimeException("I can't do this"))

  // compose success + failure
  val parallelWithFailure = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)

  // compose failure + failure

  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException(" Another Failure"))
  val parallelWithFailure_v2 = (anotherFailure.debug, aFailure.debug).parMapN(_ + _)
  //the first effect to fail gives to failure of the result
  override def run: IO[Unit] = {
    // composedIO.map(println)
   // goalOfLife_v3.debug.void
   // parallelWithFailure.debug.void
    parallelWithFailure_v2.debug.void

  }

}
