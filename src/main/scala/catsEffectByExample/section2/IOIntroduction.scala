package catsEffectByExample.section2

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {
  // IO
  val firstIO: IO[Int] = IO.pure(45) // arg that shouldn't have side effects
  val aDelayedIO: IO[Int] = IO.delay({
    println("I am producing an integer")
    56
  })

  val aDelayedIO_v2: IO[Int] = IO { // apply == delay
    println("I am producing an integer")
    56
  }
  val modifiedIO = firstIO.map(_ * 2)

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedIO = (firstIO, modifiedIO).mapN(_ + _)
  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)
  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // "platform" like special threadpool for IO runtime
    println(aDelayedIO.unsafeRunSync())
    // println(smallProgram().unsafeRunSync())
    // println(smallProgram_v2().unsafeRunSync())
    println(combinedIO.unsafeRunSync())
  }

}
