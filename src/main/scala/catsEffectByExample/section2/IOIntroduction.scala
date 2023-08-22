package catsEffectByExample.section2

import cats.effect.IO

import scala.annotation.tailrec
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

  /** Exercises
    */
  // 1 - sequence two IOs and take the result of the LAST one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // andThen with by name call

  def sequenceTakeLast_v4[A, B](ioa: IO[A], iob: IO[B]): IO[B] = for {
    _ <- ioa
    b <- iob
  } yield b

  // 2 -  sequence two IOs and take the result of the FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    a <- ioa
    _ <- iob
  } yield a

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa <* iob

  // 3 - repeat an IO effect forever
  def forever[A](io: IO[A]): IO[A] = io.flatMap(_ => forever(io))

  def forever_v2[A](io: IO[A]): IO[A] = io >> forever_v2(io)

  def forever_v3[A](io: IO[A]): IO[A] =
    io *> forever_v3(io) // stack over flow due to eager evaluation

  def forever_v4[A](io: IO[A]): IO[A] = io.foreverM

  // 4 - conver an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] = ioa.as(value)

  // 5 -discard value an IO, just return UNIT
  def asUnit[A](ioa: IO[A]): IO[Unit] = convert(ioa, ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] = ioa.as(()) // discouraged
  def asUnit_v3[A](ioa: IO[A]): IO[Unit] = ioa.void // encouraged

  // 6- fix stack recursion
  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else
      for {
        lastNumber <- IO(n)
        prevSum <- sumIO(n - 1)
      } yield lastNumber + prevSum

  def sumIO_v2(n: Int): IO[Int] = { //eager evaluation, not good
    @tailrec
    def recR(n: Int, acc: Int): IO[Int] = {
      if (n <= 0) IO(acc)
      else
        recR(n - 1, n + acc)
    }
    recR(n, 0)
  }

  def sumIO_v3(n: Int): IO[Int] = // not tail recursive
    if (n == 0) IO.pure(0)
    else sumIO_v2(n - 1).map(_ + n)

  // 7- write a fibonacci IO that doesnot crash on recursion

  def fibonacci(n: Int): IO[BigInt] =
    if (n <= 2) IO(n)
    else
      for {
        a <- IO.defer(fibonacci(n - 1)) //same as .delay(..).flatten
        b <- IO(fibonacci(n - 2)).flatten
      } yield a + b

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // "platform" like special threadpool for IO runtime
    println(aDelayedIO.unsafeRunSync())
    // println(smallProgram().unsafeRunSync())
    // println(smallProgram_v2().unsafeRunSync())
    println(combinedIO.unsafeRunSync())
    // forever(IO(println("for ever"))).unsafeRunSync()
    // println(sumIO(10000).unsafeRunSync())
    println(fibonacci(32).unsafeRunSync())
  }

}
