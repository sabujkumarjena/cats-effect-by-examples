package catsEffectByExample.section2

import cats.effect.IO

import scala.util.{Failure, Success, Try}
object IOErrorHandling {
  // IO: pure, delay, defer
  // create failed effects

  val aFailedCompute: IO[Int] =
    IO.delay(throw new RuntimeException(" A FAILURE"))

  val aFailure: IO[Int] =
    IO.raiseError(new RuntimeException(" A Proper FAILURE"))

//handle exception
  val dealWithIt = aFailure.handleErrorWith { case _: RuntimeException =>
    IO.delay(println("I am still here"))
  // add more cases
  }

// turn into an Either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

//redeem: transform the failure and the success in one go

  val resultAsString: IO[String] =
    aFailure.redeem(ex => s"FAIL:$ex", value => s"SUCCESS: $value")

  // reedemWith

  val resultAsEffect: IO[String] =
    aFailure.redeemWith(ex => IO(s"FAIL:$ex"), value => IO(s"SUCCESS: $value"))

  /*
   * Exercises
   */
  // 1- construct potentially failed IOs from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] =
    option match {
      case Some(value) => IO(value)
      case None        => IO.raiseError(ifEmpty)
    }

  def try2IO[A](aTRy: Try[A]): IO[A] = aTRy match {
    case Success(value)     => IO(value)
    case Failure(exception) => IO.raiseError(exception)
  }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = anEither match {
    case Left(ex)     => IO.raiseError(ex)
    case Right(value) => IO(value)
  }

  /// 2- handleError, handleErrorWith
  def handleError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity)

  def handleErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, value => IO(value))

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    // aFailedCompute.unsafeRunSync()
    // aFailure.unsafeRunSync()
    //  dealWithIt.unsafeRunSync()
    println(resultAsString.unsafeRunSync())
    println(resultAsEffect.unsafeRunSync())
  }

}
