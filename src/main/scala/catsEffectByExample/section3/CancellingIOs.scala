package catsEffectByExample.section3

import cats.effect.{IO, IOApp}
import catsEffectByExample.utils.*
import scala.concurrent.duration.*
object CancellingIOs extends IOApp.Simple {

  /*
    Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - Manual cancellation
   */

  val chainOfIOs: IO[Int] = IO("waiting").debug >> IO.canceled >> IO(42).debug

  // uncancelable
  // example: online store, payment processor
  // payment process must NOT be canceled

  val specialPaymentSystem = (
    IO("Payment running, don't cancel me..").debug >>
      IO.sleep(5.seconds) >>
      IO("Payment completed.").debug
  ).onCancel(IO("MEGA CANCEL OF DOOM !").debug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation..") >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment =
    IO.uncancelable(_ => specialPaymentSystem) // uncancelable "mask"
  val atomicPayment_v2 = specialPaymentSystem.uncancelable // same

  val cancellationOfDoom_v2 = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation..") >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
  The uncancelable API is more complex and more general. It takes a function from Poll[IO]
 to IO. In the example above, we are not using Poll instance. The Poll object can be used to mark sections
 within the returned effect which CAN BE CANCELED
   */

  /*
  Example: authentication service. Has two parts:
  - input password, can be cancelled, because otherwise we might block indefinitely on user input
  - verify passowrd, CANNOT be cancelled once it's started.
   */

  val inputPassowrd = IO("Input password:").debug >> IO(
    "(typing password)"
  ).debug >> IO.sleep(5.seconds) >> IO("actual password")
  val verifyPassword = (pw: String) =>
    IO("verifying....").debug >> IO.sleep(3.seconds) >> IO(
      pw == "actual password"
    )

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- inputPassowrd.onCancel(
        IO("Authentication timed out. Try again later").debug.void
      )
      verified <- verifyPassword(pw)
      _ <-
        if (verified) IO("Authentication successful").debug
        else IO("Authentication failed").debug
    } yield ()

  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(4.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  val authFlow_v2: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassowrd).onCancel(
        IO("Authentication timed out. Try again later").debug.void
      ) // this is cancelable
      verified <- verifyPassword(pw) // this is NOT cancelable
      _ <-
        if (verified) IO("Authentication successful").debug
        else IO("Authentication failed").debug // This is NOT cancelable
    } yield ()

  }

  val authProgram_v2 = for {
    authFib <- authFlow_v2.start
    _ <- IO.sleep(4.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  /*
  Uncancelable calls are MASKS which suppress cancellation.
  Poll calls are "gaps opened" in the uncancelable region.
   */

  /*
  Exercises
   */

  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).debug
  val uncancelableMol = IO.uncancelable(_ => cancelBeforeMol)

  // uncancelable will eliminate ALL cancel points
  // 2
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(4.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  // 3
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(
        IO("cancelable").debug >> IO.sleep(1.second) >> IO(
          "cancelable end"
        ).debug
      ) >>
        IO("uncancelable").debug >> IO.sleep((1.second)) >> IO(
          "uncancelable end"
        ).debug >>
        poll(IO(" second cancelable").debug >> IO.sleep(1.second) >> IO("second cancelable end").debug)
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(2200.millis) >> IO("CANCELING").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = {
    // chainOfIOs.void
    // cancellationOfDoom //cancellable
    // cancellationOfDoom_v2 // uncancelable
    // authFlow
    // authProgram
    // authProgram_v2
    // cancelBeforeMol.void
    // uncancelableMol.void
    // invincibleAuthProgram.void
    threeStepProgram()
  }

}
