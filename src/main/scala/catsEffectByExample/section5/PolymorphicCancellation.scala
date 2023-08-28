package catsEffectByExample.section5

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import catsEffectByExample.utils.general.*

import scala.concurrent.duration.*

object PolymorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  // MonadCancel

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }
  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancelable[A](poll: Poll[F] => F[A]): F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // we can create values
  val molIO: IO[Int] = monadCancelIO.pure(42)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure(" once started, I can't go back ...")
      res <- monadCancelIO.pure(56)
    } yield res

  }

  import cats.syntax.flatMap.* // flatMap
  import cats.syntax.functor.* // map

  // can generalize code
  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] =
    mc.uncancelable { _ =>
      for {
        _ <- mc.pure(" once started, I can't go back ...")
        res <- mc.pure(56)
      } yield res
    }
  val mustCompute_v2 = mustComputeGeneral[IO, Throwable]

// allow cancellation listeners
  val mustComputeWithListener =
    mustCompute.onCancel(IO(" I'm being cancelled!").void)

  val mustComputeWithListener_v2 = monadCancelIO.onCancel(
    mustCompute,
    IO(" I'm being cancelled!").void
  ) // same

  // .onCancel as extension method
  import cats.effect.syntax.monadCancel.* // .onCancel

  // allow finalizers: guarantee, guarenteeCase
  val aComputationWithFinalizers = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.flatMap(a => IO(s"sucessful: $a").void)
    case Errored(e)    => IO(s"failed: $e").void
    case Canceled()    => IO("Cancelled").void
  }

  // bracket pattern is specific to MonadCancel
  val aComputationWithUsage = monadCancelIO.bracket(IO(42)) { value =>
    IO(s"using the mol: $value")
  } { value =>
    IO("releasing mol ...").void
  }

  /** Exercise - generalize a piece of code
    */
  // use this instead of IO.sleep
  def unsafeSleep[F[_], E](duration: FiniteDuration)(using
      mc: MonadCancel[F, E]
  ): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis)) // not semantic blocking

  def inputPassword[F[_], E](using mc: MonadCancel[F, E]): F[String] = for {
    _ <- mc.pure("Input password:").debug
    _ <- mc.pure("(typing password)").debug
    _ <- unsafeSleep[F, E](5.seconds)
    pw <- mc.pure("password")
  } yield pw

  def verifyPassword[F[_], E](
      pw: String
  )(using mc: MonadCancel[F, E]): F[Boolean] = for {
    _ <- mc.pure("verifying...").debug
    _ <- unsafeSleep[F, E](2.seconds)
    check <- mc.pure(pw == "password")
  } yield check

  def authFlow[F[_], E](using mc: MonadCancel[F, E]): F[Unit] =
    mc.uncancelable { poll =>
      for {
        pw <- poll(inputPassword).onCancel(
          mc.pure("Authentication timed out. Try again later.").debug.void
        ) // this is cancelable
        verified <- verifyPassword(pw) // this is NOT cancelable
        _ <-
          if (verified)
            mc.pure("Authentication successful.")
              .debug // this is NOT cancelable
          else mc.pure("Authentication failed.").debug
      } yield ()
    }

  val authProgram: IO[Unit] = for {
    authFib <- authFlow[IO, Throwable].start
    _ <- IO.sleep(3.seconds) >> IO(
      "Authentication timeout, attempting cancel..."
    ).debug >> authFib.cancel
    _ <- authFib.join
  } yield ()
  override def run: IO[Unit] = authProgram

}
