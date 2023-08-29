package catsEffectByExample.section5

import cats.effect.{Concurrent, IO, IOApp, Temporal}

import scala.concurrent.duration.*
import catsEffectByExample.utils.general.*

object PolymorphicTimeouts extends IOApp.Simple {
  // Temporal - time-blocking effects

  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(
        time: FiniteDuration
    ): F[Unit] // semantically blocks this fiber for a specified time
  }

  //abilities: pure, map/flatMap, raiseError, uncancelable, start, ref/deferred, + sleep
  val temporalIO = Temporal[IO] // given Temporal[IO] in scope
  val chainOfEffects = IO("loading..").debug *> temporalIO.sleep(1.second) *> IO("Game ready!").debug
  val chainOfEffects_v2 = temporalIO.pure("loading..").debug *> temporalIO.sleep(1.second) *> temporalIO.pure("Game ready!").debug

  /**
   * Exercise: generalize the following piece
   */

  import cats.syntax.flatMap._

  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(using temporal: Temporal[F]): F[A] = {
    val timeoutEffect = temporal.sleep(duration)
    val result = temporal.race(fa, timeoutEffect)

    result.flatMap {
      case Left(v) => temporal.pure(v)
      case Right(_) => temporal.raiseError(new RuntimeException("Computation timed out."))
    }
  }
  override def run: IO[Unit] = {
   // IO.unit
   //chainOfEffects_v2.void
   timeout(IO(42), 3.seconds).debug.void
  }
}
