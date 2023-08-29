package catsEffectByExample.section5

import cats.effect.{
  Concurrent,
  Fiber,
  FiberIO,
  IO,
  IOApp,
  MonadCancel,
  OutcomeIO,
  Ref
}
import cats.effect.kernel.{Deferred, Outcome, Spawn}

object PolymorphicCoordination extends IOApp.Simple {
  // Concurrent - Ref + Deferred for ANY effect type

  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO = Concurrent[IO] // given instance of Concurrent[IO]
  val asyncDeferred = Deferred[IO, Int] // given Concurrent[IO] in scope
  val aDeferred_v2 = concurrentIO.deferred[Int]
  val aRef = concurrentIO.ref(42)

  // capabilities: pure, map/flatMap, raiseError, uncancelable, start (fibers), + ref/deferred

  import scala.concurrent.duration.*
  import catsEffectByExample.utils.general.*

  def riceCooker(): IO[Unit] = {
    def riceReadyNotification(signal: Deferred[IO, Unit]) = for {
      _ <- IO("Rice cooking on some other fiber, waiting..").debug
      _ <- signal.get
      _ <- IO(" RICE READY!!").debug
    } yield ()

    def tickingClock(
        ticks: Ref[IO, Int],
        signal: Deferred[IO, Unit]
    ): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      count <- ticks.updateAndGet(_ + 1)
      _ <- IO(count).debug
      _ <- if (count >= 10) signal.complete(()) else tickingClock(ticks, signal)
    } yield ()

    for {
      initialCount <- Ref[IO].of(0)
      signal <- Deferred[IO, Unit]
      riceReadyNotificationFib <- riceReadyNotification(signal).start
      tickingClockFib <- tickingClock(initialCount, signal).start
      _ <- riceReadyNotificationFib.join
      _ <- tickingClockFib.join
    } yield ()
  }
  import cats.syntax.flatMap.* // flatMap
  import cats.syntax.functor.* // map
  import cats.effect.syntax.spawn.* // start extension method
  // import cats.syntax.*

  // added here explicitly due to a Scala 3 bug that we discovered during lesson recording
  def unsafeSleepDupe[F[_], E](duration: FiniteDuration)(using
      mc: MonadCancel[F, E]
  ): F[Unit] =
    mc.pure(Thread.sleep(duration.toMillis))

  def polymorphicRiceCooker[F[_]](using concurrent: Concurrent[F]): F[Unit] = {
    def riceReadyNotification(signal: Deferred[F, Unit]) = for {
      _ <- concurrent.pure("Rice cooking on some other fiber, waiting..").debug
      _ <- signal.get
      _ <- concurrent.pure(" RICE READY!!").debug
    } yield ()

    def tickingClock(
        ticks: Ref[F, Int],
        signal: Deferred[F, Unit]
    ): F[Unit] = for {
      _ <- unsafeSleepDupe[F, Throwable](1.second)
      count <- ticks.updateAndGet(_ + 1)
      _ <- concurrent.pure(count).debug
      _ <-
        if (count >= 10) signal.complete(()).void
        else tickingClock(ticks, signal)
    } yield ()

    for {
      initialCount <- concurrent.ref(0)
      signal <- concurrent.deferred[Unit]
      riceReadyNotificationFib <- riceReadyNotification(signal).start
      tickingClockFib <- tickingClock(initialCount, signal).start
      _ <- riceReadyNotificationFib.join
      _ <- tickingClockFib.join
    } yield ()

  }

  /*
   Exercise -
    1. Generalize racePair
   */

  type RaceResult[F[_], A, B] = Either[
    (
        Outcome[F, Throwable, A],
        Fiber[F, Throwable, B]
    ), // (winner result, loser fiber)
    (
        Fiber[F, Throwable, A],
        Outcome[F, Throwable, B]
    ) // (looser fiber, winner result)
  ]

  type EitherOutcome[F[_], A, B] =
    Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  import cats.effect.syntax.monadCancel.* // guaranteeCase extension method
  import cats.effect.syntax.spawn.* // start extension method

  def ourRacePair[F[_], A, B](fa: F[A], fb: F[B])(using
      concurrent: Concurrent[F]
  ): F[RaceResult[F, A, B]] =
    concurrent.uncancelable { poll =>
      for {
        signal <- concurrent.deferred[EitherOutcome[F, A, B]]
        fiba <- fa
          .guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void)
          .start
        fibb <- fb
          .guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void)
          .start
        result <- poll(signal.get)
          .onCancel { // blocking call - should be cancelable
            for {
              cancelFibA <- fiba.cancel.start
              cancelFibB <- fibb.cancel.start
              _ <- cancelFibA.join
              _ <- cancelFibB.join
            } yield ()
          }
      } yield result match {
        case Left(outcomeA)  => Left((outcomeA, fibb))
        case Right(outcomeB) => Right((fiba, outcomeB))
      }

    }

  override def run: IO[Unit] = polymorphicRiceCooker

}
