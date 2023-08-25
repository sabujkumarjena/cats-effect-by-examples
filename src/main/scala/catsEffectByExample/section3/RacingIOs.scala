package catsEffectByExample.section3

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp, Outcome, OutcomeIO}

import scala.concurrent.duration.*

object RacingIOs extends IOApp.Simple {

  import catsEffectByExample.utils.*

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] = {
    (
      IO(s"starting computation: $value").debug >>
        IO.sleep((duration)) >>
        IO(s"Computation for value $value: done") >>
        IO(value)
    ).onCancel(
      IO(s"computation CANCELLED for $value").debug.void
    )
  }

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 3.second)
    val favLang = runWithSleep("Scala", 2.seconds)

    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
      - both IOs run on separate fiber
      - the first one to finish will complete the result
      - the loser will be canceled
     */

    first.flatMap {
      case Left(mol)   => IO(s"Meaning of life won: $mol").debug
      case Right(lang) => IO(s"Fav language won: $lang").debug
    }

  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 3.second)
    val favLang = runWithSleep("Scala", 2.seconds)

    val raceResult: IO[Either[
      (OutcomeIO[Int], FiberIO[String]), // (winner result, loser fiber)
      (FiberIO[Int], OutcomeIO[String]) // (looser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((resMol, lanFib)) =>
        lanFib.cancel >> IO("MOL won").debug >> IO(resMol).debug
      case Right((molFib, resLang)) =>
        molFib.cancel >> IO("language won").debug >> IO(resLang).debug
    }
  }

  /** Exercise: 1 - implement a timeout pattern with race 2 - amethod to return
    * a LOSING effect from a race 3- implement race in terms of racePair
    */

  // 1-
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(io, IO.sleep(duration)) flatMap {
      case Left(a) => IO(s"Computation finishes first").debug >> IO(a).debug
      case Right(_) =>
        IO(s" Timed out").debug >> IO.raiseError(
          new RuntimeException("Time out")
        )
    }

  val importantTask = IO.sleep(2.seconds) >> IO(42).debug
  val testTimeout = timeout(importantTask, 1.second).void
  val testTimeout_v2 = importantTask.timeout(3.seconds) // cats effect api

  // 2 -
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = IO
    .racePair(ioa, iob)
    .flatMap {
      case Left((_, fibB)) =>
        fibB.join.flatMap {
          case Succeeded(resultEffect) =>
            resultEffect.map(Right(_))
          case Errored(e) => IO.raiseError(e)
          case Canceled() =>
            IO.raiseError(new RuntimeException(" Looser Cancelled.."))
        }
      case Right((fibA, _)) =>
        fibA.join.flatMap {
          case Succeeded(resultEffect) =>
            resultEffect.map(Left(_))
          case Errored(e) => IO.raiseError(e)
          case Canceled() =>
            IO.raiseError(new RuntimeException(" Looser Cancelled.."))
        }
    }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) =>
        outA match {
          case Succeeded(valueA) => fibB.cancel >> valueA.map(Left(_))
          case Errored(e)        => fibB.cancel >> IO.raiseError(e)
          case Canceled() =>
            fibB.join.flatMap {
              case Succeeded(valueB) => valueB.map(Right(_))
              case Errored(e)        => IO.raiseError(e)
              case Canceled() =>
                IO.raiseError(
                  new RuntimeException("Both Computation cancelled")
                )
            }
        }
      case Right((fibA, outB)) =>
        outB match {
          case Succeeded(valueB) => fibA.cancel >> valueB.map(Right(_))
          case Errored(e)        => fibA.cancel >> IO.raiseError(e)
          case Canceled() =>
            fibA.join.flatMap {
              case Succeeded(valueA) => valueA.map(Left(_))
              case Errored(e)        => IO.raiseError(e)
              case Canceled() =>
                IO.raiseError(
                  new RuntimeException("Both Computation cancelled")
                )
            }
        }
    }

  override def run: IO[Unit] = {
    // testRace().void
    // testRacePair().debug.void
    testTimeout_v2.debug.void

  }

}
