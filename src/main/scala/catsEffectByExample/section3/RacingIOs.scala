package catsEffectByExample.section3

import cats.effect.{IO, IOApp}

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
      case Left(mol) => IO(s"Meaning of life won: $mol").debug
      case Right(lang) => IO(s"Fav language won: $lang").debug
    }

  }
  override def run: IO[Unit] = testRace().void

}
