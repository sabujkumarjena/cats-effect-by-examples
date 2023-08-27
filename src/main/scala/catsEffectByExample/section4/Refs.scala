package catsEffectByExample.section4

import cats.effect.{IO, IOApp, Ref}

import scala.concurrent.duration.*
import catsEffectByExample.utils.*
object Refs extends IOApp.Simple {
  // ref = purely functional atomic reference
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(45)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(45)

  // modifying is an effect
  val increasedMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.set(40) // thread-safe
  }
  // obtain a value
  val mol: IO[Int] = atomicMol.flatMap { ref =>
    ref.get // thread-safe
  }

  // gets the old value, sets the new one
  val gsMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.getAndSet(43)
  }

  // updating with a function
  val fMol: IO[Unit] = atomicMol.flatMap { ref =>
    ref.update(_ * 10)
  }

  val updatedMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.updateAndGet(_ * 10) // get the new value
  } // use getAndUpdate to get the OLD value

  val modifiedMol: IO[String] = atomicMol.flatMap { ref =>
    ref.modify(value => (value * 10, s"my current value is $value"))
  }

  // why: concurrent + thread-safe read/write over shared values, in a purely functional way

  import cats.syntax.parallel._
  def demoConcurrentWorkImpure(): IO[Unit] = {

    var count = 0
    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for $workload :  $wordCount").debug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New total: $newCount").debug
        _ <- IO(count += wordCount)
      } yield ()
    }

    List(
      " I love Scala",
      "This ref thing is useless",
      "Sabuj writes alot of code"
    ).map(task).parSequence.void
  }
  /*
    Drawbacks:
      - hard to read/debug
      - mix pure/impure code
      - NOT THREAD SAFE
   */

  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for $workload :  $wordCount").debug
        newCount <- total.updateAndGet(_ + wordCount)
        _ <- IO(s"New total: $newCount").debug
      } yield ()
    }
    for {
      initialCount <- Ref[IO].of(0)
      _ <- List(
        " I love Scala",
        "This ref thing is useless",
        "Sabuj writes alot of code"
      ).map (string => task(string, initialCount)).parSequence
    } yield ()
  }
  override def run: IO[Unit] = {
    // IO.unit
    // demoConcurrentWorkImpure()
    demoConcurrentWorkPure()
  }
}
