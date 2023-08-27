package catsEffectByExample.section4

import cats.effect.{Deferred, IO, IOApp, Ref}
import catsEffectByExample.utils.*

import scala.concurrent.duration.*
import cats.syntax.traverse.*
object Defers extends IOApp.Simple {
  // deferred is a primitive for waiting for an effect, while some other effect completes with a value
  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  // get blocks the calling fiber (semantically) util some other fiber completes the Deferred with a value
  val reader = aDeferred.flatMap { signal =>
    signal.get // blocks the fiber semantically
  }

  val writer = aDeferred.flatMap { signal =>
    signal.complete(45)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[Consumer] waiting the result...").debug
      mol <- signal.get // blocker
      _ <- IO(s"[consumer] got the result: $mol").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO("[producer] computing...").debug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] produced: 45").debug
      mol <- IO(45)
      _ <- signal.complete(mol)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts
        .map { part =>
          IO(s"[downloader] got '$part'").debug >> IO.sleep(
            1.second
          ) >> contentRef.update(_ + part)
        }
        .sequence
        .void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <-
        if (file.endsWith("<EOF>")) IO("[notifier] File download complete").debug
        else
          IO("[notifier] downloading...").debug >> IO.sleep(
            500.millis
          ) >> notifyFileComplete(contentRef) // busy wait!
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fileDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fileDownloader.join
      _ <- notifier.join
    } yield ()

  }

  //using deferred
  def fileNotifierWithDefered(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading...").debug
      _ <- signal.get // semantically blocks until the signal is completed
      _ <- IO("[notifier] File download complete").debug
    } yield ()

    def downloadFilePart(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]) : IO[Unit] = for {
      _ <- IO(s"[downloader] got '$part'").debug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(_ + part)
        _ <- if (latestContent.contains("<EOF>")) signal.complete(latestContent) else IO.unit
    } yield ()
    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifierFib <- notifyFileComplete(signal).start
      fileTaskFib <- fileParts.map(part => downloadFilePart(part, contentRef, signal)).sequence.start
      _ <- notifierFib.join
      _ <- fileTaskFib.join
    } yield ()

  }

  /**
   * Exercises:
   *  - (medium) write a small alarm notification with two simultaneous IOs
   *    - one that increments a counter every second (a clock)
   *    - one that waits for the counter to become 10, then prints a message "time's up!"
   *
   *  - (mega hard) implement racePair with Deferred.
   *    - use a Deferred which can hold an Either[outcome for ioa, outcome for iob]
   *    - start two fibers, one for each IO
   *    - on completion (with any status), each IO needs to complete that Deferred
   *      (hint: use a finalizer from the Resources lesson)
   *      (hint2: use a guarantee call to make sure the fibers complete the Deferred)
   *    - what do you do in case of cancellation (the hardest part)?
   */
  // 1

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[Either[]]

  override def run: IO[Unit] = {
    // IO.unit
    //demoDeferred()
    //fileNotifierWithRef()
    fileNotifierWithDefered()
  }
}
