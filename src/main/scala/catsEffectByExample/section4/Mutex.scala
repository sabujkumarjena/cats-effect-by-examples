package catsEffectByExample.section4

import cats.effect.{Deferred, IO, IOApp, Ref}

import scala.util.Random
import scala.concurrent.duration.*
import catsEffectByExample.utils.*
import cats.syntax.parallel.*

import scala.collection.immutable.Queue

abstract class Mutex {
  def acquire: IO[Unit]
  def release: IO[Unit]
}
object Mutex {
  type Signal = Deferred[IO, Unit]
  case class State(locked: Boolean, waiting: Queue[Signal])
  val unlocked = State(false, Queue.empty)

  def createSignal(): IO[Signal] = Deferred[IO, Unit]
  def create: IO[Mutex] = Ref[IO].of(unlocked).map { state =>
    new Mutex {
      /*
            Change the state of the Ref:
            - if the mutex is currently unlocked, state becomes (true, [])
            - if the mutex is locked, state becomes (true, queue + new signal) AND WAIT ON THAT SIGNAL.
       */
      override def acquire: IO[Unit] = createSignal().flatMap { signal =>
        state.modify {
          case State(false, _) =>
            State(locked = true, Queue()) -> IO.unit

          case State(true, queue) =>
            State(locked = true, queue.enqueue(signal)) -> signal.get
        }.flatten
      }

      /*
            Change the state of the Ref:
            - if the mutex is unlocked, leave the state unchanged
            - if the mutex is locked,
              - if the queue is empty, unlock the mutex, i.e. state becomes (false, [])
              - if the queue is not empty, take a signal out of the queue and complete it (thereby unblocking a fiber waiting on it)
       */

      override def release: IO[Unit] = state.modify {
        case State(false, _) => unlocked -> IO.unit
        case State(true, queue) =>
          if (queue.isEmpty) unlocked -> IO.unit
          else {
            val (signal, rest) = queue.dequeue
            State(locked = true, rest) -> signal.complete(()).void
          }
      }.flatten
    }
  }
}

object MutexPlayGround extends IOApp.Simple {
  def criticalTask(): IO[Int] = IO.sleep((1.second)) >> IO(Random.nextInt(100))
  def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"[task $id] working...").debug
    res <- criticalTask()
    _ <- IO(s"[task $id] got result: $res").debug
  } yield res

  def demoNonLockingTasks() = (1 to 10).toList.parTraverse(createNonLockingTask)

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
    _ <- IO(s"[task $id] waiting for permission...").debug
    _ <-
      mutex.acquire // blocks if the mutex has been acquired by some other fiber
      // critical section
    _ <- IO(s"[task $id] working...").debug
    res <- criticalTask()
    _ <- IO(s"[task $id] got result: $res").debug
    // critical section end
    _ <- mutex.release
    _ <- IO(s"[task $id] lock removed...").debug
  } yield res

  def demoLockingTasks() = for {
    mutex <- Mutex.create
    results <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
  } yield results
  override def run: IO[Unit] = {
    //demoNonLockingTasks().debug.void
    demoLockingTasks().debug.void
  }
}