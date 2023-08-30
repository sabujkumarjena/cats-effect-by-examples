package catsEffectByExample.section4

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Concurrent, Deferred, IO, IOApp, Ref}

import scala.util.Random
import scala.concurrent.duration.*
import catsEffectByExample.utils.*
import cats.syntax.parallel.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.effect.syntax.monadCancel.*
import catsEffectByExample.section4.Mutex.MutexV2

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
  def create: IO[Mutex] = Ref[IO].of(unlocked).map(createMutexWithCancellation)
  def createMutexWithCancellation(state: Ref[IO, State]): Mutex = new Mutex {

    override def acquire: IO[Unit] = IO.uncancelable { poll =>
      createSignal().flatMap { signal =>
        val cleanup = state.modify { case State(locked, queue) =>
          val newQueue = queue.filterNot(_ eq signal)
          val isBlocking = queue.exists(_ eq signal)
          val decision = if (isBlocking) IO.unit else release
          State(locked, newQueue) -> decision // problem
        }.flatten
        state.modify {
          case State(false, _) =>
            State(locked = true, Queue()) -> IO.unit

          case State(true, queue) =>
            State(locked = true, queue.enqueue(signal)) -> poll(signal.get)
              .onCancel(cleanup)
        }.flatten
      }
    }

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

  // generic mutex after the polymorphic concurrent exercise
  abstract class MutexV2[F[_]] {
    def acquire: F[Unit]

    def release: F[Unit]
  }

  object MutexV2 {
    type Signal[F[_]] = Deferred[F, Unit]

    case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])

    def unlocked[F[_]] = State[F](locked = false, Queue())

    def createSignal[F[_]](using concurrent: Concurrent[F]): F[Signal[F]] =
      concurrent.deferred[Unit]

    def create[F[_]](using concurrent: Concurrent[F]): F[MutexV2[F]] =
      concurrent
        .ref(unlocked)
        .map(initialState => createMutexWithCancellation(initialState))

    def createMutexWithCancellation[F[_]](
        state: Ref[F, State[F]]
    )(using concurrent: Concurrent[F]): MutexV2[F] =
      new MutexV2[F] {
        override def acquire = concurrent.uncancelable { poll =>
          createSignal.flatMap { signal =>

            val cleanup = state.modify { case State(locked, queue) =>
              val newQueue = queue.filterNot(_ eq signal)
              val isBlocking = queue.exists(_ eq signal)
              val decision = if (isBlocking) concurrent.unit else release
              State(locked, newQueue) -> decision // problem

            }.flatten

            state.modify {
              case State(false, _) =>
                State[F](locked = true, Queue()) -> concurrent.unit
              case State(true, queue) =>
                State[F](locked = true, queue.enqueue(signal)) -> poll(
                  signal.get
                ).onCancel(cleanup)
            }.flatten
          }
        }

        override def release = state.modify {
          case State(false, _) => unlocked[F] -> concurrent.unit
          case State(true, queue) =>
            if (queue.isEmpty) unlocked[F] -> concurrent.unit
            else {
              val (signal, rest) = queue.dequeue
              State[F](locked = true, rest) -> signal.complete(()).void
            }
        }.flatten
      }

  }

  def createSimpleMutex(state: Ref[IO, State]): Mutex = new Mutex {
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

  def createLockingTask_v2(id: Int, mutex: MutexV2[IO]): IO[Int] = for {
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

  def createCancellingTask(id: Int, mutex: Mutex): IO[Int] = {
    if (id % 2 == 0) createLockingTask(id, mutex)
    else
      for {
        fib <- createLockingTask(id, mutex)
          .onCancel(IO(s"[task $id] received cancellation").debug.void)
          .start
        _ <- IO.sleep(2.seconds) >> fib.cancel
        out <- fib.join
        result <- out match {
          case Succeeded(effect) => effect
          case Errored(_)        => IO(-1)
          case Canceled()        => IO(-2)
        }
      } yield result
  }

  def createCancellingTask_v2(id: Int, mutex: MutexV2[IO]): IO[Int] = {
    if (id % 2 == 0) createLockingTask_v2(id, mutex)
    else
      for {
        fib <- createLockingTask_v2(id, mutex)
          .onCancel(IO(s"[task $id] received cancellation").debug.void)
          .start
        _ <- IO.sleep(2.seconds) >> fib.cancel
        out <- fib.join
        result <- out match {
          case Succeeded(effect) => effect
          case Errored(_)        => IO(-1)
          case Canceled()        => IO(-2)
        }
      } yield result
  }

  def demoCancellingTasks() = for {
    mutex <- Mutex.create
    results <- (1 to 10).toList.parTraverse(id =>
      createCancellingTask(id, mutex)
    )
  } yield results

  def demoCancellingTasks_v2() = for {
    mutex <- MutexV2.create[IO]
    results <- (1 to 10).toList.parTraverse(id =>
      createCancellingTask_v2(id, mutex)
    )
  } yield results

  def demoCancelWhileBlock() = for {
    mutex <- MutexV2.create[IO]
    fib1 <- (IO("[fib1] getting mutex").debug >> mutex.acquire >> IO(
      "[fib1] got the mutex, never releasing"
    ).debug >> IO.never).start

    fib2 <- (IO("[fib2] sleeping").debug >> IO.sleep(3.seconds) >> IO(
      " Trying to get mutex"
    ).debug >> mutex.acquire.onCancel(
      IO("[fib2] being cancelled").debug.void
    ) >> IO("[fib2] acquired mutex").debug).start

    fib3 <- (IO("[fib3] sleeping").debug >> IO.sleep(2500.millis) >> IO(
      " Trying to get mutex"
    ).debug >> mutex.acquire >> IO(
      "[fib3] acquired mutex:: if this shows, then FAIL"
    ).debug).start

    _ <- IO.sleep(4.seconds) >> IO("CANCELLING fib 2! ").debug >> fib2.cancel
    _ <- fib1.join
    _ <- fib2.join
    _ <- fib3.join
  } yield ()

  override def run: IO[Unit] = {
    // demoNonLockingTasks().debug.void
    // demoLockingTasks().debug.void
    // demoCancellingTasks().debug.void
    // demoCancellingTasks_v2().debug.void

    demoCancelWhileBlock().debug.void
  }
}
