package catsEffectByExample.section4

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}

import scala.concurrent.duration.*
import catsEffectByExample.utils.*

import scala.util.Random

object Semaphores extends IOApp.Simple {
  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // total no of threads permit = 2

  //example: limiting the number of concurrent sessions on a server

  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in ..").debug
    _ <- sem.acquire
    //critical section
    _ <- IO(s"[session $id] loggen in , working..").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").debug
    //end of critical section
    _ <- sem.release
  } yield res

  def demoSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- login(1, sem).start
    user2Fib <- login(2, sem).start
    user3Fib <- login(3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join

  } yield  ()

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in ..").debug
    _ <- sem.acquireN(requiredPermits)
    //critical section
    _ <- IO(s"[session $id] loggen in , working..").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").debug
    //end of critical section
    _ <- sem.releaseN(requiredPermits)
  } yield res

  def demoWeightedSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1Fib <- weightedLogin(1, 1, sem).start
    user2Fib <- weightedLogin(2, 2,  sem).start
    user3Fib <- weightedLogin(3, 3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join

  } yield ()

  override def run: IO[Unit] = {
    //IO.unit
   // demoSemaphore()
   demoWeightedSemaphore()
  }

}
