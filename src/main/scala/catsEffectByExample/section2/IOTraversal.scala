package catsEffectByExample.section2

import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {
  import scala.concurrent.ExecutionContext.Implicits.global
  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] =
    List(" I like Cats", " Scala is awsome", "ZIO is great")

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)

    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._

  val listTraverse = Traverse[List]
  def traverseFutures(): Unit = {
    // traverse

    val singleFuture: Future[List[Int]] =
      listTraverse.traverse(workLoad)(heavyComputation)

    // this stores ALL the results in one future
    singleFuture.foreach(println)
  }

  // traverse for ios
  import catsEffectByExample.utils._
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug
  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._ // parTraverse is an extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /** Exercises
    */
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(identity)
  def sequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(identity)

  // parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)
  def parSequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(identity)

  // existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)
  // existing parallel sequence
  val parallelSingleIO_v2: IO[List[Int]] =
    ios.parSequence // extension method  from the Parallel syntax package
  override def run: IO[Unit] = parallelSingleIO.map(println)
}
