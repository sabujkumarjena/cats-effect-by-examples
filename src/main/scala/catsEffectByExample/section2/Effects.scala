package catsEffectByExample.section2

import scala.concurrent.Future

object Effects {

  // referential transparency = can replace an expression with its value as many times as we want without changing behavior

  /*
  Effect Types
  Properties:
  - type signature describes the kind of calculation that will be performed
  - type signature describes the VALUE that will be calculated
  - when side effects are needed, effect construction is separate from effect execution
   */
  /*
  Example: Option is an effect type
  - describe a possibly value
  - computes a value of type A, if it exists
  - side effects are not needed
   */
  val anOption: Option[Int] = Option(45)

  /*
   example: Future is NOT an effect type
   - describe an asynchronous computation
  - computes a value of type A, if its successful
   - side effect is required (allocating/scheduling a thread), execution is not separate from construction
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(45)

  /* example: MyIO - it is an Effect type
  -describes any computation that might produce side effect
  - calculates a value of type A, if it is successful
  - side effects are required for the evaluation of () => A
    -YES, the creation of MyIO does NOT produce the side effects on construction

   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something")
    42
  })


  def main(args: Array[String]): Unit = {
    println("sabuj")
    anIO.unsafeRun()
  }

}
