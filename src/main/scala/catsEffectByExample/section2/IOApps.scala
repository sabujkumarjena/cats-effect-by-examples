package catsEffectByExample.section2

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps {
  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO(println(s"You have entered : $line"))
  } yield ()
}

object Test {

  import IOApps._

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    program.unsafeRunSync()
  }
}
object FirstCEApp extends IOApp {
  import IOApps._
  override def run(args: List[String]): IO[ExitCode] =
    //program.map(_ => ExitCode.Success)
    program.as(ExitCode.Success)
}

object MySimpleApp extends IOApp.Simple{
  import IOApps._
  override def run: IO[Unit] = program
}
