package catsEffectByExample.whiteBoard

import cats.effect.{IO, IOApp}
object WhiteBoard extends IOApp.Simple {
  override def run: IO[Unit] =
    IO.println("learing cats effect")

}
object WhiteBoard2 {
  def main(args: Array[String]): Unit = {
    println("sabuj")
  }
}