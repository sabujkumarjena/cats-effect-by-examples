/*
//val scala3Version = "2.13.6"
//val catsEffectVersion = "3.2.0"

//val scala3Version = "3.3.0"
//val catsEffectVersion = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-effect-by-examples",
    version := "0.1.0",

    scalaVersion := "3.3.0",
    scalacOptions ++= Seq("-deprecation"),
//    libraryDependencies ++= Seq(
//      "org.typelevel" %% "cats-effect" % "3.2.0"
//    ),
//    scalacOptions ++= Seq(
//      "-language:higherKinds"
//    )
  )

 */

ThisBuild / scalaVersion := "3.0.0"

lazy val root = (project in file(".")).settings(
  name := "cats-effects-by-examples",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.3.12",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.3.12",
    "org.typelevel" %% "cats-effect-testing-specs2" % "1.4.0" % Test,
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
  )
)