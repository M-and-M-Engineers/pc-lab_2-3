name := "formalsystems"

ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "3.1.3"

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8", // Option and arguments on same line
  "-Xfatal-warnings",  // New lines for each options
  "-deprecation",
  "-explain",
  "-unchecked",
  "-new-syntax",
  "-rewrite"
)

val scalatest = "org.scalatest" %% "scalatest" % "3.2.12" % "test"
val scalacheck = "org.scalacheck" %% "scalacheck" % "1.16.0" % "test"
val scalaParallelCollections = "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

lazy val root = (project in file (".")).
  settings(Seq(scalaVersion := "3.1.3")).
  settings(libraryDependencies ++= Seq (scalatest, scalacheck, scalaParallelCollections))
