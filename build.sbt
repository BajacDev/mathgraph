lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "ch.epfl",
      scalaVersion := "2.12.12"
    )
  ),
  name := "mathgraph"
)

scalacOptions += "-deprecation"
parallelExecution in Test := false

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
