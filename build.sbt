lazy val root = (project in file("."))
  .configs(IntegrationTest)
  .settings(
    Defaults.itSettings,
    inThisBuild(
      List(
        organization := "ch.epfl",
        scalaVersion := "2.12.12"
      )
    ),
    name := "mathgraph",
  )

scalacOptions += "-deprecation"
logBuffered in IntegrationTest := false
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test,it"
