lazy val root = project
  .in(file("."))
  .settings(
    name         := "scala3-derivation",
    version      := "0.1.0",
    scalaVersion := "3.0.0",

    libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.9" % "test"
  )
