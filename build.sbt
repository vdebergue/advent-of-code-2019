ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "com.github.vdebergue"

lazy val advent = (project in file("."))
  .settings(
    name := "advent",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.1.0" % Test
    )
  )

scalafmtOnCompile := true
