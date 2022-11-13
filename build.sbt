ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "snake"
  )

libraryDependencies := Seq(
  "org.typelevel" %% "cats-core" % "2.8.0",
  "org.typelevel" %% "cats-effect" % "3.3.14",
  "com.googlecode.lanterna" % "lanterna" % "3.1.1"
)
