ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "me.vilunov"
ThisBuild / organizationName := "Nikita Vilunov"

lazy val root = (project in file("."))
  .settings(
    name := "bsalc",
    libraryDependencies += deps.cats,
    libraryDependencies += deps.derevo.cats,
    libraryDependencies += deps.scalaTest % Test,
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
  )
