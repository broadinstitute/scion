val scionV = "0.0.1"
val scalaV = "2.12.8"
val scalatestV = "3.0.5"
val betterFilesV = "3.7.0"
val logbackClassicV = "1.2.3"
val circeV = "0.11.1"

lazy val commonSettings = Seq(
  version := scionV,
  organization := "org.broadinstitute",
  scalaVersion := scalaV,
  resolvers ++= Seq(
    Resolver.mavenLocal
  ),
  scalacOptions := Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-Ypartial-unification"
  ),
  libraryDependencies ++= Seq(
    "com.github.pathikrit" %% "better-files" % betterFilesV,
    "ch.qos.logback" % "logback-classic" % logbackClassicV,
    "org.scalatest" %% "scalatest" % scalatestV % "test",
    "io.circe" %% "circe-generic" % circeV
  )
)

lazy val core = (project in file("core"))
  .settings(commonSettings)

