name := "bucklang"

version := "1.0"

enablePlugins(ScalaJSPlugin)


scalaVersion := "2.11.8"

resolvers ++= List(
  Resolver.sonatypeRepo("releases"),
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)


// I have no idea what I'm doing with dependency management for Scala and ScalaJS.
// Please don't use this as a model of a well-built project :/
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "3.0.0",
  "com.lihaoyi" %%% "fastparse" % "0.4.1",
  "org.scalacheck" %% "scalacheck" % "1.13.2",
  "com.softwaremill.quicklens" %%% "quicklens" % "1.4.8",
  "org.scalactic" %%% "scalactic" % "3.0.0"
)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.0"

scalaJSUseRhino in Global := false


resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "Twitter Maven" at "https://maven.twttr.com")

