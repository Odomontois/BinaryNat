scalaVersion := "2.11.7"

name := "bnat"

version := "0.1-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("releases")

fork in run := true

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalaz" % "scalaz-core_2.11" % "7.2.0-M4",
  "org.spire-math" %% "spire" % "0.10.1")