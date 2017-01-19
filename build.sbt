scalaVersion := "2.12.1"

organization := "odomontois"

name := "bnat"

version := "0.1-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("releases")

fork in run := true

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scalaz" % "scalaz-core_2.11" % "7.2.8",
  "org.spire-math" %% "spire" % "0.13.0")

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")