scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("releases")

fork in run := true

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.5",
  "org.scalaz" % "scalaz-core_2.11" % "7.2.0-M3",
  "org.spire-math" %% "spire" % "0.10.1" )