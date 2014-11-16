name := "testinator-client"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.4"

resolvers += "Tim Tennant's repo" at "http://dl.bintray.com/timt/repo/"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.2.2" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test" withSources() withJavadoc(),
  "io.shaka" %% "naive-http-server" % "28" withSources() withJavadoc(),
  "org.scalaz" % "scalaz-effect_2.10" % "7.1.0" withSources() withJavadoc()
  //"org.scala-lang" % "scala-library" % "2.10.4"
)
