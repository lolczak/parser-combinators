name := "testinator-client"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.2.2" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test" withSources() withJavadoc()
  //"org.scala-lang" % "scala-library" % "2.10.4"
)
