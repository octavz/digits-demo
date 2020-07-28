scalaVersion := "2.13.1"
name := "digits"
organization := "demo"
version := "1.0"

val zioVersion = "1.0.0-RC21"

libraryDependencies += "dev.zio" %% "zio-test" % zioVersion
libraryDependencies += "com.google.guava" % "guava" % "29.0-jre"

