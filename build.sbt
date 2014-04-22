organization := "org.gnieh"

name := "sreg"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.0"

crossScalaVersions := Seq("2.10.4", "2.11.0")

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test"

