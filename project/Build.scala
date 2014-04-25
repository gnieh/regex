package regex

import sbt._
import Keys._
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.sbt.osgi.OsgiKeys

object DiffsonBuild extends Build {

  lazy val regex = (Project(id = "regex",
    base = file(".")) settings (
    organization := "org.gnieh",
    name := "regex",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "2.11.0",
    crossScalaVersions := Seq("2.10.4", "2.11.0"),
    description := "Efficient regular expressions in Scala",
    licenses += ("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    homepage := Some(url("https://github.com/gnieh/regex")),
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    parallelExecution := false,
    fork in test := true)
    settings(osgiSettings: _*)
    settings(
    resourceDirectories in Compile := List(),
    OsgiKeys.exportPackage := Seq(
      "gnieh.regex"
    ),
    OsgiKeys.additionalHeaders := Map (
      "Bundle-Name" -> "Gnieh Regular Expressions"
    ),
    OsgiKeys.bundleSymbolicName := "org.gnieh.regex",
    OsgiKeys.privatePackage := Seq(),
    scalacOptions ++= Seq("-deprecation", "-feature"))
    settings(publishSettings: _*)
  )

  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    publishArtifact in Test := false,
    // The Nexus repo we're publishing to.
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
        else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    pomExtra := (
      <scm>
        <url>https://github.com/gnieh/regex</url>
        <connection>scm:git:git://github.com/gnieh/regex.git</connection>
        <developerConnection>scm:git:git@github.com:gnieh/regex.git</developerConnection>
        <tag>HEAD</tag>
      </scm>
      <developers>
        <developer>
          <id>satabin</id>
          <name>Lucas Satabin</name>
          <email>lucas.satabin@gnieh.org</email>
        </developer>
      </developers>
      <ciManagement>
        <system>travis</system>
        <url>https://travis-ci.org/#!/gnieh/regex</url>
      </ciManagement>
      <issueManagement>
        <system>github</system>
        <url>https://github.com/gnieh/regex/issues</url>
      </issueManagement>
    )
  )

  lazy val benchmarks =
    Project(id = "benchmarks", base = file("benchmarks")) dependsOn(regex) settings(
      scalaVersion := "2.11.0",
      libraryDependencies += "com.github.axel22" %% "scalameter" % "0.5-M2",
      testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
      parallelExecution in Test := false
    )

}
