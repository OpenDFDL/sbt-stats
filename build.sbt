sbtPlugin := true

name := "sbt-stats"

organization := "com.orrsella"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.6"

libraryDependencies += "org.apache.tika" % "tika-core" % "1.21"

// libraryDependencies += "org.apache.tika" % "tika-parsers" % "1.21"


retrieveManaged := true
scalaVersion := "2.12.6"

scalacOptions ++= Seq("-feature", "-language:implicitConversions" )

// publishing
// crossSbtVersions := Vector("0.13.16", "1.0.1")

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>https://github.com/orrsella/sbt-stats</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:orrsella/sbt-stats.git</url>
    <connection>scm:git:git@github.com:orrsella/sbt-stats.git</connection>
  </scm>
  <developers>
    <developer>
      <id>orrsella</id>
      <name>Orr Sella</name>
      <url>http://orrsella.com</url>
    </developer>
  </developers>
)

releasePublishArtifactsAction := PgpKeys.publishSigned.value

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)
