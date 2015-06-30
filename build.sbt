//import bintray.Keys._
import com.github.retronym.SbtOneJar._

organization := "org.raisercostin"
name := "ownit"
description := "Content Organizer Assistant – automatic heuristic metadata extractor of Exif(images), ID3 tag(mp3)"
homepage := Some(url(s"https://github.com/raisercostin/"+name.value))

scalaVersion := "2.10.5"
//crossScalaVersions := Seq(scalaVersion.value, "2.11.4")
scalacOptions ++= Seq(Opts.compile.deprecation, "-feature")

libraryDependencies ++= Seq(
	"org.raisercostin" %% "jedi-io" % "0.18"
	,"commons-io" % "commons-io" % "2.4"
	,"org.slf4j" % "slf4j-api" % "1.7.5"
	,"org.slf4j" % "slf4j-simple" % "1.7.5"

	//"net.sf.jopt-simple" % "jopt-simple" % "2.4.1" intransitive() //exclude("org.apache.ant" % "ant"),
	,"dom4j" % "dom4j" % "1.6.1"
	,"jaxen" % "jaxen" % "1.1.6"
	,"org.apache.sanselan" % "sanselan" % "0.97-incubator"
	,"com.thenewmotion" % "time_2.10" % "2.4"
	//,"com.thebuzzmedia" % "exiftool-lib" % "1.1"
	,"com.thebuzzmedia.exiftool" % "exiftool-lib" % "2.3.9" exclude("org.slf4j","slf4j-log4j12")
	//for guava
	,"com.google.code.findbugs" % "jsr305" % "2.0.3"
	//,"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
	//,"com.netflix.rxjava" % "rxjava-scala" % "0.15.0"
	,"org.scala-lang" % "scala-swing" % scalaVersion.value
	//,"org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
	,"org.scalatest" %% "scalatest" % "2.0" % "test"
	,"junit" % "junit" % "4.10" % "test"
)

// This is an example.  bintray-sbt requires licenses to be specified 
// (using a canonical name).
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
resolvers += "raisercostin resolver" at "http://dl.bintray.com/raisercostin/maven"
resolvers += "thenewmotion" at "http://nexus.thenewmotion.com/content/repositories/releases-public"
resolvers += "The Buzz Media Maven Repository" at "http://maven.thebuzzmedia.com"

pomExtra := (
  <scm>
    <url>git@github.com:raisercostin/{name.value}.git</url>
    <connection>scm:git:git@github.com:raisercostin/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>raisercostin</id>
      <name>raisercostin</name>
      <url>https://github.com/raisercostin</url>
    </developer>
  </developers>
)

//eclipse
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource
EclipseKeys.withSource := true
EclipseKeys.eclipseOutput := Some("target2/eclipse")
unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil
unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil

//style
//scalariformSettings
scalastyleConfig := baseDirectory.value / "project" / "scalastyle_config.xml"

//bintray
publishMavenStyle := true
bintrayPackageLabels := Seq("scala", "io", "nio", "file", "path", "stream", "writer")
//bintrayPublishSettings
//repository in bintray := "generic"
//bintrayOrganization in bintray := None

//release plugin
//version is commented since the version is in version.sbt
releaseCrossBuild := false

//bintray&release
//bintray doesn't like snapshot versions - https://github.com/softprops/bintray-sbt/issues/12
releaseNextVersion := { ver => sbtrelease.Version(ver).map(_.bumpMinor.string).getOrElse(sbtrelease.versionFormatError) }

//autoexecutable
mainClass := Some("org.raisercostin.apps.Renamer")
com.github.retronym.SbtOneJar.oneJarSettings
//commented to be able to upload to bintray with the standard name
//[error] (*:publish) error uploading to https://api.bintray.com/maven/raisercostin/maven/maven/org/raisercostin/ownit_2.10/0.7/organize_2.10-0.7-onejar.jar: {"message":"Provided artifact path does not comply with Maven's convention"}
//artifact in oneJar <<= moduleName(x=>Artifact("organize"))
mainClass in oneJar := Some("org.raisercostin.apps.Renamer")
test in assembly := {}
artifact in (Compile, oneJar) ~= { art =>
	art.copy(`classifier` = Some("one-jar"))
}
addArtifact(artifact in (Compile, oneJar), oneJar)

//net.virtualvoid.sbt.graph.Plugin.graphSettings
