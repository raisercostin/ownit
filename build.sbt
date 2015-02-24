import bintray.Keys._
import com.github.retronym.SbtOneJar._

organization := "org.raisercostin"

name := "ownit"

version := "0.4"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
	//"net.sf.jopt-simple" % "jopt-simple" % "2.4.1" intransitive() //exclude("org.apache.ant" % "ant"),
	"dom4j" % "dom4j" % "1.6.1"
	,"jaxen" % "jaxen" % "1.1.6"
	,"org.scalatest" %% "scalatest" % "2.0" //% "test"
	,"junit" % "junit" % "4.10" //% "test"
	,"org.slf4j" % "slf4j-api" % "1.7.5"
	,"org.slf4j" % "slf4j-simple" % "1.7.5"
	,"commons-io" % "commons-io" % "2.4"
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
	,"org.raisercostin" % "raisercostin-utils" % "0.3"
)

sbtPlugin := true

publishMavenStyle := true

bintrayPublishSettings

repository in bintray := "generic"

// This is an example.  bintray-sbt requires licenses to be specified 
// (using a canonical name).
licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))

bintrayOrganization in bintray := None

resolvers ++= Seq(
	"thenewmotion" at "http://nexus.thenewmotion.com/content/repositories/releases-public"
	//,"The Buzz Media Maven Repository" at "http://maven.thebuzzmedia.com"
	,"raisercostin" at "https://raisercostin.googlecode.com/svn/maven2"
)

//EclipseKeys.withSource := true


com.github.retronym.SbtOneJar.oneJarSettings

artifact in oneJar <<= moduleName(x=>Artifact("organize"))

mainClass in oneJar := Some("org.raisercostin.apps.Renamer")

mainClass := Some("org.raisercostin.apps.Renamer")

unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil

unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil

//generate eclipse project with resources folders in classpath as well
EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

test in assembly := {}

net.virtualvoid.sbt.graph.Plugin.graphSettings