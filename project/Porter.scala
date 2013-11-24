import sbt._
import sbt.Keys._

object Version {
  val scala = "2.10.3"
  val scalaTest = "1.9.1"
}

object Deps {

  val testBasics = Seq(
    "org.scalatest" %% "scalatest" % "1.9.1",
    "org.scalacheck" %% "scalacheck" % "1.10.1"
  ) map (_ % "test")

  val apiCompile = Seq(
    "org.mindrot" % "jbcrypt" % "0.3m",
    "com.lambdaworks" % "scrypt" % "1.4.0"
  )

  val akka = Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.2.3"
  )
}

object Porter extends sbt.Build {
  import sbtbuildinfo.Plugin._

  lazy val module = Project(
    id = "porter",
    base = file("."),
    settings = Project.defaultSettings ++ buildInfoSettings ++ Seq(
      name := "porter",
      publish := {},
      publishLocal := {},
      unmanagedSourceDirectories in Compile := Seq(),
      unmanagedSourceDirectories in Test := Seq(),
      unmanagedResourceDirectories := Seq()
    )
  ) aggregate (Api.module, App.module)

  override lazy val settings = super.settings ++ Seq(
    version := "0.1.0",
    organization := "org.eknet.porter",
    scalaVersion := Version.scala,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    licenses := Seq("ASL2" -> url("http://www.apache.org/LICENESE-2.0.txt"))
  )
}

object Api extends sbt.Build {

  lazy val module = Project(
    id = "api",
    base = file("api"),
    settings = Project.defaultSettings ++ Seq(
      name := "porter-api",
      libraryDependencies ++= Deps.apiCompile ++ Deps.testBasics
    )
  )
}

object App extends sbt.Build {

  lazy val module = Project(
    id = "app",
    base = file("app"),
    settings = Project.defaultSettings ++ Seq(
      name := "porter-app",
      libraryDependencies ++= Deps.akka ++ Deps.testBasics
    )
  ) dependsOn Api.module
}