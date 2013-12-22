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
    "com.typesafe.akka" %% "akka-actor" % "2.2.3",
    "com.typesafe.akka" %% "akka-testkit" % "2.2.3" % "test"
  )
  val akkaRemote = Seq(
    "com.typesafe.akka" %% "akka-remote" % "2.2.3"
  )

  val casbah = Seq(
    "org.mongodb" %% "casbah-commons" % "2.6.3",
    "org.mongodb" %% "casbah-query" % "2.6.3",
    "org.mongodb" %% "casbah-core" % "2.6.3"
  )

  val logback = Seq(
    "org.slf4j" % "slf4j-api" % "1.7.5",
    "ch.qos.logback" % "logback-classic" % "1.0.13" % "runtime",
    "com.typesafe.akka" %% "akka-slf4j" % "2.2.3" % "runtime"
  )

  val spray = Seq(
    "io.spray" % "spray-can" % "1.2.0"
  )
  val sprayRouting = Seq(
    "io.spray" % "spray-routing" % "1.2.0"
  )
}

object Porter extends sbt.Build {
  lazy val module = Project(
    id = "porter",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "porter",
      publish := {},
      publishLocal := {},
      unmanagedSourceDirectories in Compile := Seq(),
      unmanagedSourceDirectories in Test := Seq(),
      unmanagedResourceDirectories := Seq()
    )
  ) aggregate (Api.module, App.module, OpenId.module, Dist.module)

  override lazy val settings = super.settings ++ Seq(
    version := "0.1.0",
    resolvers ++= Seq("spray repo" at "http://repo.spray.io"),
    organization := "org.eknet.porter",
    scalaVersion := Version.scala,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    licenses := Seq("ASL2" -> url("http://www.apache.org/LICENESE-2.0.txt"))
  )
}

object Api extends sbt.Build {
  import sbtbuildinfo.Plugin._

  lazy val module = Project(
    id = "api",
    base = file("api"),
    settings = Project.defaultSettings ++ buildInfoSettings ++ Seq(
      name := "porter-api",
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq(name, version, scalaVersion, buildTimeKey, gitRevKey),
      buildInfoPackage := "porter",
      libraryDependencies ++= Deps.apiCompile ++ Deps.testBasics
    )
  )

  lazy val buildTimeKey = BuildInfoKey.action("buildTime") {
    System.currentTimeMillis
  }
  lazy val gitRevKey = BuildInfoKey.action("revision") {
    Process("git rev-parse HEAD").lines.head
  }
}

object App extends sbt.Build {

  lazy val module = Project(
    id = "app",
    base = file("app"),
    settings = Project.defaultSettings ++ Seq(
      name := "porter-app",
      libraryDependencies ++= Deps.akka ++ Deps.testBasics ++ Deps.casbah ++ Deps.spray
    )
  ) dependsOn Api.module
}

object OpenId extends Build {
  lazy val module = Project(
    id = "openid",
    base = file("openid"),
    settings = Project.defaultSettings ++ Seq(
      name := "porter-openid",
      libraryDependencies ++= Deps.akka ++ Deps.spray ++ Deps.sprayRouting ++ Deps.testBasics
    )
  ) dependsOn App.module
}

object Dist extends Build {

  lazy val module = Project(
    id = "dist",
    base = file("dist"),
    settings = Project.defaultSettings ++ Distribution.distSettings ++ Seq(
      name := "porter-dist",
      libraryDependencies ++= Deps.logback ++ Deps.akkaRemote
    )
  ) dependsOn (App.module, OpenId.module)
}
