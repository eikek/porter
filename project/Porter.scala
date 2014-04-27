import sbt._
import sbt.Keys._

object Version {
  val scala = "2.10.4"
  val scalaTest = "2.1.0"
  val akka = "2.2.4"
  val spray = "1.2.1"
  val casbah = "2.7.0"
  val reactiveMongo = "0.10.0"
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
    "com.typesafe.akka" %% "akka-actor" % Version.akka,
    "com.typesafe.akka" %% "akka-testkit" % Version.akka % "test"
  )
  val akkaRemote = Seq(
    "com.typesafe.akka" %% "akka-remote" % Version.akka
  )

  val casbah = Seq(
    "org.mongodb" %% "casbah-commons" % Version.casbah,
    "org.mongodb" %% "casbah-query" % Version.casbah,
    "org.mongodb" %% "casbah-core" % Version.casbah
  )

  val reactiveMongo = Seq(
    //use a new version of log4j2, ran into this: https://issues.apache.org/jira/browse/LOG4J2-477
    //also remove the 14mb dependency scala-compiler
    "org.reactivemongo" %% "reactivemongo" % Version.reactiveMongo excludeAll (
      ExclusionRule("org.apache.logging.log4j", "log4j-api"),
      ExclusionRule("org.apache.logging.log4j", "log4j-core"),
      ExclusionRule("org.scala-lang", "scala-compiler"),
      ExclusionRule("org.scala-lang", "scala-reflect")
      ),
    "org.reactivemongo" %% "reactivemongo-bson" % Version.reactiveMongo intransitive(),
    "org.apache.logging.log4j" % "log4j-core" % "2.0-rc1",
    "org.apache.logging.log4j" % "log4j-api" % "2.0-rc1"
  )

  val logback = Seq(
    "org.slf4j" % "slf4j-api" % "1.7.5",
    "ch.qos.logback" % "logback-classic" % "1.0.13" % "runtime",
    "com.typesafe.akka" %% "akka-slf4j" % Version.akka % "runtime"
  )

  val spray = Seq(
    "io.spray" % "spray-can" % Version.spray,
    "io.spray" % "spray-routing" % Version.spray
  )
  val sprayJson = Seq(
    "io.spray" %%  "spray-json" % "1.2.5"
  )
  val sprayOpenId = Seq(
    "org.eknet.spray" %% "spray-openid" % "0.1.0-SNAPSHOT"
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
    version := "0.2.0-SNAPSHOT",
    resolvers ++= Seq("spray repo" at "http://repo.spray.io", "eknet-maven2" at "https://eknet.org/maven2", "typesafe-releases" at "http://repo.typesafe.com/typesafe/releases"),
    publishTo := Some("eknet-maven2" at "https://eknet.org/maven2"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    pomIncludeRepository := { _ => false },
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
      libraryDependencies ++= Deps.apiCompile ++ Deps.testBasics ++ Deps.sprayJson.map(_ % "optional")
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
      libraryDependencies ++= Deps.akka ++ Deps.testBasics ++ Deps.casbah ++ Deps.reactiveMongo ++ Deps.spray ++ Deps.sprayJson
    )
  ) dependsOn Api.module
}

object OpenId extends Build {
  lazy val module = Project(
    id = "openid",
    base = file("openid"),
    settings = Project.defaultSettings ++ Seq(
      name := "porter-openid",
      libraryDependencies ++= Deps.akka ++ Deps.spray ++ Deps.sprayOpenId ++ Deps.testBasics
    )
  ) dependsOn App.module
}

object Dist extends Build {
  import com.typesafe.sbt.SbtAtmos.{Atmos, atmosSettings, AtmosKeys}

  lazy val module = Project(
    id = "dist",
    base = file("dist"),
    settings = Project.defaultSettings ++ Distribution.distSettings ++ Seq(
      name := "porter-dist",
      libraryDependencies ++= Deps.logback ++ Deps.akkaRemote,
      fork in run := true,
      baseDirectory in (Atmos, run) := file("dist/src/main/dist"),
      AtmosKeys.atmosJvmOptions := Seq("-Xms512m", "-Xmx1024m", "-XX:+UseParallelGC", "-Dconfig.file=etc/porter.conf"),
      javaOptions in run := Seq(
        "-Dconfig.file=etc/porter.conf",
        "-Dlogback.configurationFile=etc/logback.xml",
        "-Dporter.openid.cookie-secure=false",
        "-Dporter.openid.registration-enabled=true",
        "-Dporter.openid.registration-invitation-key=1"
      ),
      com.typesafe.sbt.SbtAtmos.traceAkka("2.2.1")
    )
  )
  .dependsOn (App.module, OpenId.module)
  .configs(Atmos)
  .settings(atmosSettings: _*)
}
