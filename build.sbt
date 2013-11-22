name := "porter"

organization := "org.ekent.porter"

version := "0.1"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation", "-feature", "-deprecation")

// resolvers +=  "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"

// resolvers += "Typesafe repository snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"

// resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

libraryDependencies += "org.mindrot" % "jbcrypt" % "0.3m"

libraryDependencies += "com.lambdaworks" % "scrypt" % "1.4.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.3"

// libraryDependencies += "org.reactivemongo" %% "reactivemongo" % "0.10.0-SNAPSHOT"