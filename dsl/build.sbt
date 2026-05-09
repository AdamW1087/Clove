import scala.sys.process.*

val scala3Version = "3.8.1"

addCommandAlias("play", "run")
addCommandAlias("dev", "run --reload")

lazy val root = project
  .in(file("."))
  .settings(
    name := "Clove",
    version := "0.0.1",
    scalaVersion := scala3Version,

    scalacOptions ++= Seq("-feature", "-language:implicitConversions"),

    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.10.0",

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,

    Compile / run / fork := true,

    (Compile / run) := {
      (Compile / run).evaluated
      val cmd = sys.props("os.name").toLowerCase() match {
        case osName if osName contains "windows" => Seq("cmd", "/C", "love", "output")
        case _ => Seq("love", "output")
      }
      cmd.!
    }
  )