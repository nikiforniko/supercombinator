import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "lambda",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    libraryDependencies += "org.http4s" %% "http4s-dsl"          % "0.17.5",
    libraryDependencies += "org.http4s" %% "http4s-blaze-server" % "0.17.5",
    libraryDependencies += "org.http4s" %% "http4s-blaze-client" % "0.17.5",
    libraryDependencies += "org.http4s" %% "http4s-circe"        % "0.17.5",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.8.0",
    libraryDependencies += "io.circe" %% "circe-literal" % "0.8.0",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime
  )

// Uncomment the following for publishing to Sonatype.
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for more detail.

// ThisBuild / description := "Some descripiton about your project."
// ThisBuild / licenses    := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
// ThisBuild / homepage    := Some(url("https://github.com/example/project"))
// ThisBuild / scmInfo := Some(
//   ScmInfo(
//     url("https://github.com/your-account/your-project"),
//     "scm:git@github.com:your-account/your-project.git"
//   )
// )
// ThisBuild / developers := List(
//   Developer(
//     id    = "Your identifier",
//     name  = "Your Name",
//     email = "your@email",
//     url   = url("http://your.url")
//   )
// )
// ThisBuild / pomIncludeRepository := { _ => false }
// ThisBuild / publishTo := {
//   val nexus = "https://oss.sonatype.org/"
//   if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
//   else Some("releases" at nexus + "service/local/staging/deploy/maven2")
// }
// ThisBuild / publishMavenStyle := true
