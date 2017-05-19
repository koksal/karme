lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

val circeVersion = "0.8.0"

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "karme",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.5.0",
      "org.ddahl" %% "rscala" % "1.0.15",
      "com.github.tototoshi" %% "scala-csv" % "1.3.4",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test",
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion

    )
  )

fork in run := true
javaOptions in run += "-Xmx32G"

// Show test times
testOptions in Test += Tests.Argument("-oD")

logLevel in run := Level.Warn
