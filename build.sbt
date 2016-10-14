lazy val commonSettings = Seq(
  version := "0.1.0",
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "karme",
    libraryDependencies ++= Seq(
      "org.ddahl" % "rscala_2.11" % "1.0.13",
      "com.github.tototoshi" %% "scala-csv" % "1.3.3",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    )
  )
