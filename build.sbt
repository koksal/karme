name := "karme"
version := "1.0"
scalaVersion := "2.11.7"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.2.1"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

resolvers += Resolver.sonatypeRepo("public")
