name := "scala-bootcamp-assigment-poker"

version := "1.0"
scalaVersion := "2.13.4"

assemblyJarName in assembly := "poker.jar"

libraryDependencies += "io.monix" %% "monix" % "3.3.0"

libraryDependencies += "org.scalatest" %% "scalatest"          % "3.2.2" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.2" % "test"