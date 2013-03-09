import sbt._
import Keys._

object ElevatorsBuild extends Build {

    lazy val root =
      Project(id = "elevators",
        base = file(".")).
        settings(libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test").
        settings(libraryDependencies += "org.mockito" % "mockito-core" % "1.9.5-rc1" % "test")


}
