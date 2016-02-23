lazy val root = (project in file(".")).
	settings(
		name := "fpscala",
		version := "0.9",
		scalaVersion := "2.11.7"
	)

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

val scalazVersion = "7.2.0"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "io.spray" %%  "spray-json" % "1.3.2",
  "io.gatling.uncommons.maths" % "uncommons-maths" % "1.2.3",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
  )
  
initialCommands in console := "import scalaz._, Scalaz._"
fork in run := true