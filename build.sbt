lazy val root = (project in file(".")).
	settings(
		name := "fpscala",
		version := "0.9",
		scalaVersion := "2.11.7"
	)

scalacOptions := Seq("-unchecked", "-deprecation", "-feature", "-encoding", "utf8")

val scalazVersion = "7.1.3"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "io.spray" %%  "spray-json" % "1.3.2",
  "org.uncommons.maths" % "uncommons-maths" % "1.2.2" exclude("jfree","jfreechart"),
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
  )
  
initialCommands in console := "import scalaz._, Scalaz._"

fork in run := true