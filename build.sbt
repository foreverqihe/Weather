name := "WeatherData"
version := "1.0"
scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation")


libraryDependencies += "org.json4s" %% "json4s-native" % "3.4.0"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.4.0"