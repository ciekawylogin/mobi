name := "mobi2"

version := "0.1"

scalaVersion := "2.11.2"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases" at "http://oss.sonatype.org/content/repositories/releases"
)
libraryDependencies += "com.github.bruneli.scalaopt" % "scalaopt-core_2.11" % "0.2"