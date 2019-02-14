name := "chessgraph"

version := "0.1"

scalaVersion := "2.12.7"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.platanios" % "tensorflow_2.12" % "0.4.0" classifier "linux-cpu-x86_64"
