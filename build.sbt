name := "diyfea"

version := "0.1"

scalaVersion := "2.10.0-RC3"

scalacOptions ++= Seq(
  "-target:jvm-1.6",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlog-reflective-calls",
  "-Ywarn-adapted-args"
)

resolvers += "Sonatype OSS" at
  "https://oss.sonatype.org/content/groups/public/"

libraryDependencies += "org.scalatest" % "scalatest_2.10.0-RC3" % "2.0.M5-B1" %
  "test"

libraryDependencies += "com.googlecode.matrix-toolkits-java" %
  "mtj" % "0.9.14"
