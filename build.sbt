name := "ScalaByTheBay"

version := "0.0.1"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.0.0",
   "org.scala-lang" % "scala-library" % scalaVersion.value,
   "org.scala-lang" % "scala-compiler" % scalaVersion.value % "scala-tool",
   "org.scala-lang" % "scala-reflect" % scalaVersion.value
)
