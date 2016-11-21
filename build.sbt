name := "mephi-functional-programming"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.bintrayRepo("megamsys", "scala")

libraryDependencies ++= Seq (
    "org.specs2" %% "specs2-core" % "3.8.5" % "test",
    "io.megam" %% "newman" % "1.3.12"
)

