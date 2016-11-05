
name := "Sierpinski"
version := "0.1"
scalaVersion := "2.10.3"

enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.0"
)


mainClass := Some("sierpinski.Sierpinski")
