name := "brbo2-impl"

version := "0.1"

scalaVersion := "2.12.12"

// libraryDependencies += "org.checkerframework" % "checker" % "3.12.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.14.0"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.14.0"

libraryDependencies += "commons-io" % "commons-io" % "2.5"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "com.ibm.wala" % "com.ibm.wala.util" % "1.5.5"

libraryDependencies += "args4j" % "args4j" % "2.33"

libraryDependencies += "org.jgrapht" % "jgrapht-core" % "1.5.1"
// libraryDependencies += "org.jgrapht" % "jgrapht-ext" % "1.5.1"
libraryDependencies += "org.jgrapht" % "jgrapht-io" % "1.5.1"

// Add tools.jar such that sbt can find it
unmanagedJars in Compile ~= {
  uj: Classpath =>
    Seq(
      Attributed.blank(file(System.getProperty("java.home").dropRight(3) + "lib/tools.jar")),
    ) ++ uj
}
// https://stackoverflow.com/questions/12409847/how-to-add-tools-jar-as-a-dynamic-dependency-in-sbt-is-it-possible/12508163

val nativeLibraryPath = {
  val currentDirectory = System.getProperty("user.dir")
  s"$currentDirectory/lib/z3"
}

// To avoid "javaOptions will be ignored, fork is set to false": https://github.com/sbt/sbt/issues/3832
fork in(Test, test) := true

javaOptions in Test += s"-Djava.library.path=$nativeLibraryPath"
javaOptions in Runtime += s"-Djava.library.path=$nativeLibraryPath"

// To avoid errors when running `sbt test`: https://stackoverflow.com/a/45173411
testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-P1")