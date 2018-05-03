val catsVersion = "0.9.0"
val scalaJSReactVersion = "1.1.0"
val monocleVersion = "1.4.0-M2"
val circeVersion = "0.8.0"

lazy val root = project.in(file("."))
  .aggregate(qasrlJVM, qasrlJS, crowdJVM, crowdJS, exampleJVM, exampleJS)
  .settings(
  publish := {},
  publishLocal := {})

lazy val commonSettings = Seq(
  organization := "com.github.julianmichael",
  scalaOrganization in ThisBuild := "org.typelevel",
  scalaVersion in ThisBuild := "2.11.8",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds"/*, "-Ypartial-unification"*/),
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  libraryDependencies += "com.github.julianmichael" %%% "nlpdata" % "0.1-SNAPSHOT",
  libraryDependencies += "org.typelevel" %% "cats" % catsVersion,
  libraryDependencies += "com.github.julien-truffaut" %%% "monocle-core"  % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut" %%% "monocle-macro" % monocleVersion,
  libraryDependencies += "io.circe" %% "circe-core" % circeVersion,
  libraryDependencies += "io.circe" %% "circe-generic" % circeVersion,
  libraryDependencies += "io.circe" %% "circe-parser" % circeVersion
)

lazy val commonJVMSettings = Seq()

lazy val commonJSSettings = Seq(
  relativeSourceMaps := true,
  scalaJSStage in Global := FastOptStage,
  persistLauncher in Compile := true,
  persistLauncher in Test := false,
  skip in packageJSDependencies := false)

lazy val qasrl = crossProject.in(file("qasrl"))
  .settings(commonSettings).settings(
  name := "qasrl",
  version := "0.1-SNAPSHOT"
).jvmSettings(
  commonJVMSettings
).jsSettings(
  commonJSSettings
)

lazy val qasrlJS = qasrl.js
lazy val qasrlJVM = qasrl.jvm

lazy val crowd = crossProject.in(file("qasrl-crowd"))
  .settings(commonSettings).settings(
  name := "qasrl-crowd",
  version := "0.1-SNAPSHOT",
  libraryDependencies ++= Seq(
    "com.github.julianmichael" %%% "spacro" % "0.1-SNAPSHOT",
    "com.lihaoyi" %%% "upickle" % "0.4.3")
).jvmSettings(commonJVMSettings).jvmSettings(
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.4.8",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
    // java deps:
    "org.slf4j" % "slf4j-api" % "1.7.21", // decided to match scala-logging transitive dep
    "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0",
    "edu.stanford.nlp" % "stanford-corenlp" % "3.6.0" classifier "models" // for automatically downloading pos-tagging model
  )
).jsSettings(commonJSSettings).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
    "com.github.japgolly.scalajs-react" %%% "core" % scalaJSReactVersion,
    "com.github.japgolly.scalajs-react" %%% "ext-monocle" % scalaJSReactVersion,
    "com.github.japgolly.scalajs-react" %%% "ext-cats" % scalaJSReactVersion,
    "com.github.japgolly.scalacss" %%% "core" % "0.5.3",
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.3"
  ),
  jsDependencies ++= Seq(
    RuntimeDOM,
    "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js",

    "org.webjars.bower" % "react" % "15.6.1"
      /        "react-with-addons.js"
      minified "react-with-addons.min.js"
      commonJSName "React",

    "org.webjars.bower" % "react" % "15.6.1"
      /         "react-dom.js"
      minified  "react-dom.min.js"
      dependsOn "react-with-addons.js"
      commonJSName "ReactDOM",

    "org.webjars.bower" % "react" % "15.6.1"
      /         "react-dom-server.js"
      minified  "react-dom-server.min.js"
      dependsOn "react-dom.js"
      commonJSName "ReactDOMServer"
  )
)

lazy val crowdJS = crowd.js.dependsOn(qasrlJS)
lazy val crowdJVM = crowd.jvm.dependsOn(qasrlJVM)

lazy val example = crossProject.in(file("qasrl-crowd-example"))
  .settings(commonSettings).settings(
  name := "qasrl-crowd-example",
  version := "0.1-SNAPSHOT"
).jvmSettings(commonJVMSettings).jvmSettings(
  libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
).jsSettings(commonJSSettings)

lazy val exampleJS = example.js.dependsOn(qasrlJS, crowdJS)
lazy val exampleJVM = example.jvm.dependsOn(qasrlJVM, crowdJVM).settings(
  (resources in Compile) += (fastOptJS in (exampleJS, Compile)).value.data,
  (resources in Compile) += (packageScalaJSLauncher in (exampleJS, Compile)).value.data,
  (resources in Compile) += (packageJSDependencies in (exampleJS, Compile)).value
)
