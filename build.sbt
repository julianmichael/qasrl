val scalaJSReactVersion = "1.1.0"
val monocleVersion = "1.4.0-M2"

lazy val root = project.in(file("."))
  .aggregate(qasrlJVM, qasrlJS, crowdJVM, crowdJS)
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
  libraryDependencies += "org.typelevel" %% "cats" % "0.9.0",
  libraryDependencies += "com.github.julien-truffaut" %%% "monocle-core"  % monocleVersion,
  libraryDependencies += "com.github.julien-truffaut" %%% "monocle-macro" % monocleVersion
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
  libraryDependencies += "com.github.julianmichael" %%% "spacro" % "0.1-SNAPSHOT",
  libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.3"
).jvmSettings(commonJVMSettings).jvmSettings(
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.4.8",
    // "com.typesafe.akka" %% "akka-http-experimental" % "2.4.9",
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

// lazy val exampleProjectSettings = commonSettings ++ Seq(
//   libraryDependencies += "com.github.uwnlp" %%% "qamr-example" % "0.1-SNAPSHOT",
//   libraryDependencies += "com.github.uwnlp" %%% "qamr-analysis" % "0.1-SNAPSHOT"
// )

// lazy val exampleProjectJVMSettings = commonJVMSettings ++ Seq(
//   libraryDependencies ++= Seq(
//     "com.lihaoyi" %% "scalatags" % "0.6.5",
//     "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
//     // java deps:
//     "org.slf4j" % "slf4j-api" % "1.7.21", // decided to match scala-logging transitive dep
//     "ch.qos.logback" % "logback-classic" % "1.2.3",
//     "log4j" % "log4j" % "1.2.17", // runtime error if not included?
//     "ca.juliusdavies" % "not-yet-commons-ssl" % "0.3.11",  // runtime error if not included?
//     "xerces" % "xercesImpl" % "2.9.1" // runtime error if not included?
//   )
// )

// lazy val exampleProjectJSSettings = commonJSSettings

// lazy val emnlp2017 = crossProject.in(file("example/emnlp2017"))
//   .settings(name := "turksem-emnlp2017", version := "0.1-SNAPSHOT")
//   .settings(exampleProjectSettings)
//   .jvmSettings(exampleProjectJVMSettings)
//   .jvmSettings(libraryDependencies += "io.argonaut" %% "argonaut" % "6.1")
//   .jsSettings(exampleProjectJSSettings)

// lazy val emnlp2017JS = emnlp2017.js.dependsOn(turksemJS)
// lazy val emnlp2017JVM = emnlp2017.jvm.dependsOn(turksemJVM).settings(
//   (resources in Compile) += (fastOptJS in (emnlp2017JS, Compile)).value.data,
//   (resources in Compile) += (packageScalaJSLauncher in (emnlp2017JS, Compile)).value.data,
//   (resources in Compile) += (packageJSDependencies in (emnlp2017JS, Compile)).value
// )
