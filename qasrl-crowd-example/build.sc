import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import ammonite.ops._
import coursier.maven.MavenRepository

val thisScalaVersion = "2.11.12"
val thisScalaJSVersion = "0.6.23"

val macroParadiseVersion = "2.1.0"
val kindProjectorVersion = "0.9.4"

val nlpdataVersion = "0.1.0-SNAPSHOT"
val spacroVersion = "0.1.0-SNAPSHOT"
val qasrlVersion = "0.1.0-SNAPSHOT"
val qasrlCrowdVersion = "0.1.0-SNAPSHOT"

val catsVersion = "0.9.0"
val upickleVersion = "0.4.4"
val monocleVersion = "1.4.0"
val circeVersion = "0.8.0"

val akkaActorVersion = "2.4.20"
val scalaLoggingVersion = "3.5.0"
val corenlpVersion = "3.6.0"
val slf4jApiVersion = "1.7.21"

val scalajsDomVersion = "0.9.6"
val scalajsJqueryVersion = "0.9.3"
val scalajsReactVersion = "1.1.0"
val scalajsScalaCSSVersion = "0.5.3"

trait JvmPlatform {
  def platformSegment: String = "jvm"
}

trait JsPlatform extends ScalaJSModule {
  def platformSegment: String = "js"
  def scalaJSVersion = thisScalaJSVersion
}

trait SimpleJSDeps extends Module {
  def jsDeps = T { Agg.empty[String] }
  def downloadedJSDeps = T {
    for(url <- jsDeps()) yield {
      val filename = url.substring(url.lastIndexOf("/") + 1)
        %("curl", "-o", filename, url)(T.ctx().dest)
      T.ctx().dest / filename
    }
  }
  def aggregatedJSDeps = T {
    val targetPath = T.ctx().dest / "jsdeps.js"
    downloadedJSDeps().foreach { path =>
      write.append(targetPath, read!(path))
      write.append(targetPath, "\n")
    }
    targetPath
  }
}

trait ExampleModule extends ScalaModule {

  def scalaVersion = thisScalaVersion

  def platformSegment: String

  def millSourcePath = build.millSourcePath / "example"

  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds"
  )

  // TODO remove
  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )

  def ivyDeps = Agg(
    ivy"org.julianmichael::nlpdata::$nlpdataVersion",
    ivy"org.julianmichael::spacro::$spacroVersion",
    ivy"org.julianmichael::qasrl::$qasrlVersion",
    ivy"org.julianmichael::qasrl-crowd::$qasrlCrowdVersion",
    ivy"org.typelevel::cats::$catsVersion",
    ivy"com.github.julien-truffaut::monocle-core::$monocleVersion",
    ivy"com.github.julien-truffaut::monocle-macro::$monocleVersion",
    ivy"io.circe::circe-core::$circeVersion",
    ivy"io.circe::circe-generic::$circeVersion",
    ivy"io.circe::circe-parser::$circeVersion",
    ivy"com.lihaoyi::upickle::$upickleVersion"
  )

  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"org.scalamacros:::paradise:$macroParadiseVersion",
    ivy"org.spire-math::kind-projector:$kindProjectorVersion"
  )
}

object example extends Module {
  object jvm extends ExampleModule with JvmPlatform {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.typesafe.akka::akka-actor::$akkaActorVersion",
      ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
      ivy"org.slf4j:slf4j-api:$slf4jApiVersion", // decided to match scala-logging transitive dep
      ivy"edu.stanford.nlp:stanford-corenlp:$corenlpVersion",
      // interp.load.ivy("edu.stanford.nlp" % "stanford-corenlp" % s"$corenlpVersion" classifier "models") // for automatically downloading pos-tagging model
      ivy"ch.qos.logback:logback-classic:1.2.3"
      )
    def resources = T.sources(
      millSourcePath / "resources",
      example.js.fastOpt().path / RelPath.up,
      example.js.aggregatedJSDeps() / RelPath.up
    )
  }
  object js extends ExampleModule with JsPlatform with SimpleJSDeps {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
      ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
      ivy"com.github.japgolly.scalajs-react::core::$scalajsReactVersion",
      ivy"com.github.japgolly.scalajs-react::ext-monocle::$scalajsReactVersion",
      ivy"com.github.japgolly.scalajs-react::ext-cats::$scalajsReactVersion",
      ivy"com.github.japgolly.scalacss::core::$scalajsScalaCSSVersion",
      ivy"com.github.japgolly.scalacss::ext-react::$scalajsScalaCSSVersion"
    )

    def mainClass = T(Some("example.Dispatcher"))

    def jsDeps = Agg(
      "https://code.jquery.com/jquery-2.1.4.min.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react-dom.js"
    )
  }
}
