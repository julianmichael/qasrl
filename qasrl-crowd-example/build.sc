import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import ammonite.ops._
import coursier.maven.MavenRepository

val thisScalaVersion = "2.12.13"
val thisScalaJSVersion = "0.6.33"

val macroParadiseVersion = "2.1.1"
val kindProjectorVersion = "0.11.3"

val jjmVersion = "0.1.0"
val qasrlVersion = "0.2.0"

val corenlpVersion = "3.6.0"

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

trait ExampleModule extends ScalaModule with ScalafmtModule {

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

  def ivyDeps = Agg(
    ivy"org.julianmichael::qasrl::$qasrlVersion",
    ivy"org.julianmichael::qasrl-crowd::$qasrlVersion",
  )

  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"org.scalamacros:::paradise:$macroParadiseVersion",
    ivy"org.typelevel:::kind-projector:$kindProjectorVersion"
  )
}

object example extends Module {
  object jvm extends ExampleModule with JvmPlatform {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.julianmichael::jjm-corenlp::$jjmVersion",
      // TODO can I get this in as a transitive dep?
      ivy"edu.stanford.nlp:stanford-corenlp:$corenlpVersion".configure(
        coursier.core.Attributes(`type` = coursier.core.Type(""), classifier = coursier.core.Classifier("models"))
      ),
      ivy"ch.qos.logback:logback-classic:1.2.3"
    )
    def resources = T.sources(
      millSourcePath / "resources",
      example.js.fastOpt().path / RelPath.up,
      example.js.aggregatedJSDeps() / RelPath.up
    )
  }
  object js extends ExampleModule with JsPlatform with SimpleJSDeps {
    def mainClass = T(Some("example.Dispatcher"))

    def jsDeps = Agg(
      "https://code.jquery.com/jquery-2.1.4.min.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react.js",
      "https://cdnjs.cloudflare.com/ajax/libs/react/15.6.1/react-dom.js"
    )
  }
}
