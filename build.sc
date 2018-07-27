import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import ammonite.ops._
import coursier.maven.MavenRepository

val thisPublishVersion = "0.1.1-SNAPSHOT"

val scalaVersions = List("2.11.12", "2.12.6")
val thisScalaJSVersion = "0.6.23"

val macroParadiseVersion = "2.1.0"
val kindProjectorVersion = "0.9.4"

// cats and react libs -- make sure versions match up
val catsVersion = "1.1.0"
val scalajsReactVersion = "1.2.3"
val spacroVersion = "0.2.0"
val nlpdataVersion = "0.2.0"
val circeVersion = "0.9.3"
val monocleVersion = "1.5.1-cats"

// end cats libs

val upickleVersion = "0.5.1"

val akkaActorVersion = "2.4.20"
val scalaLoggingVersion = "3.5.0"
val slf4jApiVersion = "1.7.21"

val scalajsDomVersion = "0.9.6"
val scalajsJqueryVersion = "0.9.3"
val scalajsScalaCSSVersion = "0.5.3"

trait JvmPlatform {
  def platformSegment: String = "jvm"
}

trait JsPlatform extends ScalaJSModule {
  def platformSegment: String = "js"
  def scalaJSVersion = thisScalaJSVersion
}

trait CommonModule extends CrossScalaModule with ScalafmtModule {

  def platformSegment: String

  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds",
    "-Ypartial-unification"
  )

  def ivyDeps = Agg(
    ivy"org.julianmichael::nlpdata::$nlpdataVersion",
    ivy"org.typelevel::cats-core::$catsVersion",
    ivy"com.github.julien-truffaut::monocle-core::$monocleVersion",
    ivy"com.github.julien-truffaut::monocle-macro::$monocleVersion",
    ivy"io.circe::circe-core::$circeVersion",
    ivy"io.circe::circe-generic::$circeVersion",
    ivy"io.circe::circe-parser::$circeVersion"
  )

  def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    ivy"org.scalamacros:::paradise:$macroParadiseVersion",
    ivy"org.spire-math::kind-projector:$kindProjectorVersion"
  )

}

trait CommonPublishModule extends CommonModule with PublishModule {
  def publishVersion = thisPublishVersion
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "org.julianmichael",
    url = "https://github.com/julianmichael/qasrl-crowdsourcing",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("julianmichael", "qasrl-crowdsourcing"),
    developers = Seq(
      Developer("julianmichael", "Julian Michael","https://github.com/julianmichael")
    )
  )
}

trait QasrlModule extends CommonPublishModule {
  def millSourcePath = build.millSourcePath / "qasrl"
  def artifactName = "qasrl"
}

object qasrl extends Module {
  object jvm extends Cross[QasrlJvmModule](scalaVersions: _*)
  class QasrlJvmModule(val crossScalaVersion: String) extends QasrlModule with JvmPlatform
  object js extends Cross[QasrlJsModule](scalaVersions: _*)
  class QasrlJsModule(val crossScalaVersion: String) extends QasrlModule with JsPlatform
}

trait QasrlCrowdModule extends CommonPublishModule {
  def millSourcePath = build.millSourcePath / "qasrl-crowd"
  def artifactName = "qasrl-crowd"
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.julianmichael::spacro::$spacroVersion",
    ivy"com.lihaoyi::upickle::$upickleVersion"
  )
}

object `qasrl-crowd` extends Module {
  object jvm extends Cross[QasrlCrowdJvmModule](scalaVersions: _*)
  class QasrlCrowdJvmModule(val crossScalaVersion: String) extends QasrlCrowdModule with JvmPlatform {
    def moduleDeps = Seq(qasrl.jvm())
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.typesafe.akka::akka-actor::$akkaActorVersion",
      ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
      ivy"org.slf4j:slf4j-api:$slf4jApiVersion" // decided to match scala-logging transitive dep
    )
  }
  object js extends Cross[QasrlCrowdJSModule](scalaVersions: _*)
  class QasrlCrowdJSModule(val crossScalaVersion: String) extends QasrlCrowdModule with JsPlatform {
    def moduleDeps = Seq(qasrl.js())
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
      ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
      ivy"com.github.japgolly.scalajs-react::core::$scalajsReactVersion",
      ivy"com.github.japgolly.scalajs-react::ext-monocle-cats::$scalajsReactVersion",
      ivy"com.github.japgolly.scalajs-react::ext-cats::$scalajsReactVersion",
      ivy"com.github.japgolly.scalacss::core::$scalajsScalaCSSVersion",
      ivy"com.github.japgolly.scalacss::ext-react::$scalajsScalaCSSVersion"
    )
  }
}
