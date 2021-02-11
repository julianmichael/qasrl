import mill._, mill.scalalib._, mill.scalalib.publish._, mill.scalajslib._
import mill.scalalib.scalafmt._
import ammonite.ops._
import coursier.maven.MavenRepository

val thisPublishVersion = "0.3.0-SNAPSHOT"
val qasrlBankPublishVersion = "0.4.0-SNAPSHOT"

val scalaVersions = List(
  "2.12.13",
  "2.13.4"
)
val thisScalaJSVersion = "1.4.0"

val macroParadiseVersion = "2.1.1"
val kindProjectorVersion = "0.11.3"
val betterMonadicForVersion = "0.3.1"

// cats and react libs -- make sure versions match up
val jjmVersion = "0.2.0-SNAPSHOT"
val spacroVersion = "0.4.0-SNAPSHOT"
val radhocVersion = "0.4.0-SNAPSHOT"

// end cats libs

val scalaLoggingVersion = "3.9.2"
val slf4jApiVersion = "1.7.30"

val scalajsDomVersion = "1.1.0"
val scalajsJqueryVersion = "1.0.0"
val scalajsScalaCSSVersion = "0.7.0"

val scalatestVersion = "3.2.2"
val scalacheckVersion = "1.14.1"
val disciplineVersion = "1.0.0"

trait JvmPlatform {
  def platformSegment: String = "jvm"
}

trait JsPlatform extends ScalaJSModule {
  def platformSegment: String = "js"
  def scalaJSVersion = thisScalaJSVersion
}

trait CommonModule extends ScalaModule with ScalafmtModule {

  def platformSegment: String

  def sources = T.sources(
    millSourcePath / "src",
    millSourcePath / s"src-$platformSegment"
  )

  def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:higherKinds"
  ) ++ (
    if(scalaVersion().startsWith("2.12")) {
      Seq("-Ypartial-unification")
    } else Seq("-Ymacro-annotations")
  )

  def ivyDeps = Agg(
    ivy"org.julianmichael::jjm-core::$jjmVersion"
  )

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(
    // ivy"io.tryp:::splain:$splainVersion",
    ivy"org.typelevel:::kind-projector:$kindProjectorVersion",
    ivy"com.olegpy::better-monadic-for:$betterMonadicForVersion"
  ) ++ (
    if(scalaVersion().startsWith("2.12")) {
      Agg(ivy"org.scalamacros:::paradise:$macroParadiseVersion")
    } else Agg()
  )

}

trait CommonPublishModule extends CommonModule with CrossScalaModule with PublishModule {
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

trait CommonQasrlBankPublishModule extends CommonPublishModule {
  def publishVersion = qasrlBankPublishVersion
}

trait QasrlModule extends CommonPublishModule {
  def millSourcePath = build.millSourcePath / "qasrl"
  def artifactName = "qasrl"

  object test extends Tests with CommonModule {
    override def scalaVersion = QasrlModule.this.scalaVersion
    def platformSegment = QasrlModule.this.platformSegment
    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:$scalatestVersion",
      ivy"org.scalacheck::scalacheck:$scalacheckVersion",
      ivy"org.typelevel::discipline-core:$disciplineVersion"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
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
    ivy"org.julianmichael::spacro::$spacroVersion"
  )
}

object `qasrl-crowd` extends Module {
  object jvm extends Cross[QasrlCrowdJvmModule](scalaVersions.filter(_.startsWith("2.12")): _*)
  class QasrlCrowdJvmModule(val crossScalaVersion: String) extends QasrlCrowdModule with JvmPlatform {
    def moduleDeps = Seq(qasrl.jvm())
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.typesafe.scala-logging::scala-logging::$scalaLoggingVersion",
      ivy"org.slf4j:slf4j-api:$slf4jApiVersion" // decided to match scala-logging transitive dep
    )
  }
  object js extends Cross[QasrlCrowdJSModule](scalaVersions.filter(_.startsWith("2.12")): _*)
  class QasrlCrowdJSModule(val crossScalaVersion: String) extends QasrlCrowdModule with JsPlatform {
    def moduleDeps = Seq(qasrl.js())
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.julianmichael::radhoc::$radhocVersion",
      ivy"org.scala-js::scalajs-dom::$scalajsDomVersion",
      ivy"be.doeraene::scalajs-jquery::$scalajsJqueryVersion",
      ivy"com.github.japgolly.scalacss::core::$scalajsScalaCSSVersion",
      ivy"com.github.japgolly.scalacss::ext-react::$scalajsScalaCSSVersion"
    )
  }
}


trait QasrlBankModule extends CommonQasrlBankPublishModule {
  def artifactName = "qasrl-bank"
  def millSourcePath = build.millSourcePath / "qasrl-bank"
}

object `qasrl-bank` extends Module {
  class Jvm(val crossScalaVersion: String) extends QasrlBankModule with JvmPlatform {
    def moduleDeps = Seq(qasrl.jvm())
  }
  object jvm extends Cross[Jvm](scalaVersions: _*)
  class Js(val crossScalaVersion: String) extends QasrlBankModule with JsPlatform {
    def moduleDeps = Seq(qasrl.js())
  }
  object js extends Cross[Js](scalaVersions: _*)
}

trait QasrlBankServiceModule extends CommonQasrlBankPublishModule {
  def artifactName = "qasrl-bank-service"
  def millSourcePath = build.millSourcePath / "qasrl-bank-service"
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.julianmichael::jjm-io::$jjmVersion"
  )
}

object `qasrl-bank-service` extends Module {
  class Jvm(val crossScalaVersion: String) extends QasrlBankServiceModule with JvmPlatform {
    def moduleDeps = Seq(`qasrl-bank`.jvm())
  }
  object jvm extends Cross[Jvm](scalaVersions: _*)
  class Js(val crossScalaVersion: String) extends QasrlBankServiceModule with JsPlatform {
    def moduleDeps = Seq(`qasrl-bank`.js())
  }
  object js extends Cross[Js](scalaVersions: _*)
}

val declineVersion = "1.3.0"
val logbackVersion = "1.2.3"
val scalaCsvVersion = "1.3.6"

trait AppJvmModule extends CommonModule with JvmPlatform {
  val version = scalaVersions.head
  def scalaVersion = T(version)
  def moduleDeps = Seq(
    qasrl.jvm(version),
    `qasrl-bank`.jvm(version),
    `qasrl-bank-service`.jvm(version))
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.monovore::decline::$declineVersion",
    ivy"com.monovore::decline-effect::$declineVersion",
    ivy"ch.qos.logback:logback-classic:$logbackVersion"
  )
}

object apps extends Module {
  object align extends AppJvmModule {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.github.tototoshi::scala-csv:$scalaCsvVersion"
    )
  }

  object resolution extends AppJvmModule

  object reprocess extends AppJvmModule
}
