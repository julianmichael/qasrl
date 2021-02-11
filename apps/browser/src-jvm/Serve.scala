package qasrl.apps.browser

import cats.Id
import cats.~>
import cats.data.NonEmptySet
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._

import fs2.Stream

import java.nio.file.Path

import qasrl.bank.Data
import qasrl.bank.service.Search
import qasrl.bank.service.DocumentService

import jjm.io.HttpUtil

import scala.concurrent.duration._
import io.circe.syntax._

import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._


import com.monovore.decline._
import com.monovore.decline.effect._
import java.nio.file.Paths

object Serve extends CommandIOApp(
  name = "mill -i browser.jvm.runMain qasrl.apps.browser.Serve",
  header = "Spin up the data server for the QA-SRL Bank browser webapp.",
  version = "0.3.0") {

  def main: Opts[IO[ExitCode]] = {

    val jsDepsPathO = Opts.option[Path](
      "jsDeps", metavar = "path", help = "Where to get the JS deps file."
    )

    val jsPathO = Opts.option[Path](
      "js", metavar = "path", help = "Where to get the JS main file."
    )

    val qasrlBankO = Opts.option[Path](
      "qasrl-bank", metavar = "path", help = "Path to the QA-SRL Bank 2.0 data, e.g., ../qasrl-bank/data/qasrl-v2."
    ).withDefault(Paths.get("data/qasrl-v2_1"))
    //.
      // .or()

    val qaNomO = Opts.option[Path](
      "qanom", metavar = "path", help = "Path to the QA-Nom data."
    ).withDefault(Paths.get("data/qanom"))

    val domainO = Opts.option[String](
      "domain", metavar = "domain", help = "domain name the server is being hosted at."
    )

    val portO = Opts.option[Int](
      "port", metavar = "port number", help = "Port to host the HTTP service on."
    )

    val proxyO = Opts.flag(
      "proxy", help = "Whether the server is behind a public HTTPS reverse proxy."
    ).orFalse

    (jsPathO, jsDepsPathO, qasrlBankO, qaNomO, portO, domainO, proxyO).mapN {
      case (jsPath, jsDepsPath, qasrlBankPath, qaNomPath, port, domain, behindProxy) =>

        val portOpt = if(behindProxy) None else Some(port)
        val useHttps = behindProxy
        val dataMetaIndexPath = qasrlBankPath.resolve("index.json.gz")

        import scala.concurrent.ExecutionContext.Implicits.global

        for {
          qasrlData <- IO.fromTry(Data.readFromQasrlBank(qasrlBankPath))
          allData <- IO.fromTry(Data.loadQANomData(qasrlData.small, qaNomPath))
          index = allData.index
          indexStr = "var dataMetaIndex = " + index.asJson.noSpaces + ";"
          pageService = StaticPageService.makeService(
            domain,
            jsDepsPath, jsPath,
            indexStr,
            portOpt, useHttps
          )
          docs = allData.documentsById
          searchIndex = Search.createSearchIndex(docs.values.toList)
          basicService = DocumentService.basic(index, docs, searchIndex)
          docService = HttpUtil.makeHttpPostServer[IO, DocumentService.Request](
            basicService.andThenK(Lambda[Id ~> IO](IO.pure(_)))
          )
          app = Router(
            "/" -> pageService,
            s"/${SharedConstants.docApiSuffix}" -> docService
          ).orNotFound
          _ <- BlazeServerBuilder[IO](global)
          .withIdleTimeout(5.minutes)
          .bindHttp(port, "0.0.0.0")
          .withHttpApp(app)
          .serve.compile.drain
        } yield ExitCode.Success
    }
  }
}
