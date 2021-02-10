package qasrl.bank.service

import jjm.io.HttpUtil

import qasrl.bank.ConsolidatedData

import cats.~>
import cats.Id
import cats.data.NonEmptySet
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import org.http4s._
import org.http4s.implicits._

import org.http4s.server.blaze._

import fs2.Stream

// import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext

object DocumentServiceWebServer {

  def serve(
    data: ConsolidatedData,
    port: Int,
    restrictedClientDomains: Option[NonEmptySet[String]] // None for no restrictions
  )(implicit cs: ContextShift[IO], t: Timer[IO], ec: ExecutionContext): Stream[IO, ExitCode] = {

    val index = data.index
    val docs = data.documentsById
    val searchIndex = Search.createSearchIndex(docs.values.toList)

    val basicService = DocumentService.basic(index, docs, searchIndex)
    val bareHttpService = HttpUtil.makeHttpPostServer[IO, DocumentService.Request](
      basicService.andThenK(Lambda[Id ~> IO](IO.pure(_)))
    )

    import org.http4s.server.middleware._
    import scala.concurrent.duration._

    val corsConfig = restrictedClientDomains match {
      case None =>
        CORSConfig(
          anyOrigin = true,
          anyMethod = false,
          allowedMethods = Some(Set("GET", "POST")),
          allowCredentials = false,
          maxAge = 1.day.toSeconds
        )
      case Some(domains) =>
        CORSConfig(
          anyOrigin = false,
          allowedOrigins = domains.toSortedSet,
          anyMethod = false,
          allowedMethods = Some(Set("GET", "POST")),
          allowCredentials = false,
          maxAge = 1.day.toSeconds
        )
    }

    val service = CORS(bareHttpService, corsConfig).orNotFound

    BlazeServerBuilder[IO](ec)
      .bindHttp(port, "0.0.0.0")
      .withHttpApp(service)
      .serve
  }

}
