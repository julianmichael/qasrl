// package qasrl.bank.service

// import qasrl.bank.DataIndex
// import qasrl.bank.Document
// import qasrl.bank.DocumentId

// import cats.Monad
// import cats.~>
// import cats.Id
// import cats.implicits._

// import io.circe.parser.decode
// import io.circe.Encoder

// import scala.collection.mutable
// import scala.concurrent.Future
// import scala.concurrent.ExecutionContext.Implicits.global

// case class WebClientDocumentServiceInterpreter(
//   apiUrl: String
// ) extends (DocumentService.Request ~> Future) {
//   import DocumentService._

//   import qasrl.bank.JsonCodecs._
//   import qasrl.bank.service.JsonCodecs._
//   import io.circe.syntax._
//   val printer = io.circe.Printer.noSpaces

//   def apply[A](req: Request[A]): Future[A] = {

//     req match {
//       case GetDataIndex =>
//         get(req).map(_.responseText).flatMap { dataIndexJsonStr =>
//           decode[DataIndex](dataIndexJsonStr) match {
//             case Left(err)    => Future.failed[DataIndex](new RuntimeException(err))
//             case Right(index) => Future.successful(index)
//           }
//         }
//       case GetDocument(id) => {
//         get(GetDocument(id)).map(_.responseText).flatMap { documentJsonStr =>
//           decode[Document](documentJsonStr) match {
//             case Left(err)       => Future.failed[Document](new RuntimeException(err))
//             case Right(document) => Future.successful(document)
//           }
//         }
//       }
//       case SearchDocuments(query) => {
//         postSearch(query).map(_.responseText).flatMap { documentIdSetJsonStr =>
//           decode[Set[DocumentId]](documentIdSetJsonStr) match {
//             case Left(err)          => Future.failed[Set[DocumentId]](new RuntimeException(err))
//             case Right(documentIds) => Future.successful(documentIds)
//           }
//         }
//       }
//     }
//   }

//   private[this] def get[A](req: Request[A]) = {
//     import scala.concurrent.ExecutionContext.Implicits.global
//     org.scalajs.dom.ext.Ajax.get(url = apiUrl + "/" + getRoute(req))
//   }
//   private[this] def postSearch(query: Search.Query) = {
//     import scala.concurrent.ExecutionContext.Implicits.global
//     org.scalajs.dom.ext.Ajax.post(url = apiUrl + "/search_full", data = printer.pretty(query.asJson))
//   }

//   private[this] def getRoute[A](req: Request[A]): String = req match {
//     case GetDataIndex           => s"index"
//     case GetDocument(id)        => s"doc/${id.domain}/${id.id}"
//     case SearchDocuments(query) => s"search/${query.keywords.mkString(" ")}"
//   }
// }

// class WebClientDocumentService(
//   apiUrl: String
// ) extends InterpretedDocumentService[Future](
//       WebClientDocumentServiceInterpreter(apiUrl)
//     )
