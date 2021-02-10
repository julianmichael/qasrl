// package qasrl.bank.service

// import qasrl.bank.DataIndex
// import qasrl.bank.Document
// import qasrl.bank.DocumentId
// import qasrl.bank.Domain

// import nlpdata.util.LowerCaseStrings._

// import cats.implicits._
// import cats.effect._

// import org.http4s._
// import org.http4s.implicits._

// import scala.concurrent.ExecutionContext.Implicits.global

// object HttpDocumentService {

//   def makeService(
//     index: DataIndex,
//     documents: Map[DocumentId, Document],
//     searchIndex: Search.Index
//   ) = {

//     import qasrl.bank.JsonCodecs._
//     import qasrl.bank.service.JsonCodecs._
//     import io.circe.syntax._
//     import org.http4s.dsl.io._
//     import org.http4s.circe._

//     implicit val searchQueryEntityDecoder = jsonOf[IO, Search.Query]
//     implicit val docIdSetEntityEncoder = jsonEncoderOf[IO, Set[DocumentId]]
//     // implicit val clauseResolutionEncoder = jsonEncoderOf[IO, ClauseResolution]
//     // implicit val clauseChoiceOptEncoder = jsonEncoderOf[IO, Option[ClauseChoice]]

//     HttpRoutes.of[IO] {
//       case GET -> Root / "index" =>
//         Ok(index.asJson)
//       case GET -> Root / "doc" / domain / id =>
//         Ok(documents(DocumentId(Domain.fromString(domain.lowerCase).get, id)).asJson)
//       case GET -> Root / "search" / query =>
//         val keywords = query.split(" ").map(_.trim).filter(_.nonEmpty).map(_.lowerCase).toSet
//         if (keywords.isEmpty) {
//           Ok(documents.keySet.asJson)
//         } else {
//           val results: Set[DocumentId] = keywords
//             .map(w => searchIndex.keyword.get(w).getOrElse(Set.empty[DocumentId]))
//             .reduce(_ intersect _)
//           Ok(results.asJson)
//         }
//       case req @ POST -> Root / "search_full" =>
//         req.as[Search.Query].map(q => Search.execute(q, searchIndex, documents)).flatMap(Ok(_))
//     }
//   }
// }
