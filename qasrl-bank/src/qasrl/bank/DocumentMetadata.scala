package qasrl.bank

import cats.Order
import cats.implicits._

import io.circe.{Encoder, Decoder}
import io.circe.Json

case class DocumentMetadata(
  id: DocumentId,
  part: DatasetPartition,
  title: String
)

object DocumentMetadata {
  implicit val documentMetadataOrder = Order.whenEqual(
    Order.by[DocumentMetadata, String](_.title),
    Order.by[DocumentMetadata, DocumentId](_.id)
  )

  implicit val documentMetadataEncoder = Encoder.instance[DocumentMetadata] { docMeta =>
    import io.circe.syntax._
    val idPrefix = docMeta.id.domain match {
      case Domain.Wikipedia => "Wiki1k:wikipedia"
      case Domain.Wikinews  => "Wiki1k:wikinews"
      case Domain.TQA       => "TQA"
    }
    val idString = idPrefix + ":" + docMeta.id.id
    Json.obj(
      "part"     -> docMeta.part.asJson,
      "idString" -> Json.fromString(idString),
      "domain"   -> docMeta.id.domain.asJson,
      "id"       -> Json.fromString(docMeta.id.id),
      "title"    -> Json.fromString(docMeta.title)
    )
  }
  implicit val documentMetadataDecoder = Decoder.instance { c =>
    for {
      part   <- c.downField("part").as[DatasetPartition]
      domain <- c.downField("domain").as[Domain]
      id     <- c.downField("id").as[String]
      title  <- c.downField("title").as[String]
    } yield DocumentMetadata(DocumentId(domain, id), part, title)
  }
}
