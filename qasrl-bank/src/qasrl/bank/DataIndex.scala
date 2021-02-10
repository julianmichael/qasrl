package qasrl.bank

import scala.collection.immutable.SortedSet
import io.circe.generic.JsonCodec

import cats.Order.catsKernelOrderingForOrder

case class DataIndex(
  documents: Map[DatasetPartition, SortedSet[DocumentMetadata]],
  denseIds: SortedSet[SentenceId],
  qaNomIds: SortedSet[SentenceId] = SortedSet() // XXX
) {
  lazy val allDocumentMetas = documents.values.reduce(_ union _)
  lazy val allDocumentIds = allDocumentMetas.map(_.id)
  def numDocuments = allDocumentMetas.size
  def getPart(id: DocumentId): DatasetPartition = documents.find(_._2.exists(_.id == id)).get._1
}
object DataIndex {
  import io.circe.{Encoder, Decoder}
  import io.circe.HCursor
  implicit val dataIndexEncoder: Encoder[DataIndex] = {
    io.circe.generic.semiauto.deriveEncoder[DataIndex]
  }
  implicit val dataIndexDecoder: Decoder[DataIndex] = new Decoder[DataIndex] {
    override def apply(c: HCursor): Decoder.Result[DataIndex] = for {
      documents <- c.downField("documents").as[Map[DatasetPartition, SortedSet[DocumentMetadata]]]
      denseIds <- c.downField("denseIds").as[SortedSet[SentenceId]]
      qaNomIds = c.downField("qaNomIds").as[SortedSet[SentenceId]].getOrElse(SortedSet[SentenceId]())
    } yield DataIndex(documents, denseIds, qaNomIds)
  }
}
