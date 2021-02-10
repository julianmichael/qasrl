package qasrl.bank

import cats.Order
import cats.Order.catsKernelOrderingForOrder

import scala.collection.immutable.SortedSet

import io.circe.generic.JsonCodec

@JsonCodec case class Document(
  metadata: DocumentMetadata, sentences: SortedSet[ConsolidatedSentence]
)

object Document {
  implicit val documentOrder = Order.by[Document, DocumentMetadata](_.metadata)
}
