package qasrl.bank.service

import qasrl.bank.DataIndex
import qasrl.bank.Document
import qasrl.bank.DocumentId

import qasrl.data.Sentence

import jjm.{DotEncoder, DotDecoder}
import jjm.DotKleisli

import cats.Id
import cats.implicits._

import io.circe.{Encoder, Decoder}
import io.circe.generic.JsonCodec

// TODO decide on the best way to use this syntactically & all

case class DocumentService[M[_]](f: DotKleisli[M, DocumentService.Request]) {
  import DocumentService._
  def getDataIndex: M[DataIndex] = f(GetDataIndex)
  def getDocument(id: DocumentId): M[Document] = f(GetDocument(id))
  def searchDocuments(query: Search.Query): M[Set[DocumentId]] = f(SearchDocuments(query))
}
object DocumentService {
  @JsonCodec sealed trait Request { type Out }
  case object GetDataIndex extends Request {
    type Out = DataIndex
  }
  @JsonCodec final case class GetDocument(id: DocumentId) extends Request {
    type Out = Document
  }
  @JsonCodec final case class SearchDocuments(query: Search.Query) extends Request {
    type Out = Set[DocumentId]
  }
  object Request {
    implicit val documentServiceRequestDotEncoder: DotEncoder[Request] = new DotEncoder[Request] {
      def apply(req: Request): Encoder[req.Out] = _apply[req.Out](req)
      @scala.annotation.nowarn
      private[this] def _apply[A](req: Request { type Out = A }): Encoder[A] = req match {
        case GetDataIndex => implicitly[Encoder[DataIndex]]
        case GetDocument(_) => implicitly[Encoder[Document]] // TODO why does it say unreachable code?
        case SearchDocuments(_) => implicitly[Encoder[Set[DocumentId]]]
      }
    }
    implicit val documentServiceRequestDotDecoder: DotDecoder[Request] = new DotDecoder[Request] {
      def apply(req: Request): Decoder[req.Out] = _apply[req.Out](req)
      @scala.annotation.nowarn
      private[this] def _apply[A](req: Request { type Out = A }): Decoder[A] = req match {
        case GetDataIndex => implicitly[Decoder[DataIndex]]
        case GetDocument(_) => implicitly[Decoder[Document]] // TODO why does it say unreachable code?
        case SearchDocuments(_) => implicitly[Decoder[Set[DocumentId]]]
      }
    }
  }

  def basic(
    index: DataIndex,
    documents: Map[DocumentId, Document],
    searchIndex: Search.Index
  ) = new DotKleisli[Id, DocumentService.Request] {
    def apply(req: Request): req.Out = _apply[req.Out](req)
    @scala.annotation.nowarn
    private[this] def _apply[A](req: Request { type Out = A }): A = req match {
      case GetDataIndex    => index
      case GetDocument(id) => documents(id) // TODO why does it say this is unreachable code?
      case SearchDocuments(query) => Search.execute(query, searchIndex, documents)
    }
  }
}
