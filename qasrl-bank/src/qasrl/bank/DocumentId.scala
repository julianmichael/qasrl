package qasrl.bank

import cats.Order
import cats.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class DocumentId(domain: Domain, id: String)

object DocumentId {
  implicit val documentIdOrder = Order.whenEqual(
    Order.by[DocumentId, Domain](_.domain),
    Order.by[DocumentId, String](_.id)
  )
}
