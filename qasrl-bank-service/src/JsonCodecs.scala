// package qasrl.bank.service
// 
// import jjm.implicits._
// 
// import qasrl.bank.DocumentId
// 
// import io.circe.{Decoder, Encoder}
// 
// object JsonCodecs {
//   import JsonCodecsContainer._
//   implicit lazy val documentIdSetEncoder = derivedDocumentIdSetEncoder
//   implicit lazy val documentIdSetDecoder = derivedDocumentIdSetDecoder
// }
// 
// private[service] object JsonCodecsContainer {
// 
//   val derivedDocumentIdSetEncoder: Encoder[collection.immutable.Set[DocumentId]] = {
//     import io.circe.generic.auto._
//     implicitly[Encoder[collection.immutable.Set[DocumentId]]]
//   }
// 
//   val derivedDocumentIdSetDecoder: Decoder[collection.immutable.Set[DocumentId]] = {
//     import cats.Order.catsKernelOrderingForOrder
//     import io.circe.generic.auto._
//     implicitly[Decoder[collection.immutable.Set[DocumentId]]]
//   }
// }
