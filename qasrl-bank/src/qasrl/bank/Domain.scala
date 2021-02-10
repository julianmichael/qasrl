package qasrl.bank

import jjm.LowerCaseString
import jjm.implicits._

import cats.Order
import cats.implicits._

import io.circe.{Encoder, Decoder}
import io.circe.Json
import io.circe.DecodingFailure

sealed trait Domain {
  import Domain._
  override def toString: String = this match {
    case Wikipedia => "wikipedia"
    case Wikinews  => "wikinews"
    case TQA       => "TQA"
  }
}

object Domain {
  case object Wikipedia extends Domain
  case object Wikinews extends Domain
  case object TQA extends Domain

  implicit val domainOrder = Order.by[Domain, Int] {
    case Wikipedia => 0
    case Wikinews  => 1
    case TQA       => 2
  }

  def fromString(s: LowerCaseString): Option[Domain] = s.toString match {
    case "wikipedia" => Some(Wikipedia)
    case "wikinews"  => Some(Wikinews)
    case "tqa"       => Some(TQA)
  }

  implicit val domainEncoder = Encoder.instance[Domain](
    domain => Json.fromString(domain.toString.toLowerCase)
  )
  implicit val domainDecoder = Decoder.instance(
    c =>
    c.as[String]
      .right
      .flatMap(
        str =>
        Domain.fromString(str.lowerCase) match {
          case Some(part) => Right(part)
          case None       => Left(DecodingFailure("Failed to parse domain value", c.history))
        }
      )
  )

}
