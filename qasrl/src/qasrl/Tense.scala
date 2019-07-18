package qasrl

import jjm.LowerCaseString
import jjm.implicits._

import io.circe.{Encoder, Decoder}

sealed trait Tense
case class Modal(modalVerb: LowerCaseString) extends Tense
case object PresentTense extends Tense
case object PastTense extends Tense
object Tense {

  implicit val tenseEncoder: Encoder[Tense] = Encoder.encodeString.contramap[Tense] {
    case PastTense    => "past"
    case PresentTense => "present"
    case Modal(m)     => m.toString
  }

  implicit val tenseDecoder: Decoder[Tense] = Decoder.decodeString.map {
    case "past"    => PastTense
    case "present" => PresentTense
    case m         => Modal(m.lowerCase)
  }
}
