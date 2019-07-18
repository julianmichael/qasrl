package qasrl.data

import jjm.ling.ESpan

import cats.data.NonEmptySet
import cats.implicits._

import monocle.macros._

import io.circe.{Encoder, Decoder}
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.Json

@Lenses case class AnswerLabel(sourceId: String, judgment: AnswerJudgment)
object AnswerLabel {
  implicit val answerLabelEncoder: Encoder[AnswerLabel] = new Encoder[AnswerLabel] {
    final def apply(a: AnswerLabel): Json = {
      Json.obj(
        Seq(
          List("sourceId" -> Json.fromString(a.sourceId)),
          a.judgment match {
            case InvalidQuestion => List("isValid" -> Json.fromBoolean(false))
            case Answer(spans) =>
              List(
                "isValid" -> Json.fromBoolean(true),
                "spans"   -> Json.fromValues(spans.toList.map(ESpan.espanEncoder.apply))
              )
          }
        ).flatten: _*
      )
    }
  }

  implicit val answerLabelDecoder: Decoder[AnswerLabel] = new Decoder[AnswerLabel] {
    final def apply(c: HCursor): Decoder.Result[AnswerLabel] =
      for {
        sourceId <- c.downField("sourceId").as[String].right
        isValid  <- c.downField("isValid").as[Boolean].right
        spansOpt <- if (isValid) {
          c.downField("spans").as[List[ESpan]].right.map(Option(_)).right
        } else Right[DecodingFailure, Option[List[ESpan]]](None).right
        spansSetOpt = spansOpt.flatMap(spans => NonEmptySet.fromSet(scala.collection.immutable.TreeSet(spans:_*)))
      } yield AnswerLabel(sourceId, spansSetOpt.fold(InvalidQuestion: AnswerJudgment)(Answer(_)))
  }
}
