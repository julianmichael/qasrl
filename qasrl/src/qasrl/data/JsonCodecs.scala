package qasrl.data

import cats.Monoid
import cats.Monad
import cats.implicits._

import qasrl._
import qasrl.labeling.SlotBasedLabel
import qasrl.util.mergeMaps

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.datasets.wiktionary.Stem
import nlpdata.datasets.wiktionary.PresentSingular3rd
import nlpdata.datasets.wiktionary.PresentParticiple
import nlpdata.datasets.wiktionary.Past
import nlpdata.datasets.wiktionary.PastParticiple

import io.circe._

import monocle.macros._
import monocle.function.{all => Optics}

import scala.util.{Failure, Success, Try}

object JsonCodecs {
  implicit val spanEncoder: Encoder[AnswerSpan] = new Encoder[AnswerSpan] {
    final def apply(span: AnswerSpan): Json =
      Json.arr(Json.fromInt(span.begin), Json.fromInt(span.end))
  }
  implicit val spanDecoder: Decoder[AnswerSpan] = new Decoder[AnswerSpan] {
    final def apply(c: HCursor): Decoder.Result[AnswerSpan] =
      for {
        begin <- c.downN(0).as[Int].right
        end   <- c.downN(1).as[Int].right
      } yield AnswerSpan(begin, end)
  }

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
                "spans"   -> Json.fromValues(spans.map(spanEncoder.apply))
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
          c.downField("spans").as[List[AnswerSpan]].right.map(Option(_)).right
        } else Right[DecodingFailure, Option[List[AnswerSpan]]](None).right
        spansSetOpt = spansOpt.map(_.toSet)
      } yield AnswerLabel(sourceId, spansSetOpt.fold(InvalidQuestion: AnswerJudgment)(Answer(_)))
  }

  implicit val inflectedFormsEncoder: Encoder[InflectedForms] = new Encoder[InflectedForms] {
    final def apply(forms: InflectedForms): Json = Json.obj(
      "stem"               -> Json.fromString(forms.stem),
      "presentSingular3rd" -> Json.fromString(forms.present),
      "presentParticiple"  -> Json.fromString(forms.presentParticiple),
      "past"               -> Json.fromString(forms.past),
      "pastParticiple"     -> Json.fromString(forms.pastParticiple)
    )
  }
  implicit val inflectedFormsDecoder: Decoder[InflectedForms] = new Decoder[InflectedForms] {
    final def apply(c: HCursor): Decoder.Result[InflectedForms] =
      for {
        stem <- c.downField("stem").as[String].right.map(_.lowerCase).right
        presentSingular3rd <- c
          .downField("presentSingular3rd")
          .as[String]
          .right
          .map(_.lowerCase)
          .right
        presentParticiple <- c
          .downField("presentParticiple")
          .as[String]
          .right
          .map(_.lowerCase)
          .right
        past           <- c.downField("past").as[String].right.map(_.lowerCase).right
        pastParticiple <- c.downField("pastParticiple").as[String].right.map(_.lowerCase).right
      } yield InflectedForms(stem, presentSingular3rd, presentParticiple, past, pastParticiple)
  }

  def verbFormToString(form: VerbForm): String = form match {
    case Stem               => "stem"
    case PresentSingular3rd => "presentSingular3rd"
    case PresentParticiple  => "presentParticiple"
    case Past               => "past"
    case PastParticiple     => "pastParticiple"
  }

  def verbInfoFromString(s: String): Decoder.Result[(List[LowerCaseString], VerbForm)] = {
    val vec = s.split(" ").toVector
    val prefix = vec.init.toList.map(_.lowerCase)
    val verbFormResult = vec.last match {
      case "stem"               => Right(Stem)
      case "presentSingular3rd" => Right(PresentSingular3rd)
      case "presentParticiple"  => Right(PresentParticiple)
      case "past"               => Right(Past)
      case "pastParticiple"     => Right(PastParticiple)
      case x                    => Left(DecodingFailure(s"Invalid string for verb form: $x", Nil))
    }
    verbFormResult.right.map(prefix -> _)
  }

  implicit val slotBasedLabelEncoder: Encoder[SlotBasedLabel[VerbForm]] =
    new Encoder[SlotBasedLabel[VerbForm]] {
      final def apply(label: SlotBasedLabel[VerbForm]): Json = Json.obj(
        "wh"   -> Json.fromString(label.wh),
        "aux"  -> Json.fromString(label.aux.fold("_")(_.toString)),
        "subj" -> Json.fromString(label.subj.fold("_")(_.toString)),
        "verb" -> Json.fromString(
          (label.verbPrefix ++ List(verbFormToString(label.verb))).mkString(" ")
        ),
        "obj"  -> Json.fromString(label.obj.fold("_")(_.toString)),
        "prep" -> Json.fromString(label.prep.fold("_")(_.toString)),
        "obj2" -> Json.fromString(label.obj2.fold("_")(_.toString))
      )
    }

  val readOptionalSlotString = (s: String) => if (s == "_") None else Some(s.lowerCase)

  implicit val slotBasedLabelDecoder: Decoder[SlotBasedLabel[VerbForm]] =
    new Decoder[SlotBasedLabel[VerbForm]] {
      final def apply(c: HCursor): Decoder.Result[SlotBasedLabel[VerbForm]] =
        for {
          wh                <- c.downField("wh").as[String].right.map(_.lowerCase).right
          aux               <- c.downField("aux").as[String].right.map(readOptionalSlotString).right
          subj              <- c.downField("subj").as[String].right.map(readOptionalSlotString).right
          verbString        <- c.downField("verb").as[String].right
          verbPrefixAndForm <- verbInfoFromString(verbString).right
          obj               <- c.downField("obj").as[String].right.map(readOptionalSlotString).right
          prep              <- c.downField("prep").as[String].right.map(readOptionalSlotString).right
          obj2              <- c.downField("obj2").as[String].right.map(readOptionalSlotString).right
        } yield
          SlotBasedLabel(wh, aux, subj, verbPrefixAndForm._1, verbPrefixAndForm._2, obj, prep, obj2)
    }

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

  implicit val questionLabelEncoder: Encoder[QuestionLabel] = {
    import io.circe.generic.semiauto._
    deriveEncoder[QuestionLabel]
  }

  implicit val qasrlVerbEntryEncoder: Encoder[VerbEntry] = {
    import io.circe.generic.semiauto._
    deriveEncoder[VerbEntry]
  }

  implicit val qasrlSentenceEncoder: Encoder[Sentence] = {
    import io.circe.generic.semiauto._
    deriveEncoder[Sentence]
  }

  implicit val questionLabelDecoder: Decoder[QuestionLabel] = {
    import io.circe.generic.semiauto._
    deriveDecoder[QuestionLabel]
  }

  implicit val qasrlVerbEntryDecoder: Decoder[VerbEntry] = {
    import io.circe.generic.semiauto._
    deriveDecoder[VerbEntry]
  }

  implicit val qasrlSentenceDecoder: Decoder[Sentence] = {
    import io.circe.generic.semiauto._
    deriveDecoder[Sentence]
  }
}
