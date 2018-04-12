package qasrl.crowd

import cats.Monoid
import cats.Monad
import cats.implicits._

import spacro.util.Span

import qasrl.labeling.SlotBasedLabel

import nlpdata.datasets.wiktionary.VerbForm
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

import scala.util.{Try, Success, Failure}

object QASRLDatasetNew {

  // superseded by `Align` once that's in cats
  import cats.data.Ior
  def mergeMaps[A, B](x: Map[A, B], y: Map[A, B]): Map[A, Ior[B, B]] = {
    val keySet = x.keySet ++ y.keySet
    keySet.iterator.map { key =>
      key -> Ior.fromOptions(x.get(key), y.get(key)).get // should always work
    }.toMap
  }

  @Lenses case class QASRLDataset(
    sentences: Map[String, QASRLSentence]
  ) {

    def cullVerblessSentences = filterSentences(_.verbEntries.isEmpty)
    def cullQuestionlessSentences = filterSentences(_.verbEntries.values.forall(_.questionLabels.isEmpty))

    def filterSentenceIds(predicate: String => Boolean) = QASRLDataset(
      sentences.filter(e => predicate(e._1))
    )

    def filterSentences(predicate: QASRLSentence => Boolean) = QASRLDataset(
      sentences.filter(e => predicate(e._2))
    )

    def filterQuestionLabels(predicate: QuestionLabel => Boolean) =
      QASRLDataset.sentences
        .composeTraversal(Optics.each)
        .composeLens(QASRLSentence.verbEntries)
        .composeTraversal(Optics.each)
        .composeLens(QASRLVerbEntry.questionLabels)
        .modify(_.filter(p => predicate(p._2)))(this)

    // culls questions with no source
    def filterQuestionSources(predicate: String => Boolean) =
      QASRLDataset(
        sentences.transform { case (sid, sentence) =>
          QASRLSentence.verbEntries
            .composeTraversal(Optics.each)
            .composeLens(QASRLVerbEntry.questionLabels)
            .modify(questionLabels =>
            questionLabels.flatMap { case (qLCS, qLabel) =>
              val newSources = qLabel.questionSources.filter(predicate)
              if(newSources.isEmpty) None
              else Some(qLCS -> qLabel.copy(questionSources = newSources))
            }
          )(sentence)
        }
      )
  }

  @Lenses case class QASRLSentence(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbEntries: Map[Int, QASRLVerbEntry]
  )

  @Lenses case class QASRLVerbEntry(
    verbIndex: Int,
    verbInflectedForms: InflectedForms,
    questionLabels: Map[String, QuestionLabel]
  ) {
    def combineWithLike(other: QASRLVerbEntry): Try[QASRLVerbEntry] = {
      if(verbIndex != other.verbIndex || verbInflectedForms != other.verbInflectedForms) {
        val thisVerbString = verbIndex + " (" + verbInflectedForms.allForms.mkString(",") + ")"
        val otherVerbString = other.verbIndex + " (" + other.verbInflectedForms.allForms.mkString(",") + ")"
        Failure(
          new IllegalArgumentException(
            s"Can only combine same verb; attempted to combine indices $thisVerbString and $otherVerbString "
          )
        )
      } else {
        mergeMaps(questionLabels, other.questionLabels)
          .traverse(_.fold(Monad[Try].pure, Monad[Try].pure, _ combineWithLike _))
          .map(newQuestionLabels =>
          QASRLVerbEntry(
            verbIndex,
            verbInflectedForms,
            newQuestionLabels
          )
        )
      }
    }
  }

  @Lenses case class QuestionLabel(
    questionString: String,
    questionSources: Set[String],
    questionSlots: SlotBasedLabel[VerbForm],
    answerJudgments: Set[AnswerLabel]
  ) {
    // requires that questions are about the same verb with same inflected forms
    def combineWithLike(other: QuestionLabel): Try[QuestionLabel] = {
      if(questionString != other.questionString || questionSlots != other.questionSlots) {
        val thisQString = questionString + " (" + questionSlots.renderWithSeparator(
          vf => QASRLDataset.JsonCodecs.verbFormToString(vf).lowerCase, ","
        ) + ")"
        val otherQString = other.questionString + " (" + other.questionSlots.renderWithSeparator(
          vf => QASRLDataset.JsonCodecs.verbFormToString(vf).lowerCase, ","
        ) + ")"
        Failure(
          new IllegalArgumentException(
            s"""Can only combine like questions; attempted to combine $thisQString and $otherQString """
          )
        )
      } else Success(
        QuestionLabel(
          questionString,
          questionSources ++ other.questionSources,
          questionSlots,
          answerJudgments ++ other.answerJudgments
        )
      )
    }
  }

  @Lenses case class AnswerLabel(
    sourceId: String,
    judgment: QASRLValidationAnswer)

  object QASRLDataset {

    implicit val qasrlDatasetMonoid: Monoid[QASRLDataset] =
      new Monoid[QASRLDataset] {
        override def empty: QASRLDataset = QASRLDataset(Map.empty[String, QASRLSentence])
        override def combine(x: QASRLDataset, y: QASRLDataset) = {
          val allKeys = x.sentences.keySet ++ y.sentences.keySet
          QASRLDataset(
            mergeMaps(x.sentences, y.sentences).transform { case (sentenceId, sentenceEntryIor) =>
              sentenceEntryIor.fold(
                identity, identity, (xe, ye) => QASRLSentence(
                  sentenceId,
                  xe.sentenceTokens,
                  mergeMaps(xe.verbEntries, ye.verbEntries).transform { case (verbIndex, verbEntryIor) =>
                    verbEntryIor.fold(
                      identity, identity, (xv, yv) =>
                      if(xv.verbIndex == yv.verbIndex && xv.verbInflectedForms == yv.verbInflectedForms) {
                        QASRLVerbEntry(
                          verbIndex,
                          xv.verbInflectedForms,
                          mergeMaps(xv.questionLabels, yv.questionLabels).transform {
                            case (questionString, questionLabelIor) =>
                              questionLabelIor.fold(
                                identity, identity, (xq, yq) => xq.combineWithLike(yq) match {
                                  case Success(q) => q
                                  case Failure(t) =>
                                    System.err.println(s"Sentence: $sentenceId")
                                    System.err.println(s"Sentence tokens: " + xe.sentenceTokens.mkString(" "))
                                    System.err.println(
                                      s"Verb: $verbIndex - " + xv.verbInflectedForms.allForms.mkString(", ")
                                    )
                                    System.err.println(t.getMessage)
                                    xq
                                }
                              )
                          }
                        )
                      } else {
                        val thisVerbString = xv.verbIndex + " (" +
                          xv.verbInflectedForms.allForms.mkString(",") + ")"
                        val otherVerbString = yv.verbIndex + " (" +
                          yv.verbInflectedForms.allForms.mkString(",") + ")"
                        System.err.println(s"Sentence: $sentenceId")
                        System.err.println(s"Sentence tokens: " + xe.sentenceTokens.mkString(" "))
                        System.err.println(
                          s"Attempted to combine indices $thisVerbString and $otherVerbString"
                        )
                        xv
                      }
                    )
                  }
                )
              )
            }
          )
        }
      }

    object JsonCodecs {
      implicit val spanEncoder: Encoder[Span] = new Encoder[Span] {
        final def apply(span: Span): Json = Json.arr(Json.fromInt(span.begin), Json.fromInt(span.end + 1))
      }
      implicit val spanDecoder: Decoder[Span] = new Decoder[Span] {
        final def apply(c: HCursor): Decoder.Result[Span] = for {
          begin <- c.downN(0).as[Int].right
          end <- c.downN(1).as[Int].right
        } yield Span(begin, end - 1)
      }

      implicit val answerLabelEncoder: Encoder[AnswerLabel] = new Encoder[AnswerLabel] {
        final def apply(a: AnswerLabel): Json = {
          Json.obj(
            Seq(
              List("sourceId" -> Json.fromString(a.sourceId)),
              a.judgment match {
                case InvalidQuestion => List("isValid" -> Json.fromBoolean(false))
                case Answer(spans) => List(
                  "isValid" -> Json.fromBoolean(true),
                  "spans" -> Json.fromValues(spans.map(spanEncoder.apply))
                )
              }
            ).flatten:_*
          )
        }
      }

      implicit val answerLabelDecoder: Decoder[AnswerLabel] = new Decoder[AnswerLabel] {
        final def apply(c: HCursor): Decoder.Result[AnswerLabel] = for {
          sourceId <- c.downField("sourceId").as[String].right
          isValid <- c.downField("isValid").as[Boolean].right
          spansOpt <- if(isValid) {
            c.downField("spans").as[List[Span]].right.map(Option(_)).right
          } else Right[DecodingFailure, Option[List[Span]]](None).right
        } yield AnswerLabel(sourceId, spansOpt.fold(InvalidQuestion: QASRLValidationAnswer)(Answer(_)))
      }

      implicit val inflectedFormsEncoder: Encoder[InflectedForms] = new Encoder[InflectedForms] {
        final def apply(forms: InflectedForms): Json = Json.obj(
          "stem" -> Json.fromString(forms.stem),
          "presentSingular3rd" -> Json.fromString(forms.present),
          "presentParticiple" -> Json.fromString(forms.presentParticiple),
          "past" -> Json.fromString(forms.past),
          "pastParticiple" -> Json.fromString(forms.pastParticiple)
        )
      }
      implicit val inflectedFormsDecoder: Decoder[InflectedForms] = new Decoder[InflectedForms] {
        final def apply(c: HCursor): Decoder.Result[InflectedForms] = for {
          stem <- c.downField("stem").as[String].right.map(_.lowerCase).right
          presentSingular3rd <- c.downField("presentSingular3rd").as[String].right.map(_.lowerCase).right
          presentParticiple <- c.downField("presentParticiple").as[String].right.map(_.lowerCase).right
          past <- c.downField("past").as[String].right.map(_.lowerCase).right
          pastParticiple <- c.downField("pastParticiple").as[String].right.map(_.lowerCase).right
        } yield InflectedForms(stem, presentSingular3rd, presentParticiple, past, pastParticiple)
      }

      def verbFormToString(form: VerbForm): String = form match {
        case Stem => "stem"
        case PresentSingular3rd => "presentSingular3rd"
        case PresentParticiple => "presentParticiple"
        case Past => "past"
        case PastParticiple => "pastParticiple"
      }

      def verbInfoFromString(s: String): Decoder.Result[(List[LowerCaseString], VerbForm)] = {
        val vec = s.split(" ").toVector
        val prefix = vec.init.toList.map(_.lowerCase)
        val verbFormResult = vec.last match {
          case "stem" => Right(Stem)
          case "presentSingular3rd" => Right(PresentSingular3rd)
          case "presentParticiple" => Right(PresentParticiple)
          case "past" => Right(Past)
          case "pastParticiple" => Right(PastParticiple)
          case x => Left(DecodingFailure(s"Invalid string for verb form: $x", Nil))
        }
        verbFormResult.right.map(prefix -> _)
      }

      implicit val slotBasedLabelEncoder: Encoder[SlotBasedLabel[VerbForm]] =
        new Encoder[SlotBasedLabel[VerbForm]] {
          final def apply(label: SlotBasedLabel[VerbForm]): Json = Json.obj(
            "wh" -> Json.fromString(label.wh),
            "aux" -> Json.fromString(label.aux.fold("_")(_.toString)),
            "subj" -> Json.fromString(label.subj.fold("_")(_.toString)),
            "verb" -> Json.fromString((label.verbPrefix ++ List(verbFormToString(label.verb))).mkString(" ")),
            "obj" -> Json.fromString(label.obj.fold("_")(_.toString)),
            "prep" -> Json.fromString(label.prep.fold("_")(_.toString)),
            "obj2" -> Json.fromString(label.obj2.fold("_")(_.toString))
          )
        }

      val readOptionalSlotString = (s: String) => if(s == "_") None else Some(s.lowerCase)

      implicit val slotBasedLabelDecoder: Decoder[SlotBasedLabel[VerbForm]] =
        new Decoder[SlotBasedLabel[VerbForm]] {
          final def apply(c: HCursor): Decoder.Result[SlotBasedLabel[VerbForm]] = for {
            wh <- c.downField("wh").as[String].right.map(_.lowerCase).right
            aux <- c.downField("aux").as[String].right.map(readOptionalSlotString).right
            subj <- c.downField("subj").as[String].right.map(readOptionalSlotString).right
            verbString <- c.downField("verb").as[String].right
            verbPrefixAndForm <- verbInfoFromString(verbString).right
            obj <- c.downField("obj").as[String].right.map(readOptionalSlotString).right
            prep <- c.downField("prep").as[String].right.map(readOptionalSlotString).right
            obj2 <- c.downField("obj2").as[String].right.map(readOptionalSlotString).right
          } yield SlotBasedLabel(wh, aux, subj, verbPrefixAndForm._1, verbPrefixAndForm._2, obj, prep, obj2)
        }

      implicit val questionLabelEncoder: Encoder[QuestionLabel] = {
        import io.circe.generic.semiauto._
        deriveEncoder[QuestionLabel]
      }
      implicit val questionLabelDecoder: Decoder[QuestionLabel] = {
        import io.circe.generic.semiauto._
        deriveDecoder[QuestionLabel]
      }

      implicit val qasrlVerbEntryEncoder: Encoder[QASRLVerbEntry] = {
        import io.circe.generic.semiauto._
        deriveEncoder[QASRLVerbEntry]
      }
      implicit val qasrlVerbEntryDecoder: Decoder[QASRLVerbEntry] = {
        import io.circe.generic.semiauto._
        deriveDecoder[QASRLVerbEntry]
      }

      implicit val qasrlSentenceEncoder: Encoder[QASRLSentence] = {
        import io.circe.generic.semiauto._
        deriveEncoder[QASRLSentence]
      }
      implicit val qasrlSentenceDecoder: Decoder[QASRLSentence] = {
        import io.circe.generic.semiauto._
        deriveDecoder[QASRLSentence]
      }
    }
  }
}

@Lenses case class QASRLDataset(
  entries: Map[String, QASRLSentenceEntry]
) {

  // def toNewQASRLDataset: QASRLDatasetNew.QASRLDataset = {
  //   QASRLDatasetNew.QASRLDataset(
  //     entries.transform((id, entry) =>
  //       QASRLDatasetNew.QASRLSentence(
  //         entry.sentenceId,
  //         entry.sentenceTokens,
  //         verbEntries: Map[Int, QASRLVerbEntry]
  //       )
  //     )
  //   )
  // }

  def filterSentenceIds(predicate: String => Boolean) = QASRLDataset(
    entries.filter(e => predicate(e._1))
  )

  def filterSentenceEntries(predicate: QASRLSentenceEntry => Boolean) = QASRLDataset(
    entries.filter(e => predicate(e._2))
  )

  def filterQASRLLabels(predicate: QASRLLabel => Boolean) = QASRLDataset(
    entries.flatMap { case (sid, sentenceEntry) =>
      val newLabels = sentenceEntry.labels.filter(predicate)
      if(newLabels.nonEmpty) Some(sid -> sentenceEntry.copy(labels = newLabels))
      else None
    }
  )

  def filterQuestionLabels(predicate: QuestionLabel => Boolean) =
    filterQASRLLabels(l => predicate(l.question))

  def filterQuestionSources(predicate: String => Boolean) = QASRLDataset(
    entries.flatMap { case (sid, sentenceEntry) =>
      val newLabels = sentenceEntry.labels.flatMap { label =>
        val newSources = label.question.sourceIds.filter(predicate)
        if(newSources.nonEmpty) Some(
          (QASRLLabel.question composeLens QuestionLabel.sourceIds).set(newSources)(label)
        ) else None
      }
      if(newLabels.nonEmpty) Some(sid -> sentenceEntry.copy(labels = newLabels))
      else None
    }
  )
}

@Lenses case class QASRLSentenceEntry(
  sentenceId: String,
  sentenceTokens: Vector[String],
  labels: List[QASRLLabel]
)

@Lenses case class QASRLLabel(
  question: QuestionLabel,
  answers: Set[AnswerLabel]
)

@Lenses case class QuestionLabel(
  sourceIds: Set[String],
  verbIndex: Int,
  verbInflectedForms: InflectedForms,
  questionString: String,
  questionSlots: SlotBasedLabel[VerbForm]
)

@Lenses case class AnswerLabel(
  sourceId: String,
  judgment: QASRLValidationAnswer)

object QASRLDataset {

  implicit val qasrlDatasetMonoid: Monoid[QASRLDataset] =
    new Monoid[QASRLDataset] {
      override def empty: QASRLDataset = QASRLDataset(Map.empty[String, QASRLSentenceEntry])
      override def combine(x: QASRLDataset, y: QASRLDataset) = {
        val allKeys = x.entries.keySet ++ y.entries.keySet
        QASRLDataset(
          allKeys.iterator.map { sentenceId =>
            val entry = (x.entries.get(sentenceId), y.entries.get(sentenceId)) match {
              case (Some(xe), Some(ye)) =>
                val combinedLabels = for {
                  (verbIndex, labelsForVerb) <- (
                    xe.labels ++ ye.labels
                  ).groupBy(_.question.verbIndex).toList.sortBy(_._1)
                  (qInfo, labels) <- (
                    labelsForVerb.groupBy(l =>
                      (l.question.verbInflectedForms, l.question.questionString, l.question.questionSlots)
                    )
                  ).toList.sortBy(_._1._2)
                } yield {
                  val sourceIds = labels.flatMap(_.question.sourceIds).toSet
                  val answers = labels.flatMap(_.answers).toSet
                  QASRLLabel(
                    QuestionLabel(
                      sourceIds, verbIndex,
                      qInfo._1, qInfo._2, qInfo._3),
                    answers
                  )
                }
                QASRLSentenceEntry(
                  sentenceId,
                  xe.sentenceTokens,
                  combinedLabels)
              case (Some(xe), None) => xe
              case (None, Some(ye)) => ye
              case _ => ??? // will never happen
            }
            sentenceId -> entry
          }.toMap
        )
      }
    }

  object JsonCodecs {
    implicit val spanEncoder: Encoder[Span] = new Encoder[Span] {
      final def apply(span: Span): Json = Json.arr(Json.fromInt(span.begin), Json.fromInt(span.end + 1))
    }
    implicit val spanDecoder: Decoder[Span] = new Decoder[Span] {
      final def apply(c: HCursor): Decoder.Result[Span] = for {
        begin <- c.downN(0).as[Int].right
        end <- c.downN(1).as[Int].right
      } yield Span(begin, end - 1)
    }

    implicit val answerLabelEncoder: Encoder[AnswerLabel] = new Encoder[AnswerLabel] {
      final def apply(a: AnswerLabel): Json = {
        Json.obj(
          Seq(
            List("sourceId" -> Json.fromString(a.sourceId)),
            a.judgment match {
              case InvalidQuestion => List("isValid" -> Json.fromBoolean(false))
              case Answer(spans) => List(
                "isValid" -> Json.fromBoolean(true),
                "spans" -> Json.fromValues(spans.map(spanEncoder.apply))
              )
            }
          ).flatten:_*
        )
      }
    }

    implicit val answerLabelDecoder: Decoder[AnswerLabel] = new Decoder[AnswerLabel] {
      final def apply(c: HCursor): Decoder.Result[AnswerLabel] = for {
        sourceId <- c.downField("sourceId").as[String].right
        isValid <- c.downField("isValid").as[Boolean].right
        spansOpt <- if(isValid) {
          c.downField("spans").as[List[Span]].right.map(Option(_)).right
        } else Right[DecodingFailure, Option[List[Span]]](None).right
      } yield AnswerLabel(sourceId, spansOpt.fold(InvalidQuestion: QASRLValidationAnswer)(Answer(_)))
    }

    implicit val inflectedFormsEncoder: Encoder[InflectedForms] = new Encoder[InflectedForms] {
      final def apply(forms: InflectedForms): Json = Json.obj(
        "stem" -> Json.fromString(forms.stem),
        "presentSingular3rd" -> Json.fromString(forms.present),
        "presentParticiple" -> Json.fromString(forms.presentParticiple),
        "past" -> Json.fromString(forms.past),
        "pastParticiple" -> Json.fromString(forms.pastParticiple)
      )
    }
    implicit val inflectedFormsDecoder: Decoder[InflectedForms] = new Decoder[InflectedForms] {
      final def apply(c: HCursor): Decoder.Result[InflectedForms] = for {
        stem <- c.downField("stem").as[String].right.map(_.lowerCase).right
        presentSingular3rd <- c.downField("presentSingular3rd").as[String].right.map(_.lowerCase).right
        presentParticiple <- c.downField("presentParticiple").as[String].right.map(_.lowerCase).right
        past <- c.downField("past").as[String].right.map(_.lowerCase).right
        pastParticiple <- c.downField("pastParticiple").as[String].right.map(_.lowerCase).right
      } yield InflectedForms(stem, presentSingular3rd, presentParticiple, past, pastParticiple)
    }

    def verbFormToString(form: VerbForm): String = form match {
      case Stem => "stem"
      case PresentSingular3rd => "presentSingular3rd"
      case PresentParticiple => "presentParticiple"
      case Past => "past"
      case PastParticiple => "pastParticiple"
    }

    def verbInfoFromString(s: String): Decoder.Result[(List[LowerCaseString], VerbForm)] = {
      val vec = s.split(" ").toVector
      val prefix = vec.init.toList.map(_.lowerCase)
      val verbFormResult = vec.last match {
        case "stem" => Right(Stem)
        case "presentSingular3rd" => Right(PresentSingular3rd)
        case "presentParticiple" => Right(PresentParticiple)
        case "past" => Right(Past)
        case "pastParticiple" => Right(PastParticiple)
        case x => Left(DecodingFailure(s"Invalid string for verb form: $x", Nil))
      }
      verbFormResult.right.map(prefix -> _)
    }

    implicit val slotBasedLabelEncoder: Encoder[SlotBasedLabel[VerbForm]] =
      new Encoder[SlotBasedLabel[VerbForm]] {
        final def apply(label: SlotBasedLabel[VerbForm]): Json = Json.obj(
          "wh" -> Json.fromString(label.wh),
          "aux" -> Json.fromString(label.aux.fold("_")(_.toString)),
          "subj" -> Json.fromString(label.subj.fold("_")(_.toString)),
          "verb" -> Json.fromString((label.verbPrefix ++ List(verbFormToString(label.verb))).mkString(" ")),
          "obj" -> Json.fromString(label.obj.fold("_")(_.toString)),
          "prep" -> Json.fromString(label.prep.fold("_")(_.toString)),
          "obj2" -> Json.fromString(label.obj2.fold("_")(_.toString))
        )
      }

    val readOptionalSlotString = (s: String) => if(s == "_") None else Some(s.lowerCase)

    implicit val slotBasedLabelDecoder: Decoder[SlotBasedLabel[VerbForm]] =
      new Decoder[SlotBasedLabel[VerbForm]] {
        final def apply(c: HCursor): Decoder.Result[SlotBasedLabel[VerbForm]] = for {
          wh <- c.downField("wh").as[String].right.map(_.lowerCase).right
          aux <- c.downField("aux").as[String].right.map(readOptionalSlotString).right
          subj <- c.downField("subj").as[String].right.map(readOptionalSlotString).right
          verbString <- c.downField("verb").as[String].right
          verbPrefixAndForm <- verbInfoFromString(verbString).right
          obj <- c.downField("obj").as[String].right.map(readOptionalSlotString).right
          prep <- c.downField("prep").as[String].right.map(readOptionalSlotString).right
          obj2 <- c.downField("obj2").as[String].right.map(readOptionalSlotString).right
        } yield SlotBasedLabel(wh, aux, subj, verbPrefixAndForm._1, verbPrefixAndForm._2, obj, prep, obj2)
      }

    implicit val questionLabelEncoder: Encoder[QuestionLabel] = {
      import io.circe.generic.semiauto._
      deriveEncoder[QuestionLabel]
    }
    implicit val questionLabelDecoder: Decoder[QuestionLabel] = {
      import io.circe.generic.semiauto._
      deriveDecoder[QuestionLabel]
    }

    implicit val qasrlLabelEncoder: Encoder[QASRLLabel] = {
      import io.circe.generic.semiauto._
      deriveEncoder[QASRLLabel]
    }
    implicit val qasrlLabelDecoder: Decoder[QASRLLabel] = {
      import io.circe.generic.semiauto._
      deriveDecoder[QASRLLabel]
    }

    implicit val qasrlSentenceEntryEncoder: Encoder[QASRLSentenceEntry] = {
      import io.circe.generic.semiauto._
      deriveEncoder[QASRLSentenceEntry]
    }
    implicit val qasrlSentenceEntryDecoder: Decoder[QASRLSentenceEntry] = {
      import io.circe.generic.semiauto._
      deriveDecoder[QASRLSentenceEntry]
    }

    implicit val qasrlDatasetEntryEncoder: Encoder[QASRLDataset] = {
      import io.circe.generic.auto._
      implicitly[Encoder[Map[String, QASRLSentenceEntry]]].contramap[QASRLDataset](_.entries)
    }

    implicit val qasrlDatasetEntryDecoder: Decoder[QASRLDataset] = {
      import io.circe.generic.auto._
      implicitly[Decoder[Map[String, QASRLSentenceEntry]]].map(QASRLDataset(_))
    }
  }
}
