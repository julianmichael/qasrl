package qasrl.data

import scala.collection.immutable.SortedMap

import cats.Monad
import cats.Monoid
import cats.data.Writer
import cats.implicits._

import monocle.Iso
import monocle.macros.{GenPrism, Lenses}
import monocle.function.{all => Optics}

import qasrl.util.implicits._
import qasrl.util.mergeMaps

@Lenses case class Dataset(
  sentences: SortedMap[String, Sentence]
) {

  def cullVerblessSentences = filterSentences(_.verbEntries.isEmpty)

  def cullQuestionlessSentences =
    filterSentences(_.verbEntries.values.forall(_.questionLabels.isEmpty))

  def filterSentenceIds(predicate: String => Boolean) = Dataset(
    sentences.filter(e => predicate(e._1))
  )

  def filterSentences(predicate: Sentence => Boolean) = Dataset(
    sentences.filter(e => predicate(e._2))
  )

  def filterQuestionLabels(predicate: QuestionLabel => Boolean) =
    Dataset.sentences
      .composeTraversal(Optics.each)
      .composeLens(Sentence.verbEntries)
      .composeTraversal(Optics.each)
      .composeLens(VerbEntry.questionLabels)
      .modify(_.filter(p => predicate(p._2)))(this)

  // culls questions with no source
  def filterQuestionSources(predicate: String => Boolean) =
    Dataset(
      sentences.transform {
        case (sid, sentence) =>
          Sentence.verbEntries
            .composeTraversal(Optics.each)
            .composeLens(VerbEntry.questionLabels)
            .modify(
              questionLabels =>
                questionLabels.flatMap {
                  case (qLCS, qLabel) =>
                    val newSources = qLabel.questionSources.filter(predicate)
                    if (newSources.isEmpty) None
                    else Some(qLCS -> qLabel.copy(questionSources = newSources))
              }
            )(sentence)
      }
    )

  def merge(that: Dataset): Writer[List[Dataset.DataMergeFailure], Dataset] = {
    val allKeys = this.sentences.keySet ++ that.sentences.keySet
    type Fails = List[Dataset.DataMergeFailure]
    mergeMaps(this.sentences, that.sentences).toList
      .traverse[Writer[Fails, ?], (String, Sentence)] {
        case (sentenceId, sentenceEntryIor) =>
          sentenceEntryIor
            .mergeM[Writer[Fails, ?]] { (xe, ye) =>
              mergeMaps(xe.verbEntries, ye.verbEntries).toList
                .traverse[Writer[Fails, ?], (Int, VerbEntry)] {
                  case (verbIndex, verbEntryIor) =>
                    verbEntryIor
                      .mergeM[Writer[Fails, ?]](
                        (xv, yv) =>
                          if (xv.verbIndex == yv.verbIndex && xv.verbInflectedForms == yv.verbInflectedForms) {
                            mergeMaps(xv.questionLabels, yv.questionLabels).toList
                              .traverse[Writer[Fails, ?], (String, QuestionLabel)] {
                                case (questionString, questionLabelIor) =>
                                  questionLabelIor
                                    .mergeM[Writer[Fails, ?]](
                                      (xq, yq) =>
                                        xq.combineWithLike(yq) match {
                                          case Right(q) => Writer.value[Fails, QuestionLabel](q)
                                          case Left(msg) =>
                                            Writer
                                              .tell[Fails](
                                                List(
                                                  Dataset.QuestionMergeFailure(
                                                    sentenceId,
                                                    xe.sentenceTokens,
                                                    xv,
                                                    yv,
                                                    xq,
                                                    yq,
                                                    msg
                                                  )
                                                )
                                              )
                                              .as(xq)
                                      }
                                    )
                                    .map(questionString -> _)
                              }
                              .map(
                                questionLabels =>
                                  VerbEntry(verbIndex, xv.verbInflectedForms, SortedMap(questionLabels: _*))
                              )
                          } else {
                            Writer
                              .tell[Fails](
                                List(
                                  Dataset.VerbMergeFailure(sentenceId, xe.sentenceTokens, xv, yv)
                                )
                              )
                              .as(xv)
                        }
                      )
                      .map(verbIndex -> _)
                }
                .map(verbEntries => Sentence(sentenceId, xe.sentenceTokens, SortedMap(verbEntries: _*)))
            }
            .map(sentenceId -> _)
      }
      .map(sentences => Dataset(SortedMap(sentences: _*)))
  }
}

object Dataset {
  sealed trait DataMergeFailure

  @Lenses case class QuestionMergeFailure(
    sentenceId: String,
    sentenceTokens: Vector[String],
    v1: VerbEntry,
    v2: VerbEntry,
    q1: QuestionLabel,
    q2: QuestionLabel,
    message: String
  ) extends DataMergeFailure

  @Lenses case class VerbMergeFailure(
    sentenceId: String,
    sentenceTokens: Vector[String],
    v1: VerbEntry,
    v2: VerbEntry
  ) extends DataMergeFailure

  object DataMergeFailure {
    val questionMergeFailure = GenPrism[DataMergeFailure, QuestionMergeFailure]
    val verbMergeFailure = GenPrism[DataMergeFailure, VerbMergeFailure]
  }

  val printMergeErrors: (List[DataMergeFailure] => Unit) =
    (fails: List[DataMergeFailure]) =>
      fails.foreach {
        case QuestionMergeFailure(sentenceId, sentenceTokens, v1, v2, q1, q2, msg) =>
          System.err.println(s"Sentence: $sentenceId")
          System.err.println(s"Sentence tokens: " + sentenceTokens.mkString(" "))
          System.err.println(
            s"Verb: ${v1.verbIndex} - " + v1.verbInflectedForms.allForms.mkString(", ")
          )
          System.err.println(msg)
        case VerbMergeFailure(sentenceId, sentenceTokens, v1, v2) =>
          val thisVerbString = v1.verbIndex + " (" +
          v1.verbInflectedForms.allForms.mkString(",") + ")"
          val otherVerbString = v2.verbIndex + " (" +
          v2.verbInflectedForms.allForms.mkString(",") + ")"
          System.err.println(s"Sentence: $sentenceId")
          System.err.println(s"Sentence tokens: " + sentenceTokens.mkString(" "))
          System.err.println(s"Attempted to combine indices $thisVerbString and $otherVerbString")
    }

  def datasetMonoid(processMergeFailures: List[DataMergeFailure] => Unit): Monoid[Dataset] =
    new Monoid[Dataset] {
      override def empty: Dataset = Dataset(SortedMap.empty[String, Sentence])
      override def combine(x: Dataset, y: Dataset) = {
        val (fails, result) = x.merge(y).run
        processMergeFailures(fails)
        result
      }
    }

  // useful optics
  val verbEntries = Dataset.sentences
    .composeTraversal(Optics.each)
    .composeLens(Sentence.verbEntries)
    .composeTraversal(Optics.each)

  val questionLabels = verbEntries
    .composeLens(VerbEntry.questionLabels)
    .composeTraversal(Optics.each)

  val questionSources = questionLabels
    .composeLens(QuestionLabel.questionSources)
    .composeIso(Iso[Set[String], List[String]](_.toList)(_.toSet))
    .composeTraversal(Optics.each)

  val answerLabelSets = questionLabels
    .composeLens(QuestionLabel.answerJudgments)

  val answerLabels = answerLabelSets
    .composeIso(Iso[Set[AnswerLabel], List[AnswerLabel]](_.toList)(_.toSet))
    .composeTraversal(Optics.each)

  val answerSources = answerLabels.composeLens(AnswerLabel.sourceId)

  val answerJudgments = answerLabels.composeLens(AnswerLabel.judgment)

  val answers = answerJudgments.composePrism(AnswerJudgment.answer)
}
