package qasrl.crowd

import cats.data.NonEmptySet

import jjm.ling.HasToken
import jjm.implicits._

import qasrl.TemplateStateMachine
import qasrl.QuestionProcessor
import qasrl.labeling.SlotBasedLabel

import spacro.HITInfo
// import nlpdata.util.HasTokens

import scala.collection.immutable.SortedMap
import scala.util.Random
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.StrictLogging

class EvaluationDataExporter[SID, Word : HasToken](
  experiment: QASRLEvaluationPipeline[SID, Word]
) extends StrictLogging {
  import experiment.inflections
  val infos = experiment.allInfos

  val workers = infos.flatMap(_.assignments).map(_.workerId).toSet

  def workerAnonymizationMapping(
    label: String,
    rand: Random = new Random(26558729L)
  ): Map[String, String] = {
    rand
      .shuffle(workers.toVector)
      .zipWithIndex
      .map {
        case (wid, index) =>
          wid -> s"turk-$label-$index"
      }
      .toMap
  }

  import qasrl.data._

  def dataset(
    sentenceIdToString: SID => String,
    workerAnonymizationMapping: String => String
  ) = Dataset(
    SortedMap(
      infos.groupBy(_.hit.prompt.id).map {
        case (id, infosForSentence) =>
          val sentenceIdString = sentenceIdToString(id)
          val sentenceTokens = experiment.getTokens(id).map(_.token)
          val verbIndices =
            infosForSentence.flatMap(_.hit.prompt.sourcedQuestions.map(_.verbIndex)).toSet
          val partialVerbEntries = for {
            HITInfo(hit, assignments) <- infosForSentence
            (verbIndex, qaTuples) <- hit.prompt.sourcedQuestions
              .zip(
                assignments
                  .map(
                    a =>
                      a.response.map(
                        valAnswer =>
                          AnswerLabel(
                            workerAnonymizationMapping(a.workerId),
                            valAnswer match {
                              case qasrl.crowd.InvalidQuestion => qasrl.data.InvalidQuestion
                              case qasrl.crowd.Answer(spans) =>
                                Answer(
                                  NonEmptySet.fromSet(
                                    scala.collection.immutable.TreeSet(
                                      spans.map(_.toExclusive): _*
                                    )
                                  ).get // TODO push error handling out
                                )
                            }
                        )
                    )
                  )
                  .transpose
              )
              .groupBy(_._1.verbIndex)
            verbInflectedForms <- inflections.getInflectedForms(sentenceTokens(verbIndex).lowerCase)
          } yield {
            val questionStrings = qaTuples
              .map(_._1.question)
              .map(_.replaceAll("\\s+", " "))
            val questionSourceSets = qaTuples
              .map(_._1.sources)
            val questionSlotLabelOpts = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
              sentenceTokens,
              verbInflectedForms,
              questionStrings
            )
            val answerSets = qaTuples.map(_._2.toSet)
            questionStrings.zip(questionSlotLabelOpts).collect {
              case (qString, None) => logger.warn(s"Unprocessable question: $qString")
            }

            VerbEntry(
              verbIndex,
              verbInflectedForms,
              SortedMap(
                (questionStrings zip questionSourceSets zip questionSlotLabelOpts zip answerSets).collect {
                  case (((questionStringRaw, questionSources), Some(questionSlots)), answers) =>
                    val questionString = questionStringRaw.toLowerCase.capitalize
                    val questionTokensIsh = questionString.init.split(" ").toVector.map(_.lowerCase)
                    val qPreps =
                      questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
                    val qPrepBigrams = questionTokensIsh
                      .sliding(2)
                      .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
                      .map(_.mkString(" ").lowerCase)
                      .toSet
                    val stateMachine = new TemplateStateMachine(
                      sentenceTokens,
                      verbInflectedForms,
                      Some(qPreps ++ qPrepBigrams)
                    )
                    val questionProcessor = new QuestionProcessor(stateMachine)
                    val frame = questionProcessor
                      .processStringFully(questionString)
                      .right
                      .get
                      .toList
                      .collect {
                        case QuestionProcessor.CompleteState(_, frame, _) => frame
                      }
                      .head
                    questionString -> QuestionLabel(
                      questionString,
                      questionSources,
                      answers,
                      questionSlots,
                      frame.tense,
                      frame.isPerfect,
                      frame.isProgressive,
                      frame.isNegated,
                      frame.isPassive
                    )
                }: _*
              )
            )
          }
          sentenceIdString -> {
            Sentence(
              sentenceIdString,
              sentenceTokens,
              SortedMap(
                partialVerbEntries.groupBy(_.verbIndex).transform {
                  case (_, verbEntries) =>
                    verbEntries.reduce(
                      (e1, e2) =>
                        e1.combineWithLike(e2) match {
                          case Right(e) => e
                          case Left(msg) =>
                            System.err.println(s"Sentence: $sentenceIdString")
                            System.err.println(s"Sentence tokens: " + sentenceTokens.mkString(" "))
                            System.err.println(msg)
                            e1
                      }
                    )
                }.toSeq: _*
              )
            )
          }
      }.toSeq: _*
    )
  )
}
