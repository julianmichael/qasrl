package qasrl.crowd

import qasrl.labeling.SlotBasedLabel

import spacro.HITInfo

import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._

import scala.util.Random

import com.typesafe.scalalogging.StrictLogging

class AnnotationDataExporter[SID : HasTokens](
  experiment: QASRLAnnotationPipeline[SID]
) extends StrictLogging {
  import experiment.inflections
  val genInfos = experiment.allGenInfos
  val valInfos = experiment.allValInfos

  val genWorkers = genInfos.flatMap(_.assignments).map(_.workerId).toSet
  val valWorkers = valInfos.flatMap(_.assignments).map(_.workerId).toSet
  val allWorkers = genWorkers ++ valWorkers

  def workerAnonymizationMapping(
    label: String,
    rand: Random = new Random(26558729L)
  ): Map[String, String] = {
    rand.shuffle(allWorkers.toVector).zipWithIndex.map { case (wid, index) =>
      wid -> s"turk-$label-$index"
    }.toMap
  }

  def dataset(
    sentenceIdToString: SID => String,
    workerAnonymizationMapping: String => String
  ) = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valAssignmentsByGenAssignmentId = valInfos
      .groupBy(_.hit.prompt.sourceAssignmentId)
      .map { case (k, v) => k -> v.flatMap(_.assignments) }
      .withDefaultValue(Nil)
    QASRLDataset(
      genInfosBySentenceId.map { case (id, sentenceGenInfos) =>
        val sentenceIdString = sentenceIdToString(id)
        sentenceIdString -> {
          val sentenceTokens = id.tokens
          val qaLabelLists = for {
            HITInfo(genHIT, genAssignments) <- sentenceGenInfos
            genAssignment <- genAssignments
            qaTuples = genAssignment.response.zip(
              valAssignmentsByGenAssignmentId(genAssignment.assignmentId)
                .map(a => a.response.map(AnswerLabel(workerAnonymizationMapping(a.workerId), _))).transpose
            )
            verbInflectedForms <- inflections.getInflectedForms(
              sentenceTokens(genHIT.prompt.verbIndex).lowerCase
            )
          } yield {
            val questionStrings = qaTuples
              .map(_._1.question)
              .map(_.replaceAll("\\s+", " "))
            val questionSlotLabelOpts = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
              sentenceTokens, verbInflectedForms, questionStrings
            )
            val answerSets = qaTuples.map { case (VerbQA(_, _, genSpans), valAnswers) =>
                valAnswers.toSet + AnswerLabel(workerAnonymizationMapping(genAssignment.workerId), Answer(genSpans))
            }
            questionStrings.zip(questionSlotLabelOpts).collect {
              case (qString, None) => logger.warn(s"Unprocessable question: $qString")
            }

            (questionStrings, questionSlotLabelOpts, answerSets).zipped.collect {
              case (questionString, Some(questionSlots), answers) =>
                QASRLLabel(
                  QuestionLabel(
                    Set(s"turk-${genAssignment.workerId}"),
                    genHIT.prompt.verbIndex, verbInflectedForms,
                    questionString, questionSlots),
                  answers
                )
            }
          }
          QASRLSentenceEntry(sentenceIdString, sentenceTokens, qaLabelLists.flatten)
        }
      }
    )
  }
}
