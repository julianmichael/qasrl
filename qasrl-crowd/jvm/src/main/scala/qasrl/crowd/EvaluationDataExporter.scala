package qasrl.crowd

import qasrl.labeling.SlotBasedLabel

import spacro.HITInfo

import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._

import scala.util.Random

import com.typesafe.scalalogging.StrictLogging

class EvaluationDataExporter[SID : HasTokens](
  experiment: QASRLEvaluationPipeline[SID]
) extends StrictLogging {
  import experiment.inflections
  val infos = experiment.allInfos

  val workers = infos.flatMap(_.assignments).map(_.workerId).toSet

  def workerAnonymizationMapping(
    label: String,
    rand: Random = new Random(26558729L)
  ): Map[String, String] = {
    rand.shuffle(workers.toVector).zipWithIndex.map { case (wid, index) =>
      wid -> s"turk-$label-$index"
    }.toMap
  }

  def dataset(
    sentenceIdToString: SID => String,
    workerAnonymizationMapping: String => String
  ) = QASRLDataset(
    infos.groupBy(_.hit.prompt.id).map { case (id, infos) =>
      val sentenceIdString = sentenceIdToString(id)
      sentenceIdString -> {
        val sentenceTokens = id.tokens
        val qaLabelLists = for {
          HITInfo(hit, assignments) <- infos
          (verbIndex, qaTuples) <- hit.prompt.qaPairs.zip(
            assignments.map(
              a => a.response.map(AnswerLabel(workerAnonymizationMapping(a.workerId) , _))
            ).transpose
          ).groupBy(_._1.verbIndex)
          verbInflectedForms <- inflections.getInflectedForms(sentenceTokens(verbIndex).lowerCase)
        } yield {
          val questionStrings = qaTuples
            .map(_._1.question)
            .map(_.replaceAll("\\s+", " "))
          val questionSlotLabelOpts = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
            sentenceTokens, verbInflectedForms, questionStrings
          )
          val answerSets = qaTuples.map(_._2.toSet)
          questionStrings.zip(questionSlotLabelOpts).collect {
            case (qString, None) => logger.warn(s"Unprocessable question: $qString")
          }

          (questionStrings, questionSlotLabelOpts, answerSets).zipped.collect {
            case (questionString, Some(questionSlots), answers) =>
              QASRLLabel(
                QuestionLabel(
                  Set(hit.prompt.sourceId), verbIndex, verbInflectedForms,
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
