package qasrl.crowd

import qasrl.labeling.SlotBasedLabel

import spacro.HITInfo

import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._

import scala.util.Random
import scala.util.{Try, Success, Failure}

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
  ) = QASRLDatasetNew.QASRLDataset(
    infos.groupBy(_.hit.prompt.id).map { case (id, infosForSentence) =>
      val sentenceIdString = sentenceIdToString(id)
      val sentenceTokens = id.tokens
      val verbIndices = infosForSentence.flatMap(_.hit.prompt.sourcedQuestions.map(_.verbIndex)).toSet
      val partialVerbEntries = for {
        HITInfo(hit, assignments) <- infosForSentence
        (verbIndex, qaTuples) <- hit.prompt.sourcedQuestions.zip(
          assignments.map(
            a => a.response.map(QASRLDatasetNew.AnswerLabel(workerAnonymizationMapping(a.workerId) , _))
          ).transpose
        ).groupBy(_._1.verbIndex)
        verbInflectedForms <- inflections.getInflectedForms(sentenceTokens(verbIndex).lowerCase)
      } yield {
        val questionStrings = qaTuples
          .map(_._1.question)
          .map(_.replaceAll("\\s+", " "))
        val questionSourceSets = qaTuples
          .map(_._1.sources)
        val questionSlotLabelOpts = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
          sentenceTokens, verbInflectedForms, questionStrings
        )
        val answerSets = qaTuples.map(_._2.toSet)
        questionStrings.zip(questionSlotLabelOpts).collect {
          case (qString, None) => logger.warn(s"Unprocessable question: $qString")
        }

        QASRLDatasetNew.QASRLVerbEntry(
          verbIndex,
          verbInflectedForms,
          (questionStrings zip questionSourceSets zip questionSlotLabelOpts zip answerSets).collect {
            case (((questionString, questionSources), Some(questionSlots)), answers) =>
              questionString.toLowerCase.capitalize -> QASRLDatasetNew.QuestionLabel(
                questionString.toLowerCase.capitalize,
                questionSources,
                questionSlots,
                answers
              )
          }.toMap
        )
      }
      sentenceIdString -> {
        QASRLDatasetNew.QASRLSentence(
          sentenceIdString,
          sentenceTokens,
          partialVerbEntries.groupBy(_.verbIndex).transform { case (_, verbEntries) =>
            verbEntries.reduce(
              (e1, e2) => e1.combineWithLike(e2) match {
                case Success(e) => e
                case Failure(t) =>
                  System.err.println(s"Sentence: $sentenceIdString")
                  System.err.println(s"Sentence tokens: " + sentenceTokens.mkString(" "))
                  System.err.println(t.getMessage)
                  e1
              })
          }
        )
      }
    }
  )
}
