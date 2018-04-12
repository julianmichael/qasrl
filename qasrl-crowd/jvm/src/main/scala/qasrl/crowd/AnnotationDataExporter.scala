package qasrl.crowd

import qasrl.labeling.SlotBasedLabel

import spacro.HITInfo

import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._

import scala.util.Random
import scala.util.{Try, Success, Failure}

import com.typesafe.scalalogging.StrictLogging

class AnnotationDataExporter[SID : HasTokens](
  experiment: QASRLAnnotationPipeline[SID]
) extends StrictLogging {
  import experiment.inflections
  val allIdsSet = experiment.allIds.toSet
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
    QASRLDatasetNew.QASRLDataset(
      genInfosBySentenceId.map { case (id, sentenceGenInfos) =>
        val sentenceIdString = sentenceIdToString(id)
        val sentenceTokens = id.tokens

        val computedVerbIndices = experiment.getKeyIndices(id)
        val verbIndices = sentenceGenInfos.map(_.hit.prompt.verbIndex).toSet
        if(computedVerbIndices != verbIndices) {
          System.err.println("Verb index disagreement!")
          System.err.println(sentenceIdString)
          System.err.println(sentenceTokens.mkString(" "))
          System.err.println("Computed: " + computedVerbIndices.toVector.sorted.mkString(" "))
          System.err.println("Observed: " + verbIndices.toVector.sorted.mkString(" "))
          System.err.println(
            "Difference: " +
              (computedVerbIndices -- verbIndices).toVector.sorted.mkString(" ") + "|" +
              (verbIndices -- computedVerbIndices).toVector.sorted.mkString(" ")
          )
        }
        sentenceIdString -> {
          QASRLDatasetNew.QASRLSentence(
            sentenceIdString,
            sentenceTokens,
            verbIndices.flatMap { verbIndex =>
              val genInfosForVerb = sentenceGenInfos.filter(_.hit.prompt.verbIndex == verbIndex)
              experiment.inflections.getInflectedForms(sentenceTokens(verbIndex).lowerCase)
                .map { verbInflectedForms =>
                val questionLabelLists = for {
                  HITInfo(genPrompt, genAssignments) <- genInfosForVerb
                  genAssignment <- genAssignments
                  qaTuples = genAssignment.response.zip(
                    valAssignmentsByGenAssignmentId(genAssignment.assignmentId)
                      .map(a => a.response.map(QASRLDatasetNew.AnswerLabel(workerAnonymizationMapping(a.workerId), _)))
                      .transpose
                  )
                } yield {
                  val questionStrings = qaTuples
                    .map(_._1.question.toLowerCase.capitalize)
                    .map(_.replaceAll("\\s+", " "))
                  val questionSlotLabelOpts = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
                    sentenceTokens, verbInflectedForms, questionStrings
                  )
                  val answerSets = qaTuples.map { case (VerbQA(_, _, genSpans), valAnswers) =>
                    valAnswers.toSet + QASRLDatasetNew.AnswerLabel(
                      workerAnonymizationMapping(genAssignment.workerId),
                      Answer(genSpans)
                    )
                  }
                  questionStrings.zip(questionSlotLabelOpts).collect {
                    case (qString, None) => logger.warn(s"Unprocessable question: $qString")
                  }

                  (questionStrings, questionSlotLabelOpts, answerSets).zipped.collect {
                    case (questionString, Some(questionSlots), answers) =>
                      QASRLDatasetNew.QuestionLabel(
                        questionString,
                        Set(workerAnonymizationMapping(genAssignment.workerId)),
                        questionSlots,
                        answers
                      )
                  }
                }
                verbIndex -> QASRLDatasetNew.QASRLVerbEntry(
                  verbIndex,
                  verbInflectedForms,
                  questionLabelLists.flatten.groupBy(_.questionString).transform { case (qString, qLabels) =>
                    qLabels.reduce((l1: QASRLDatasetNew.QuestionLabel, l2: QASRLDatasetNew.QuestionLabel) =>
                      l1.combineWithLike(l2) match {
                        case Success(l) => l
                        case Failure(t) =>
                          System.err.println(s"Sentence: $sentenceIdString")
                          System.err.println(s"Sentence tokens: " + sentenceTokens.mkString(" "))
                          System.err.println(
                            s"Verb: $verbIndex - " + verbInflectedForms.allForms.mkString(", ")
                          )
                          System.err.println(t.getMessage)
                          l1
                      }
                    )
                  }
                )
              }
            }.toMap
          )
        }
      }
    )
  }
}
