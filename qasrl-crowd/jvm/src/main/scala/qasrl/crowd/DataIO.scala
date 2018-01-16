package qasrl.crowd

import qasrl.Frame
import qasrl.QuestionProcessor
import qasrl.TemplateStateMachine
import qasrl.labeling.QuestionLabelMapper

import qasrl.crowd.util.CategoricalDistribution
import qasrl.crowd.util.implicits._

import cats.Foldable
import cats.data.NonEmptyList
import cats.implicits._

import spacro.HITInfo
import spacro.util.Span

import nlpdata.datasets.wiktionary.Inflections
import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.HasTokens.ops._
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.HasTokens
import nlpdata.util.Text

import com.typesafe.scalalogging.LazyLogging

object DataIO extends LazyLogging {

  def makeQAPairTSV[SID : HasTokens, QuestionLabel](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    genInfos: List[HITInfo[QASRLGenerationPrompt[SID], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]],
    mapLabels: QuestionLabelMapper[String, QuestionLabel],
    renderLabel: QuestionLabel => String)(
    implicit inflections: Inflections
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId).withDefaultValue(Nil)
    val sb = new StringBuilder
    for(id <- ids) {
      val idString = writeId(id)
      val sentenceTokens = id.tokens
      val sentenceSB = new StringBuilder
      var shouldIncludeSentence = false // for now, print everything
      sentenceSB.append(s"${idString}\t${sentenceTokens.mkString(" ")}\n")
      for {
        // sort by keyword group first...
        HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.verbIndex)
        // then worker ID second, so the data will be chunked correctly according to HIT;
        genAssignment <- genAssignments.sortBy(_.workerId)
        // process in order of verb
        verbIndex <- genAssignment.response.map(_.verbIndex).toSet.toList.sorted
        // only use verbs where we have inflected forms (should always be the case though)
        inflForms <- inflections.getInflectedForms(sentenceTokens(verbIndex).lowerCase).toList
        verbQAsIndexed = genAssignment.response.zipWithIndex.filter(_._1.verbIndex == verbIndex)
        labels = mapLabels(sentenceTokens, inflForms, verbQAsIndexed.map(_._1.question))
        ((wqa, qaIndex), Some(qLabel)) <- verbQAsIndexed.zip(labels)
      } yield {
        // pairs of (validation worker ID, validation answer)
        val valAnswerSpans = for {
          info <- valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil)
          assignment <- info.assignments
          answer <- assignment.response(qaIndex).getAnswer
        } yield answer.spans
        if(valAnswerSpans.size != 2) {
          logger.warn("Warning: don't have 2 validation answers for question. Actual number: " + valAnswerSpans.size)
        } else {
          shouldIncludeSentence = true
          sentenceSB.append("\t")
          sentenceSB.append(wqa.verbIndex.toString + "\t")
          sentenceSB.append(renderLabel(qLabel) + "\t")
          sentenceSB.append(
            (wqa.answers :: valAnswerSpans).map { spans =>
              spans
                .map(span => s"${span.begin}-${span.end}")
                .mkString(";")
            }.mkString("\t")
          )
          sentenceSB.append("\n")
        }
      }
      if(shouldIncludeSentence) {
        sb.append(sentenceSB.toString)
      }
    }
    sb.toString
  }

  // how much and how long must come first so we register them as question prefix
  val whPhrases = List("how much", "how long", "who", "what", "when", "where", "why", "how").map(_.lowerCase)

  def makeReadableQAPairTSV[SID : HasTokens](
    ids: List[SID],
    writeId: SID => String, // serialize sentence ID for distribution in data file
    anonymizeWorker: String => String, // anonymize worker IDs so they can't be tied back to workers on Turk
    genInfos: List[HITInfo[QASRLGenerationPrompt[SID], List[VerbQA]]],
    valInfos: List[HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]],
    keepQA: (SID, VerbQA, List[QASRLValidationAnswer]) => Boolean = (
      (_: SID, _: VerbQA, _: List[QASRLValidationAnswer]) => true)
  ): String = {
    val genInfosBySentenceId = genInfos.groupBy(_.hit.prompt.id).withDefaultValue(Nil)
    val valInfosByGenAssignmentId = valInfos.groupBy(_.hit.prompt.sourceAssignmentId).withDefaultValue(Nil)
    val sb = new StringBuilder
    for(id <- ids) {
      val idString = writeId(id)
      val sentenceTokens = id.tokens
      val sentenceSB = new StringBuilder
      var shouldIncludeSentence = false
      sentenceSB.append(s"${idString}\t${nlpdata.util.Text.render(sentenceTokens)}\n")
      // sort by keyword group first...
      for(HITInfo(genHIT, genAssignments) <- genInfosBySentenceId(id).sortBy(_.hit.prompt.verbIndex)) {
        // then worker ID second, so the data will be chunked correctly according to HIT;
        for(genAssignment <- genAssignments.sortBy(_.workerId)) {
          // and these should already be ordered in terms of the target word used for a QA pair.
          for((wqa, qaIndex) <- genAssignment.response.zipWithIndex) {
            // pairs of (validation worker ID, validation answer)
            val valResponses = valInfosByGenAssignmentId.get(genAssignment.assignmentId).getOrElse(Nil)
              .flatMap(_.assignments.map(a => (a.workerId, a.response(qaIndex))))
            if(valResponses.size != 2) {
              logger.warn("Warning: don't have 2 validation answers for question. Actual number: " + valResponses.size)
            }
            val valAnswers = valResponses.map(_._2)

            if(keepQA(id, wqa, valAnswers)) {
              shouldIncludeSentence = true
              sentenceSB.append(anonymizeWorker(genAssignment.workerId) + "\t") // anonymized worker ID
              sentenceSB.append(s"${Text.normalizeToken(sentenceTokens(wqa.verbIndex))} (${wqa.verbIndex})\t")
              sentenceSB.append(wqa.question + "\t") // question string written by worker
              sentenceSB.append(
                ((Answer(wqa.answers)) :: valResponses.map(_._2)).map { valAnswer =>
                  QASRLValidationAnswer.render(sentenceTokens, valAnswer, genAssignment.response)
                }.mkString("\t")
              )
              sentenceSB.append("\n")
            }
          }
        }
      }
      if(shouldIncludeSentence) {
        sb.append(sentenceSB.toString)
      }
    }
    sb.toString
  }
}
