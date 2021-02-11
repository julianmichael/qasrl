package qasrl.apps.align

import java.nio.file.Path

import fs2.Stream

import qasrl.ArgumentSlot
import qasrl.ArgStructure
import qasrl.bank.ConsolidatedData
import qasrl.bank.Data
import qasrl.data._
import qasrl.labeling.SlotBasedLabel
import qasrl.labeling.ClauseResolution

import com.monovore.decline._
import com.monovore.decline.effect._

import cats.Id
import cats.effect._
import cats.implicits._

import jjm.io.FileUtil
import jjm.implicits._

import io.circe.generic.JsonCodec
import _root_.qasrl.bank.ConsolidatedSentence
import jjm.ling.Text
import jjm.ling.ESpan
import jjm.metrics._

@JsonCodec case class SentenceQuestionAlignment(
  sentenceId: String,
  verbs: Map[Int, VerbQuestionAlignment]
)

@JsonCodec case class VerbQuestionAlignment(
  verbIndex: Int,
  source: String,
  questions: Map[String, SingleQuestionAlignment]
)

@JsonCodec case class SingleQuestionAlignment(
  numUnalignedPlaceholders: Int,
  alignments: Map[String, List[Int]]
)

object ConvertConsolidatedQASRL extends CommandIOApp(
  name = "mill -i apps.align.runMain qasrl.apps.align.ConvertConsolidatedQASRL",
  header = "Runs clause resolution on the consolidated QA-SRL Bank.") {

  def main: Opts[IO[ExitCode]] = {
    val qasrlBankO = Opts.option[Path](
      "qasrl-bank", metavar = "path", help = "Path to the QA-SRL Bank 2.0 data, e.g., ../qasrl-bank/data/qasrl-v2."
    )
  // val qasrlBankPath = Paths.get("../qasrl-bank/data/qasrl-v2_1")

    val qaNomO = Opts.option[Path](
      "qanom", metavar = "path", help = "Path to the QA-Nom data."
    )
    val outPathO = Opts.option[Path](
      "out", metavar = "path", help = "Output path."
    )
    (qasrlBankO, qaNomO, outPathO).mapN { case (qasrlBankPath, qaNomPath, outPath) =>
      for {
        qasrlData <- IO.fromTry(Data.readFromQasrlBank(qasrlBankPath))
        allData <- IO.fromTry(Data.loadQANomData(qasrlData.small, qaNomPath))
        _ <- FileUtil.writeJsonLinesStreaming(outPath, io.circe.Printer.noSpaces)(
          sentenceQuestionAlignments(allData)
        )
        metrics <- alignmentMetrics(allData)
        _ <- IO(println(ConvertCSV.getMetricsString(metrics).split("\n").view.take(40).mkString("\n")))
      } yield ExitCode.Success
    }
  }

  def sentenceQuestionAlignments(data: ConsolidatedData) = {
    Stream
      .fromIterator[IO](data.all.sentences.iterator)
      .map { case (sid, sentence) =>
        SentenceQuestionAlignment(
          sid, sentence.verbEntries.toMap.transform { case (_, verb) =>
            getVerbQuestionAlignment(sentence.sentenceTokens, verb)
          }
        )
      }
  }

  def alignmentMetrics(data: ConsolidatedData) = {
    Stream
      .fromIterator[IO](data.all.sentences.iterator)
      .map { case (sid, sentence) =>
        sentence.verbEntries.values.toList.foldMap { verb =>
          getAlignmentMetrics(sentence.sentenceTokens, verb)
        }
      }.compile.foldMonoid
  }

  def recapitalizeSentence(sent: Vector[String]): Vector[String] = {
    if(Text.normalizeToken(sent(0)) == "\"") { // ignore starting quotes
      sent(0) +: recapitalizeSentence(sent.tail)
    } else if(sent(0).size > 1 && sent(0).charAt(1).isUpper) { // second char of first word upper?
      sent
    } else if(sent.size > 1 && sent(1).charAt(0).isUpper) { // first char of secont word upper?
      sent
    } else { // otherwise we assume first latter capitalized only bc sentence beginning
      sent.updated(0, sent(0).updated(0, sent(0).charAt(0).toLower))
    }
  }

  def getAlignmentMetrics(sentenceTokens: Vector[String], verb: VerbEntry) = {
    val questions = verb.questionLabels.values.toList

    val recapSent = recapitalizeSentence(sentenceTokens)

    val answerSpanCounts = questions
      .map(_.answerJudgments.toList.flatMap(_.judgment.getAnswer).flatMap(_.spans.toList).counts)

    val resolvedFramePairs = ClauseResolution.getResolvedFramePairs(
      verb.verbInflectedForms, questions.map(_.questionSlots)
    )

    val aligner = QuestionAligner(resolvedFramePairs.zip(answerSpanCounts))

    aligner.metrics
  }

  def getVerbQuestionAlignment(sentenceTokens: Vector[String], verb: VerbEntry): VerbQuestionAlignment = {
    val questions = verb.questionLabels.values.toList

    val recapSent = recapitalizeSentence(sentenceTokens)

    val answerSpanCounts = questions
      .map(_.answerJudgments.toList.flatMap(_.judgment.getAnswer).flatMap(_.spans.toList).counts)

    val resolvedFramePairs = ClauseResolution.getResolvedFramePairs(
      verb.verbInflectedForms, questions.map(_.questionSlots)
    )

    val source = questions.headOption.fold("n/a")(question =>
      if(question.questionSources.exists(_.contains("qanom"))) "qanom" else "qasrl-2.0"
    )

    val aligner = QuestionAligner(resolvedFramePairs.zip(answerSpanCounts))

    val questionAlignments = questions.zip(resolvedFramePairs).map { case (questionLabel, (frame, slot)) =>
      val clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
      val allSlots = clauseTemplate.args.keys.toList
      val slotAnswers = allSlots.flatMap { (slot: ArgumentSlot) =>
        aligner.getAlignedAnswers(clauseTemplate -> slot).map(_.toList.map(slot -> _))
      }
      val argMappings = slotAnswers.sequence.map(_.toMap)

      val numUnalignedSlots = allSlots.size - slotAnswers.size

      def renderAlignedQuestion(choice: Map[ArgumentSlot, (ESpan, Int)]): Map[String, Map[ArgumentSlot, Int]] = {
        val argValues = choice.transform { case (_, (span, _)) => Text.renderSpan(recapSent, span) }
        val counts = (choice - slot).transform { case (k, v) => v._2 }
        Map(frame.questionsForSlotWithArgs(Some(slot), argValues).head -> counts)
      }
      val questionMap = argMappings.foldMap(renderAlignedQuestion)
        .transform { case (k, v) => v.values.toList }

      questionLabel.questionString -> SingleQuestionAlignment(numUnalignedSlots, questionMap)
    }.toMap

    VerbQuestionAlignment(verb.verbIndex, source, questionAlignments)
  }
}
