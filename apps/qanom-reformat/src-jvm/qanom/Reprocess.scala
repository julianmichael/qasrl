package qasrl.bank.qanom

import cats.data.NonEmptySet
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.implicits._

import fs2.Stream

import java.nio.file.Path

import com.monovore.decline._
import com.monovore.decline.effect._

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.io.FileUtil
import jjm.implicits._

import io.circe.Json
import io.circe.syntax._

import java.nio.file.StandardOpenOption
import scala.collection.immutable.SortedMap
import jjm.ling.en.Inflections

object Reprocess extends CommandIOApp(
  name = "mill -i apps.bank.jvm.runMain qasrl.bank.qanom.Reprocess",
  header = "Reprocess the jsonl-formatted QANom data. Reads from stdin, writes to stdout.",
  version = "0.1.0") {

  val qasrlBankO = Opts.option[Path](
    "qasrl-bank", metavar = "path", help = "QA-SRL Bank 2.0 location."
  )

  val resourcesO = Opts.option[Path](
    "resources", metavar = "path", help = "resources/inflections data location."
  )

  val inO = Opts.option[Path](
    "in", metavar = "path", help = "Input jsonl file location."
  )

  val outO = Opts.option[Path](
    "out", metavar = "path", help = "Output jsonl file location."
  )

  import qasrl._
  import qasrl.data._
  import qasrl.labeling.SlotBasedLabel
  import cats.data.NonEmptyList

  def getPreferredCompleteState(states: NonEmptyList[QuestionProcessor.ValidState]) = {
    // prioritize a frame with Obj (but no Obj2) over one with Obj2 (but no Obj)
    val completeStates = states.collect { case cs @ QuestionProcessor.CompleteState(_, _, _) => cs }
    completeStates.find(_.frame.args.get(Obj).nonEmpty).orElse(completeStates.headOption)
  }

  def readFrameAndSlot(
    sentenceTokens: Vector[String],
    verbInflectedForms: InflectedForms,
    question: String
  ) = {
    val questionTokensIsh = question.init.split(" ").toVector.map(_.lowerCase)
    val qPreps = questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
    val qPrepBigrams = questionTokensIsh
      .sliding(2)
      .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
      .map(_.mkString(" ").lowerCase)
      .toSet
    val stateMachine = new TemplateStateMachine(
      sentenceTokens, verbInflectedForms, Some(qPreps ++ qPrepBigrams)
    )
    val template = new QuestionProcessor(stateMachine)
    val frameWithAnswerSlotOpt = template.processStringFully(question) match {
      case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => None
      case Right(goodStates) => getPreferredCompleteState(goodStates)
    }
    frameWithAnswerSlotOpt.map {
      case QuestionProcessor.CompleteState(_, frame, answerSlot) =>
        frame -> answerSlot
    }.headOption
  }

  val SourceRegex = """Worker-([0-9]+)""".r

  def modifySource(source: String) = source match {
    case SourceRegex(index) => s"turk-qanom-$index"
    case _ => ???
  }

  def fixUpQuestion(question: String) = question match {
    case "Who rebound something?" => "Who rebounded something?"
    case x => x
  }

  def fixUpJson(
    inflections: Inflections,
    inflectionsByForm: Map[LowerCaseString, Set[InflectedForms]])(
    input: Json
  ): Json = {
    val inObj = input.asObject.get
    val tokens = inObj("sentenceTokens").get.as[Vector[String]].right.get
    val (predicateJsons, nonVerbalNounJsons) = inObj("verbEntries").get.asObject.get.toMap
      .partition(_._2.asObject.get("isVerbal").get.as[Boolean].right.get)
    val nonVerbalNouns = nonVerbalNounJsons.map { case (k, v) =>
      k.toInt -> v.asObject.get("verbForm").get.as[String].right.get.lowerCase
    }
    val verbEntries = predicateJsons.map { case (indexStr, verbJson) =>
      val index = indexStr.toInt
      val someVerbForm = verbJson.asObject.get("verbForm").get.as[String].right.get.lowerCase
      val allPossibleInflectedForms = NonEmptyList.fromList(
        inflections.getInflectedForms(someVerbForm).toList ++
          inflectionsByForm.getOrElse(someVerbForm, Set()).toList
      ).getOrElse(
        throw new IllegalArgumentException(s"Inflections not found: $someVerbForm")
      )
      val questionLabelJsons = verbJson.asObject.get("questionLabels").get.asObject.get
        .toVector.map { case (q, j) => fixUpQuestion(q) -> j }
      val (inflectedForms, questionInfos) = allPossibleInflectedForms.toList.flatMap(forms =>
        questionLabelJsons.toVector.map { case (questionString, questionLabelJson) =>
          readFrameAndSlot(tokens, forms, questionString)
        }.sequence.map(forms -> _)
      ).headOption.getOrElse(
        throw new RuntimeException(
          s"No valid inflected forms for verb: ${verbJson.noSpaces}\n" +
            allPossibleInflectedForms.toList.mkString("\n")
        )
      )
      val questionLabels = questionInfos.zip(questionLabelJsons).map {
        case ((frame, slot), (questionString, questionJson)) =>
        val slots = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
          tokens, inflectedForms, List(questionString)
        ).head.get
        val questionSources = questionJson.asObject.get("questionSources").get
          .as[Set[String]].right.get
          .map(modifySource)
        val answerLabels = questionJson.asObject.get("answerJudgments").get
          .as[Set[AnswerLabel]].right.get
          .map(AnswerLabel.sourceId.modify(modifySource))

        // modify question and answer sources to mention qanom.
        questionString -> QuestionLabel(
          questionString = questionString,
          questionSources = questionSources,
          answerJudgments = answerLabels,
          questionSlots = slots,
          tense = frame.tense,
          isPerfect = frame.isPerfect,
          isProgressive = frame.isProgressive,
          isNegated = frame.isNegated,
          isPassive = frame.isPassive
        )
      }
      index -> VerbEntry(
        index, inflectedForms, SortedMap(questionLabels.toList: _*)
      )
    }

    val res = QANomSentence(
      inObj("sentenceId").get.as[String].right.get,
      tokens,
      verbEntries,
      nonVerbalNouns
    )

    res.asJson
  }

  def main: Opts[IO[ExitCode]] = (qasrlBankO, resourcesO, inO, outO)
    .mapN { (qasrlBankPath, resourcesPath, in, out) =>
      val tokensIterator = qasrl.bank.Data.readFromQasrlBank(qasrlBankPath).get.all
        .sentences.iterator.flatMap(_._2.sentenceTokens.iterator)
      val wiktionaryService = new WiktionaryFileSystemService(resourcesPath.resolve("wiktionary"))
      val inflections = wiktionaryService.getInflectionsForTokens(tokensIterator)
      val inflectionsPath = resourcesPath.resolve("wiktionary/en_verb_inflections.txt")
      // val extraInflectionsPath = resourcesPath.resolve("extra-inflections.txt")
      for {
        inflectionsByForm <- Stream.resource(FileUtil.blockingExecutionContext).flatMap { ec =>
          fs2.io.file.readAll[IO](inflectionsPath, ec, 4096)
            .through(fs2.text.utf8Decode)
            .through(fs2.text.lines)
            .map(_.trim)
            .filter(_.nonEmpty)
            .map { line =>
              val f = line.split("\\s+")
              val forms = InflectedForms(
                stem = f(0).lowerCase,
                presentSingular3rd = f(1).lowerCase,
                presentParticiple = f(2).lowerCase,
                past = f(3).lowerCase,
                pastParticiple = f(4).lowerCase
              )
              forms.allForms.foldMap(form => Map(form -> Set(forms)))
            }
        }.compile.foldMonoid
        _ <- Stream.resource(FileUtil.blockingExecutionContext).flatMap { ec =>
          FileUtil.streamJsonLines[Json](in, ec)
            .map(fixUpJson(inflections, inflectionsByForm))
            .map(_.noSpaces)
            .map(x => s"$x\n")
            .through(fs2.text.utf8Encode)
            .through(fs2.io.file.writeAll(out, ec, Seq(StandardOpenOption.CREATE)))
        }.compile.drain
      } yield ExitCode.Success
  }
}
