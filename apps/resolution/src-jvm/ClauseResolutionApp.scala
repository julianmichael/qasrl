package qasrl.apps.resolution

import cats._
import cats.effect._
import cats.implicits._

import java.nio.file._

import qasrl._
import qasrl.data._
import qasrl.labeling._

import qasrl.bank._

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.ling.en.Inflections
import jjm.ling.en.VerbForm
import jjm.implicits._

import jjm.io.FileUtil

import io.circe.Json
import io.circe.generic.JsonCodec
import io.circe.syntax._

import scala.util.Random

import com.monovore.decline._
import com.monovore.decline.effect._

import fs2.Stream

object ClauseResolutionApp extends CommandIOApp(
  name = "qfirst.ClauseResolutionApp",
  header = "Runs clause resolution on the relevant portion of the QA-SRL Bank 2.1 and outputs to a file.") {

  import ClauseResolution._

  val paths = List(
    Paths.get("qasrl-v2_1").resolve("expanded").resolve("train.jsonl.gz"),
    Paths.get("qasrl-v2_1").resolve("expanded").resolve("dev.jsonl.gz")
  )

  def processSentence(sentence: Sentence): String = {
    val printer = io.circe.Printer.noSpaces
    printer.print(
      Json.obj(
        "sentenceId" -> sentence.sentenceId.asJson,
        "verbs" -> Json.obj(
          sentence.verbEntries.values.toList.map { verb =>
            val qLabels = verb.questionLabels.toList.map(_._2)
            val framePairs = getResolvedFramePairs(verb.verbInflectedForms, qLabels.map(_.questionSlots))
            verb.verbIndex.toString -> Json.obj(
              qLabels.zip(framePairs).map { case (qLabel, (frame, slot)) =>
                qLabel.questionString -> Json.obj(
                  "clause" -> printer.print(getClauseTemplate(frame).asJson).asJson,
                  "slot" -> ArgumentSlot.toString(slot).asJson
                )
              }: _*
            )
          }: _*
        )
      )
    )
  }

  def main: Opts[IO[ExitCode]] = {
    val outPathO = Opts.option[Path](
      "out", metavar = "path", help = "Output path."
    )
    outPathO.map(outPath =>
      Stream.resource(Blocker[IO]).flatMap { blocker  =>
        Stream.emits[IO, Path](paths)
          .flatMap(path => FileUtil.streamJsonLines[Sentence](path, blocker))
          .map(processSentence)
          .intersperse("\n")
          .through(fs2.text.utf8Encode)
          .through(fs2.io.file.writeAll(outPath, blocker))
      }.compile.drain.as(ExitCode.Success)
    )
  }
}
