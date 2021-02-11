package qasrl.apps.align

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.{Path => NIOPath}

import com.github.tototoshi.csv._

import com.monovore.decline._
import com.monovore.decline.effect._

import shapeless.syntax.singleton._
// import shapeless.labelling._
import shapeless._

import cats.effect._
import cats.implicits._

import fs2.Stream

import jjm.DependentMap
import jjm.ling.en.InflectedForms
import jjm.metrics._
import jjm.implicits._

import qasrl._
import qasrl.bank.Data
import qasrl.data.Dataset
import qasrl.labeling.SlotBasedLabel
import qasrl.labeling.ClauseResolution

object ConvertCSV extends CommandIOApp(
  name = "mill -i apps.align.runMain qasrl.apps.align.ConvertCSV",
  header = "Runs clause resolution on CSV inputs and reports results.") {

  def main: Opts[IO[ExitCode]] = {
    val inPathO = Opts.option[NIOPath](
      "in", metavar = "path", help = "Input CSV path."
    )
    val outPathO = Opts.option[NIOPath](
      "out", metavar = "path", help = "Output path."
    )
    (inPathO, outPathO).mapN(run)
  }

  val qasrlBankPath = Paths.get("../qasrl-bank/data/qasrl-v2_1")

  def readCSV[A](path: NIOPath)(f: (List[String], Stream[IO, Map[String, String]]) => IO[A]): IO[A] = {
    val format = if(path.toString.endsWith(".tsv")) new TSVFormat{} else new DefaultCSVFormat {}
    Bracket[IO, Throwable].bracket(
      acquire = IO(CSVReader.open(new File(path.toString))(format)))(
      use = reader => {
        IO(reader.readNext.get) >>= (headers =>
          f(headers,
            Stream.fromIterator[IO](
              reader.all.iterator.map(line => headers.zip(line).toMap)
            )
          )
        )
      })(
      release = reader => IO(reader.close()))
  }

  def writeCSV(path: NIOPath, headers: List[String])(lines: Stream[IO, Map[String, String]]): IO[Unit] = {
    Bracket[IO, Throwable].bracket(
      acquire = IO(CSVWriter.open(new File(path.toString))))(
      use = writer => {
        IO(writer.writeRow(headers)) >>
          lines.evalMap { line =>
            IO(writer.writeRow(headers.map(line)))
          }.compile.drain
      })(
      release = writer => IO(writer.close()))
  }

  lazy val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double = (mv: Metric) => mv match {
      case MetricMetadata(s) => 0.0
      case MetricBool(x) => if(x) 1.0 else 0.0
      case MetricInt(x) => x.toDouble
      case MetricDouble(x) => x
      case MetricIntOfTotal(x, _) => x.toDouble
    }
    val inc = value[String](double)
    val dec = value[String](double andThen (_ * -1))
    List(dec)
  }

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

  val newHeaders = List(
    "aligned_question",
    "arg_slot",
    "clause",
    "templated_clause",
    "ng_clause",
    "neg_flipped_ng_clause",
    "to_clause",
    "inv_clause",
    "inv_neg_would_clause"
  )

  case class VerbInfo(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbIndex: Int,
    verbInflectedForms: InflectedForms,
    originalLines: List[Map[String, String]]
  ) {
    val questions = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
      sentenceTokens, verbInflectedForms, originalLines.map(_.apply("question"))
    ).map(_.getOrElse((println(this): Id[Unit]) >> ???))

    val recapitalize = (s: String) => {
      if(s.isEmpty) s
      else if(
        s.tail.headOption.exists(_.isUpper) ||
          (s.dropWhile(_ != ' ').nonEmpty &&
              s.dropWhile(_ != ' ').tail.headOption.exists(_.isUpper))) s
      else s.updated(0, s.charAt(0).toLower)
    }

    val resolvedFramePairs = ClauseResolution
      .getResolvedFramePairs(verbInflectedForms, questions)

    val resolvedQAPairs = resolvedFramePairs.zip(
      originalLines.map(f => f("answer").split("~!~").toList.map(recapitalize))
    )

    val aligner = QuestionAligner(resolvedQAPairs)

    val newLines = (originalLines, resolvedFramePairs).zipped.map {
      case (fields, (frame, slot)) =>
        val templatedFrame = frame.copy(
          tense = Tense.Finite.Present, isPerfect = false,
          isProgressive = false, isNegated = false
        )
        val clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
        val argMappings = clauseTemplate.args.keys.toList.flatMap { (slot: ArgumentSlot) =>
          aligner.getAlignedAnswers(clauseTemplate -> slot).map(_.map(slot -> _))
        }.sequence.map(_.toMap)

        val tenseLens = Frame.tense
        val negLens = Frame.isNegated

        def renderFrameStrings(getString: Map[ArgumentSlot, String] => String) =
          argMappings.map(getString).toSet.mkString("~!~")

        val decl = renderFrameStrings(
          argValues => frame.clausesWithArgs(argValues).head
        )

        val ngClause = renderFrameStrings(
          argValues => tenseLens
            .set(Tense.NonFinite.Gerund)(frame)
            .clausesWithArgs(argValues).head
        )
        val negFlippedNgClause = renderFrameStrings(
          argValues => tenseLens
            .set(Tense.NonFinite.Gerund)(
              negLens.modify(!_)(frame)
            ).clausesWithArgs(argValues).head
        )
        val toClause = renderFrameStrings(
          argValues => tenseLens
            .set(Tense.NonFinite.To)(frame)
            .clausesWithArgs(argValues).head
        )
        val invClause = renderFrameStrings(
          argValues => frame
            .questionsForSlotWithArgs(None, argValues).head.init
        )
        val invNegWouldClause = renderFrameStrings(
          argValues => tenseLens
            .set(Tense.Finite.Modal("would".lowerCase))(
              negLens.set(true)(frame)
            )
            .questionsForSlotWithArgs(None, argValues).head.init
        )

        fields +
          ("aligned_question" -> renderFrameStrings(argMapping =>
             frame.questionsForSlotWithArgs(Some(slot), argMapping).head)) +
          ("arg_slot" -> slot.toString) +
          ("clause" -> decl) +
          ("templated_clause" -> templatedFrame.clauses.head) +
          ("ng_clause" -> ngClause) +
          ("neg_flipped_ng_clause" -> negFlippedNgClause) +
          ("to_clause" -> toClause) +
          ("inv_clause" -> invClause) +
          ("inv_neg_would_clause" -> invNegWouldClause)
    }

  }

  def runForFile(qasrlData: Dataset, inPath: NIOPath, outPath: NIOPath) = for {
    (verbs, headers) <- readCSV(inPath)(
      (headers, lines) => lines
        .groupAdjacentBy(fields => fields("qasrl_id") -> fields("verb_idx"))
        .map { case ((sid, verbIndex), chunk) =>
          val sentence = qasrlData.sentences(sid)
          VerbInfo(
            sid,
            sentence.sentenceTokens,
            verbIndex.toInt,
            sentence.verbEntries(verbIndex.toInt).verbInflectedForms,
            chunk.toList)
        }.compile.toList.map(_ -> headers)
    )
    _ <- writeCSV(outPath, headers ++ newHeaders)(
      Stream.emits(verbs.flatMap(_.newLines))
    )
  } yield verbs.foldMap(_.aligner.metrics)

  def run(inPath: NIOPath, outPath: NIOPath): IO[ExitCode] = for {
    qasrlData <- IO(Data.readFromQasrlBank(qasrlBankPath).get).map(_.all)
    metrics <- IO(Files.isDirectory(inPath)).ifM(
      IO(new File(inPath.toString).listFiles.toList.map(f => Paths.get(f.getPath))) >>= (paths =>
        paths.filter(_.toString.endsWith(".silver.csv")).foldMapM { inPath =>
          val newName = inPath.getFileName.toString.replaceAll("\\.[^.]*$", ".aligned.csv")
          val newPath = outPath.resolve(newName)
          runForFile(qasrlData, inPath, newPath) <* IO(
            println(s"Wrote alignments of $inPath to $newPath")
          )
        }
      ),
      runForFile(qasrlData, inPath, outPath)
    )
    _ <- IO(println(getMetricsString(metrics)))
  } yield ExitCode.Success
}
