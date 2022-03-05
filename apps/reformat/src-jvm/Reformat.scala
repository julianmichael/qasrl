package qasrl.apps.reformat

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

object Reformat extends CommandIOApp(
  name = "mill -i apps.reformat.runMain qasrl.apps.reformat.Reformat",
  header = "Reformats QA-SRL GS CSV data into JSON lines.") {

  def main: Opts[IO[ExitCode]] = {
    val inPathO = Opts.option[NIOPath](
      "in", metavar = "path", help = "Input directory."
    )
    val outPathO = Opts.option[NIOPath](
      "out", metavar = "path", help = "Output path."
    )
    (inPathO, outPathO).mapN(run)
  }

  val qasrlBankPath = Paths.get("data/qasrl-v2_1")

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

  import cats.data.NonEmptySet
  import jjm.ling.ESpan
  import scala.collection.immutable.SortedMap
  import scala.collection.immutable.SortedSet
  import qasrl.data._

  case class VerbInfo(
    sentenceId: String,
    sentenceTokens: Vector[String],
    verbIndex: Int,
    verbInflectedForms: InflectedForms,
    originalLines: List[Map[String, String]],
    anonymizationMap: Map[String, Int]
  ) {
    val questions = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
      sentenceTokens, verbInflectedForms, originalLines.map(_.apply("question"))
    ).map(_.getOrElse((println(this): Id[Unit]) >> ???))

    val resolvedFramePairs = ClauseResolution
      .getResolvedFramePairs(verbInflectedForms, questions)

    val questionLabels = (originalLines, questions, resolvedFramePairs).zipped.map { case (line, slots, (frame, argSlot)) =>
      val workerId = anonymizationMap(line("worker_id"))
      val worker = s"turk-qasrlgs-$workerId"
      val answerSpans = NonEmptySet.fromSet(
        SortedSet(
          line("answer_range").split("~!~").map { rangeStr =>
            val rangeList = rangeStr.split(":").view.map(_.toInt).toList
            ESpan(rangeList(0), rangeList(1))
          }: _*
        )
      ).get
      val answer = AnswerLabel(worker, Answer(answerSpans))
      QuestionLabel(
        questionString = slots.renderQuestionString(verbInflectedForms.apply),
        questionSources = Set(worker),
        answerJudgments = Set(answer),
        questionSlots = slots,
        tense = frame.tense,
        isPerfect = frame.isPerfect,
        isProgressive = frame.isProgressive,
        isNegated = frame.isNegated,
        isPassive = frame.isPassive
      )
    }

    val verbEntry = VerbEntry(
      verbIndex, verbInflectedForms,
      SortedMap(questionLabels.map(ql => ql.questionString -> ql): _*)
    )
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

  import qasrl.bank.ConsolidatedDataset

  def getMetricsString[M: HasMetrics](m: M) =
    m.getMetrics.toStringPrettySorted(identity, x => x.render, sortSpec)

  def readFile(qasrlData: Dataset, anonMap: Map[String, Int], inPath: NIOPath): IO[ConsolidatedDataset] = {
    readCSV(inPath)(
      (headers, lines) => lines
        .groupAdjacentBy(fields => fields("qasrl_id") -> fields("verb_idx"))
        .map { case ((sid, verbIndexStr), chunk) =>
          val verbIndex = verbIndexStr.toInt
          val sentence = qasrlData.sentences(sid)
          val verbInfo = VerbInfo(
            sid,
            sentence.sentenceTokens,
            verbIndex,
            sentence.verbEntries(verbIndex.toInt).verbInflectedForms,
            chunk.toList,
            anonMap
          )
          Sentence(sentence.sentenceId, sentence.sentenceTokens, SortedMap(verbIndex -> verbInfo.verbEntry))
        }.compile.toList
    ).map(sentences => ConsolidatedDataset.fromDataset(Dataset(SortedMap(sentences.map(s => s.sentenceId -> s): _*))))
  }

  val filenames = List(
    "wikinews.dev.gold.csv",
    "wikinews.test.gold.csv",
    "wikipedia.dev.gold.csv",
    "wikipedia.test.gold.csv",
  )

  def constructAnonMap(inPath: NIOPath): IO[Map[String, Int]] = {
    def getIds(path: NIOPath): IO[Set[String]] = readCSV(path)(
      (headers, lines) => lines.map(fields => fields("worker_id")).compile.toList.map(_.toSet)
    )

    filenames.traverse(fn => getIds(inPath.resolve(fn)))
      .map(_.combineAll.toList)
      .map(scala.util.Random.shuffle(_))
      .map(_.zipWithIndex.toMap)
  }

  implicit val datasetMonoid = ConsolidatedDataset.consolidatedDatasetMonoid(Dataset.printMergeErrors)

  import jjm.io.FileUtil

  def run(inPath: NIOPath, outPath: NIOPath): IO[ExitCode] = for {
    qasrlData <- IO(Data.readFromQasrlBank(qasrlBankPath).get).map(_.all)
    anonMap <- constructAnonMap(inPath)
    wikipediaDev <- readFile(qasrlData, anonMap, inPath.resolve("wikipedia.dev.gold.csv"))
    wikinewsDev <- readFile(qasrlData, anonMap, inPath.resolve("wikinews.dev.gold.csv"))
    wikipediaTest <- readFile(qasrlData, anonMap, inPath.resolve("wikipedia.test.gold.csv"))
    wikinewsTest <- readFile(qasrlData, anonMap, inPath.resolve("wikinews.test.gold.csv"))
    dev = wikipediaDev |+| wikinewsDev
    test = wikipediaTest |+| wikinewsTest
    _ <- IO(println("Dev sentences: " + dev.sentences.size))
    _ <- IO(println("Test sentences: " + test.sentences.size))
    _ <- FileUtil.writeJsonLines(outPath.resolve("dev.jsonl.gz"))(dev.sentences.values.toSeq)
    _ <- FileUtil.writeJsonLines(outPath.resolve("test.jsonl.gz"))(test.sentences.values.toSeq)
  } yield ExitCode.Success
}





