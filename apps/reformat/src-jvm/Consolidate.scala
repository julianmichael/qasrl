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

object Consolidate extends CommandIOApp(
  name = "mill -i apps.reformat.runMain qasrl.apps.reformat.Consolidate",
  header = "Consolidates JSON lines - formatted QA-SRL data into a single dataset.") {

  def main: Opts[IO[ExitCode]] = {
    val outPathO = Opts.option[NIOPath](
      "out", metavar = "path", help = "Output path."
    )
    outPathO.map(run)
  }

  val qasrlBankPath = Paths.get("data/qasrl-v2_1")
  val qaNomPath = Paths.get("data/qanom")
  val qasrlGsPath = Paths.get("data/qasrl-gs")

  // implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)
  
  import jjm.io.FileUtil
  import qasrl.bank.ConsolidatedData
  import qasrl.bank.ConsolidatedDataset

  val createDir = (path: NIOPath) => {
    IO(!Files.exists(path))
      .ifM(IO(Files.createDirectories(path)), IO.unit)
  }


  def maybeWriteDataset(path: NIOPath, dataset: ConsolidatedDataset) = {
    if(dataset.sentences.isEmpty) IO.unit else {
      FileUtil.writeJsonLines(path)(dataset.sentences.toSeq)
    }
  }

  def writeConsolidatedData(path: NIOPath, data: ConsolidatedData): IO[Unit] = for {
    _ <- createDir(path)
    _ <- maybeWriteDataset(path.resolve("train.jsonl.gz"), data.train)
    _ <- maybeWriteDataset(path.resolve("dev.jsonl.gz"), data.dev)
    _ <- maybeWriteDataset(path.resolve("test.jsonl.gz"), data.test)
  } yield ()

  def run(outPath: NIOPath): IO[ExitCode] = for {
    qasrlBank <- IO.fromTry(Data.readFromQasrlBank(qasrlBankPath))
    _ <- writeConsolidatedData(outPath.resolve("qasrl-v2"), qasrlBank.small)
    qasrlWithQaNom <- IO.fromTry(Data.loadQANomData(qasrlBank.small, qaNomPath))
    allData <- IO.fromTry(Data.loadQasrlGsData(qasrlWithQaNom, qasrlGsPath))
    _ <- writeConsolidatedData(outPath.resolve("consolidated"), allData)
    _ <- FileUtil.writeJson(outPath.resolve("index.json.gz"))(allData.index)

    // anonMap <- constructAnonMap(inPath)
    // wikipediaDev <- readFile(qasrlData, anonMap, inPath.resolve("wikipedia.dev.gold.csv"))
    // wikinewsDev <- readFile(qasrlData, anonMap, inPath.resolve("wikinews.dev.gold.csv"))
    // wikipediaTest <- readFile(qasrlData, anonMap, inPath.resolve("wikipedia.test.gold.csv"))
    // wikinewsTest <- readFile(qasrlData, anonMap, inPath.resolve("wikinews.test.gold.csv"))
    // dev = wikipediaDev |+| wikinewsDev
    // test = wikipediaTest |+| wikinewsTest
    // _ <- IO(println("Dev sentences: " + dev.sentences.size))
    // _ <- IO(println("Test sentences: " + test.sentences.size))
    // _ <- FileUtil.writeJsonLines(outPath.resolve("dev.jsonl.gz"))(dev.sentences.values.toSeq)
    // _ <- FileUtil.writeJsonLines(outPath.resolve("test.jsonl.gz"))(test.sentences.values.toSeq)
  } yield ExitCode.Success
}




