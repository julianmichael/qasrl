package qasrl.bank

import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import scala.util.Try

import java.nio.file.Path
import java.nio.file.Files

import cats.Id
import cats.implicits._

import qasrl.data.Dataset
import qasrl.data.Sentence

case class FullQasrlData(
  index: DataIndex,
  all: Dataset,
  documentsById: Map[DocumentId, Document],
  trainOrig: Dataset,
  devOrig: Dataset,
  testOrig: Dataset,
  trainExpanded: Dataset,
  devExpanded: Dataset,
  devDense: Dataset,
  testDense: Dataset
) {
  def small = ConsolidatedData(index, ConsolidatedDataset.fromDataset(all), documentsById)
}

case class ConsolidatedData(
  index: DataIndex,
  all: ConsolidatedDataset,
  documentsById: Map[DocumentId, Document]
)

object Data {

  def filterExpandedToOrig(dataset: Dataset) = {
    val withoutQuestions =
      dataset.filterQuestionSources(qs => QuestionSource.fromString(qs).isQasrlTurker)
    val withoutAnswers = Dataset.questionLabels.modify(
      ql =>
        ql.copy(
          answerJudgments = ql.answerJudgments.filter(
            al => AnswerSource.fromString(al.sourceId).round.isQasrlOriginal
          )
      )
    )(withoutQuestions)
    withoutAnswers
  }

  def readConsolidatedDataset(path: Path): Try[ConsolidatedDataset] = Try {
    import java.io.FileInputStream
    import java.util.zip.GZIPInputStream
    val source = scala.io.Source.fromInputStream(
      new GZIPInputStream(new FileInputStream(path.toString))
    )
    ConsolidatedDataset(
      SortedMap(
        source.getLines().map { line =>
          val sentence = io.circe.jawn.decode[ConsolidatedSentence](line).right.get
          sentence.sentenceId -> sentence
        }.toSeq: _*
      )
    )
  }

  def readQasrlDataset(path: Path): Try[Dataset] = Try {
    import java.io.FileInputStream
    import java.util.zip.GZIPInputStream
    val source = scala.io.Source.fromInputStream(
      new GZIPInputStream(new FileInputStream(path.toString))
    )
    Dataset(
      SortedMap(
        source.getLines().map { line =>
          val sentence = io.circe.jawn.decode[Sentence](line).right.get
          sentence.sentenceId -> sentence
        }.toSeq: _*
      )
    )
  }

  def writeDatasetUnzipped(path: Path, dataset: Dataset) = Try {
    import io.circe.generic.auto._
    import io.circe.syntax._
    val printer = io.circe.Printer.noSpaces
    Files.write(path, printer.print(dataset.asJson).getBytes("UTF-8"))
  }

  def writeDatasetJS(path: Path, dataset: Dataset) = Try {
    import io.circe.generic.auto._
    import io.circe.syntax._
    val printer = io.circe.Printer.noSpaces
    Files.write(path, ("var dataset = " + printer.print(dataset.asJson) + ";").getBytes("UTF-8"))
  }

  import io.circe.syntax._

  def writeIndex(path: Path, index: DataIndex) = Try {
    val printer = io.circe.Printer.noSpaces
    Files.write(path, printer.print(index.asJson).getBytes("UTF-8"))
  }

  def readIndexZipped(path: Path): Try[DataIndex] = Try {
    import java.io.FileInputStream
    import java.util.zip.GZIPInputStream
    val source = scala.io.Source.fromInputStream(
      new GZIPInputStream(new FileInputStream(path.toString))
    )
    // weirdness is so we actually close the file... yeah it's dumb, but I just don't care rn
    var jsonString: String = null
    source.getLines().foreach { line =>
      if (jsonString == null) jsonString = line
    }
    io.circe.jawn.decode[DataIndex](jsonString) match {
      case Right(index) => index
      case Left(err)    => println(err); ???
    }
  }

  def readFromQasrlBank(qasrlBankPath: Path): Try[FullQasrlData] = for {
    trainExpanded <- readQasrlDataset(qasrlBankPath.resolve("expanded").resolve("train.jsonl.gz"))
    devExpanded <- readQasrlDataset(qasrlBankPath.resolve("expanded").resolve("dev.jsonl.gz"))
    // avoid having to read more files since result is the same anyway
    trainOrig = filterExpandedToOrig(trainExpanded)
    devOrig = filterExpandedToOrig(devExpanded)
    testOrig <- readQasrlDataset(qasrlBankPath.resolve("orig").resolve("test.jsonl.gz"))
    devDense <- readQasrlDataset(qasrlBankPath.resolve("dense").resolve("dev.jsonl.gz"))
    testDense <- readQasrlDataset(qasrlBankPath.resolve("dense").resolve("test.jsonl.gz"))
    index <- readIndexZipped(qasrlBankPath.resolve("index.json.gz"))
  } yield {
    implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)
    val all = trainExpanded |+| devExpanded |+| testOrig |+| devDense |+| testDense

    val sentenceIdToPart = (
      trainExpanded.sentences.keySet.toSet[String].map(
        s => SentenceId.fromString(s) -> DatasetPartition.Train
      ) ++ devExpanded.sentences.keySet.toSet[String].map(
        s => SentenceId.fromString(s)   -> DatasetPartition.Dev
      ) ++ testOrig.sentences.keySet.toSet[String].map(
        s => SentenceId.fromString(s)      -> DatasetPartition.Test
      )
    ).toMap

    val denseIds = (devDense.sentences.keySet ++ testDense.sentences.keySet)
      .map(SentenceId.fromString)

    val documentsById = {

      val sentencesByDocId = all.sentences.values.toSet
        .groupBy((s: Sentence) => SentenceId.fromString(s.sentenceId).documentId)

      val docIdToMeta = index.documents.values.reduce(_ union _).map(meta => meta.id -> meta).toMap

      val documents = sentencesByDocId.iterator.map {
        case (docId @ DocumentId(domain, idString), sentences) =>
          def makeDocumentMetadata(title: String) = DocumentMetadata(
            docId,
            sentenceIdToPart(SentenceId.fromString(sentences.head.sentenceId)),
            title
          )
          val metadata = docIdToMeta(docId)
          Document(metadata, SortedSet(sentences.toSeq.map(ConsolidatedSentence.fromSentence): _*))
      }.toSeq

      val documentsById = documents.map(doc => doc.metadata.id -> doc).toMap

      documentsById
    }

    FullQasrlData(
      index,
      all,
      documentsById,
      trainOrig,
      devOrig,
      testOrig,
      trainExpanded,
      devExpanded,
      devDense,
      testDense
    )
  }

  def loadQANomData(qasrlData: ConsolidatedData, qaNomPath: Path): Try[ConsolidatedData] = for {
    train <- readConsolidatedDataset(qaNomPath.resolve("train.jsonl.gz"))
    dev   <- readConsolidatedDataset(qaNomPath.resolve("dev.jsonl.gz"))
    test  <- readConsolidatedDataset(qaNomPath.resolve("test.jsonl.gz"))
  } yield {
    implicit val consolidatedDatasetMonoid = ConsolidatedDataset
      .consolidatedDatasetMonoid(Dataset.printMergeErrors)
    val allQANom = train |+| dev |+| test
    // def sentenceIdToPart(sid: SentenceId) = {
    //   qasrlData.index.documents.iterator.find(_._2.exists(_.id == sid.documentId)).map(_._1).get
    // }
    val qaNomIds = allQANom.sentences.keySet.map(SentenceId.fromString)
    val index = qasrlData.index.copy(qaNomIds = qaNomIds)

    val all = qasrlData.all |+| allQANom

    val docIdToMeta = index.documents.values.reduce(_ union _).map(meta => meta.id -> meta).toMap
    val documentsById = all.sentences.values.toVector
      .groupBy(s => SentenceId.fromString(s.sentenceId).documentId)
      .map { case (docId, sentences) =>
        val metadata = docIdToMeta(docId)
        docId -> Document(metadata, SortedSet(sentences: _*))
      }
    ConsolidatedData(
      index, all, documentsById
    )
  }
}
