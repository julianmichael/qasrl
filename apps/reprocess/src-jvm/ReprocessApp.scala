package qasrl.apps.reprocess

import cats.Id
import cats.Monad
import cats.implicits._
import cats.data.NonEmptyList
import cats.data.Writer
import cats.effect.IO
import cats.effect.ExitCode

import com.monovore.decline._
import com.monovore.decline.effect._

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.Text
import jjm.ling.en.InflectedForms
import jjm.ling.en.VerbForm
import jjm.implicits._

import qasrl.bank.ConsolidatedSentence
import qasrl.bank.Data
import qasrl.bank.FullQasrlData
import qasrl.bank.SentenceId

import qasrl.data.Dataset
import qasrl.data.QuestionLabel
import qasrl.data.Sentence
import qasrl.data.VerbEntry

import qasrl.labeling.SlotBasedLabel

object ReprocessApp extends CommandIOApp(
  name = "mill -i apps.reprocess.run",
  header = "Do the data reprocessing on QA-SRL."){

  def getMismatchInfo(
    sentence: Sentence,
    verb: VerbEntry,
    qLabel: QuestionLabel
  ): Option[String] = {
    val oldSlots = qLabel.questionSlots
    val newSlotsOpt = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
      sentence.sentenceTokens,
      verb.verbInflectedForms,
      List(qLabel.questionString)
    ).head
    newSlotsOpt match {
      case None => Some("Cannot reconstruct slots for question: " + qLabel.questionString)
      case Some(newSlots) => if(oldSlots != newSlots) Some {
        val oldString = oldSlots.renderWithSeparator(verb.verbInflectedForms.apply, " ")
        val newString = newSlots.renderWithSeparator(verb.verbInflectedForms.apply, " ")
        s"Old:$oldString%-40s New: $newString%-40s"
      } else None
    }
  }

  def getAllMismatchInfo(dataset: Dataset): Vector[String] = {
    dataset.sentences.values.toList.foldMap { sentence =>
      sentence.verbEntries.values.toList.foldMap { verb =>
        verb.questionLabels.foldMap { questionLabel =>
          getMismatchInfo(sentence, verb, questionLabel).toVector
        }
      }
    }
  }

  def getSlotVocabularies: (Dataset => Map[String, Map[LowerCaseString, Vector[QuestionLabel]]]) = {
    Dataset.questionLabels.foldMap { qLabel =>
      val slots = qLabel.questionSlots
      List(
        Some("wh" -> slots.wh),
        slots.aux.map("aux" -> _),
        slots.subj.map("subj" -> _),
        Some("verb" -> (slots.verbPrefix ++ List(slots.verb.toString.lowerCase)).mkString(" ").lowerCase),
        slots.obj.map("obj" -> _),
        slots.prep.map("prep" -> _),
        slots.obj2.map("obj2" -> _)
      ).flatten.map { case (k, v) => k -> Map(v -> Vector(qLabel)) }.toMap
    }
  }

  sealed trait Log[F[_]] extends (String => F[Unit]) {
    def apply(s: String): F[Unit]
    final def any(a: Any): F[Unit] = apply(a.toString)
  }
  object Log {
    val writer: Log[Writer[Vector[String], *]] = new Log[Writer[Vector[String], *]] {
      override def apply(s: String) = Writer.tell[Vector[String]](Vector(s))
    }
    val nop: Log[Id] = new Log[Id] {
      override def apply(s: String) = ()
    }
    val console: Log[IO] = new Log[IO] {
      override def apply(s: String) = IO { println(s) }
    }
  }

  def reprocessDataset(old: Dataset): Dataset = {
    import qasrl.TemplateStateMachine
    import qasrl.QuestionProcessor
    import qasrl.labeling.SlotBasedLabel

    def fixQuestionLabel(verbInflectedForms: InflectedForms, questionLabel: QuestionLabel) = {
      val newSlotsOpt = SlotBasedLabel.getVerbTenseAbstractedSlotsForQuestion(
        Vector(), verbInflectedForms, List(questionLabel.questionString)
      ).head
      newSlotsOpt.map(s => questionLabel.copy(questionSlots = s))
    }

    val fixAllQuestionStrings = Dataset.verbEntries.modify { verbEntry =>
      verbEntry.copy(
        questionLabels = verbEntry.questionLabels.flatMap {
          case (qString, qLabel) =>
            fixQuestionLabel(verbEntry.verbInflectedForms, qLabel).map(
              qString -> _
            )
        }
      )
    }

    fixAllQuestionStrings(old.cullQuestionlessVerbs.filterSentences(_.verbEntries.nonEmpty))
  }

  // mostly copied from QA-SRL Bank client library
  def reprocessAllData(old: FullQasrlData): FullQasrlData = {

    import qasrl.bank.qasrlDataSentenceOrder
    import qasrl.bank.DatasetPartition
    import qasrl.bank.Document
    import qasrl.bank.DocumentId
    import qasrl.bank.DocumentMetadata
    import scala.collection.immutable.SortedSet

    val trainExpanded = reprocessDataset(old.trainExpanded) // readDataset(qasrlBankPath.resolve("expanded").resolve("train.jsonl.gz"))
    val devExpanded = reprocessDataset(old.devExpanded) // readDataset(qasrlBankPath.resolve("expanded").resolve("dev.jsonl.gz"))

    // avoid having to read more files since result is the same anyway
    val trainOrig = Data.filterExpandedToOrig(trainExpanded)
    val devOrig = Data.filterExpandedToOrig(devExpanded)
    val testOrig = reprocessDataset(old.testOrig) // readDataset(qasrlBankPath.resolve("orig").resolve("test.jsonl.gz"))

    val devDense = reprocessDataset(old.devDense) // readDataset(qasrlBankPath.resolve("dense").resolve("dev.jsonl.gz"))
    val testDense = reprocessDataset(old.testDense) // readDataset(qasrlBankPath.resolve("dense").resolve("test.jsonl.gz"))

    implicit val datasetMonoid = Dataset.datasetMonoid(Dataset.printMergeErrors)
    val all = trainExpanded |+| devExpanded |+| testOrig |+| devDense |+| testDense

    val sentenceIdToPart = (
      trainExpanded.sentences.keySet.map(s => SentenceId.fromString(s) -> DatasetPartition.Train) ++
      devExpanded.sentences.keySet.map(s => SentenceId.fromString(s)   -> DatasetPartition.Dev) ++
      testOrig.sentences.keySet.map(s => SentenceId.fromString(s)      -> DatasetPartition.Test)
    ).toMap

    val denseIds = (devDense.sentences.keySet ++ testDense.sentences.keySet)
      .map(SentenceId.fromString)

    val index = old.index // readIndexZipped(qasrlBankPath.resolve("index.json.gz"))

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
          Document(metadata, SortedSet(sentences.map(ConsolidatedSentence.fromSentence).toSeq: _*))
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

  def writeDatasetLines(path: NIOPath, dataset: Dataset) = {
    import io.circe.syntax._
    val printer = io.circe.Printer.noSpaces
    val linesStr = dataset.sentences.iterator.map(_._2).map(_.asJson).map(printer.print).mkString("\n")
    Files.write(path, linesStr.getBytes("UTF-8"))
  }

  def writeDataToDirectory(data: FullQasrlData, path: NIOPath) = {
    for(dirname <- List("", "orig", "expanded", "dense")) {
      val dirPath = path.resolve(dirname)
      if(!Files.exists(dirPath)) {
        Files.createDirectories(dirPath)
      }
    }
    Data.writeIndex(path.resolve("index.json"), data.index)
    def write(suff: String, d: Dataset) = writeDatasetLines(path.resolve(suff), d)
    write("orig/train.jsonl", data.trainOrig)
    write("orig/dev.jsonl", data.devOrig)
    write("orig/test.jsonl", data.testOrig)
    write("expanded/train.jsonl", data.trainExpanded)
    write("expanded/dev.jsonl", data.devExpanded)
    write("dense/dev.jsonl", data.devDense)
    write("dense/test.jsonl", data.testDense)
  }

  def getAudit[F[_]: Monad](dataset: Dataset, log: Log[F]): F[Boolean] = {
    for {
      _ <- log("==========\nRunning audit...")
      // all questions
      allQuestions = Dataset.questionLabels.getAll(dataset).map(_.questionSlots).toSet
      allQuestionTemplates = Dataset.questionLabels.getAll(dataset).map{ q =>
        val slots = q.questionSlots
        slots.wh.toString + slots.subj.toString + q.isPassive + slots.obj.toString + slots.prep.toString + slots.obj2.toString
      }.toSet
      _ <- log(s"Num unique questions: ${allQuestions.size}")
      _ <- log(s"Num unique question templates: ${allQuestionTemplates.size}")
      // identify sentences with no/all empty verbs
      emptySentences = dataset.sentences.values.toList.filter { sentence =>
        sentence.verbEntries.forall(_._2.questionLabels.isEmpty)
      }
      _ <- log(s"-----\nEmpty sentences: ${emptySentences.size}")
      _ <- log("Examples:")
      _ <- emptySentences.take(5).toList.traverse(s => log(Text.render(s.sentenceTokens)))
      // identify empty verbs
      emptyVerbs = (
        for {
          s <- emptySentences
          v <- s.verbEntries.values
          if v.questionLabels.isEmpty
        } yield (s, v)
      )
      _ <- log(s"-----\nEmpty verbs: ${emptyVerbs.size}")
      _ <- log("Examples:")
      _ <- emptyVerbs.take(5).traverse { case (s, v) =>
        log(Text.render(s.sentenceTokens)) >> log(v.verbInflectedForms.stem.toString + "(" + v.verbIndex + ")")
      }
      // check slot vocabularies
      slotVocabularies = getSlotVocabularies(dataset)
      _ <- log("-----\nSlot vocabularies:")
      _ <- slotVocabularies.toList.map { case (k, v) => s"$k (${v.size}): " + v.map(_._1).mkString(", ") }.traverse(log)
      // examples of multi-word prepositions
      _ <- slotVocabularies("prep").filter(_._1.toString.contains(" ")).toList.sortBy(_._2.size).traverse { case (p, questions) =>
        log(s"---- $p (${questions.size}) -----") >> questions.distinct.take(10).map(_.questionString).traverse(log)
      }
      prepsWithDo = slotVocabularies("prep").filter(p => p._1.endsWith("do".lowerCase) || p._1.endsWith("doing".lowerCase)).map(_._1)
      _ <- log(s"Prepositions with do (${prepsWithDo.size}): " + prepsWithDo.mkString(", "))
      emptyOrSpacePreps = slotVocabularies("prep").filter(_._1.trim.isEmpty)
      _ <- log("Spacey preps: " + emptyOrSpacePreps.map(_._1).map(p => s"|$p|").mkString(", "))
      _ <- emptyOrSpacePreps.toList.traverse { case (p, questions) =>
        log(s"---- |$p| (${questions.size}) -----") >> questions.take(10).map(_.questionString).traverse(log)
      }
      verbsWithNot = slotVocabularies("verb").filter(_._1.startsWith("not".lowerCase)).map(_._1)
      _ <- log(s"Verbs with not (${verbsWithNot.size}): " + verbsWithNot.mkString(", "))
      questionsWithSplitDo = Dataset.questionLabels.getAll(dataset).filter(q =>
        q.questionSlots.prep.exists(p => p.endsWith(" to".lowerCase) || p == "to".lowerCase) &&
          q.questionSlots.obj2.exists(o => o.startsWith("do ".lowerCase) || o == "do".lowerCase)
      )
      _ <- log(s"Questions with split do (${questionsWithSplitDo.size}): TODO")
      questionsWithDoButNoToInMisc = Dataset.questionLabels.getAll(dataset).filter(q =>
        q.questionSlots.obj2.exists(o => o.startsWith("do ".lowerCase) || o == "do".lowerCase)
      )
      _ <- log(s"Questions with do but no to in misc (${questionsWithDoButNoToInMisc.size}): TODO")
      questionsWithDoingSomeone = Dataset.questionLabels.getAll(dataset).filter(q =>
        q.questionSlots.obj2.exists(o => o == "doing someone".lowerCase)
      )
      _ <- log(s"Questions with doing someone (${questionsWithDoingSomeone.size}):")
      _ <- questionsWithDoingSomeone.take(10).map(_.questionString).traverse(log)
    } yield {
      List(
        emptySentences.isEmpty,
        emptyVerbs.isEmpty,
        prepsWithDo.isEmpty,
        emptyOrSpacePreps.isEmpty,
        questionsWithSplitDo.isEmpty,
        questionsWithDoingSomeone.isEmpty,
        // verbsWithNot.isEmpty
      ).forall(identity)
    }
  }

  def program(qasrlBankPath: NIOPath, targetPathOpt: Option[NIOPath]) = for {
    fullData <- IO { Data.readFromQasrlBank(qasrlBankPath).get }
    // audit
    _ <- getAudit(fullData.all, Log.console)
    fixedData = reprocessAllData(fullData)
    isResultValid <- getAudit(fixedData.all, Log.console)
    _ <- IO {
      targetPathOpt.foreach { targetPath =>
        if(isResultValid) {
          System.out.println(s"Saving reprocessed data to $targetPath...")
          writeDataToDirectory(fixedData, targetPath)
        }
      }
    }
  } yield ExitCode.Success

  val main: Opts[IO[ExitCode]] = {
    val qasrlBankPath = Opts.option[NIOPath](
      "qasrl-bank", metavar = "path", help = "Path to the original QA-SRL Bank."
    )
    val outPath = Opts.option[NIOPath](
      "out", metavar = "path", help = "Path to the newly reprocessed QA-SRL Bank."
    ).orNone

    (qasrlBankPath, outPath).mapN(program)
  }
}
