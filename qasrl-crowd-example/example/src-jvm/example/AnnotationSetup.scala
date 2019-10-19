package example

import jjm.ling.Text
import jjm.corenlp.Tokenizer
import jjm.corenlp.PosTagger
import jjm.implicits._

import cats._
import cats.implicits._

import qasrl.crowd._
import qasrl.labeling._

import spacro._
import spacro.tasks._

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import scala.concurrent.duration._
import scala.language.postfixOps

import scala.util.Try

import java.io.StringReader
import java.nio.file.{Files, Path, Paths}

import scala.util.Try
import scala.util.Random

class AnnotationSetup(
  val label: String = "trial",
  frozenGenerationHITTypeId: Option[String] = None,
  frozenValidationHITTypeId: Option[String] = None
)(implicit config: TaskConfig) {

  val resourcePath = java.nio.file.Paths.get("datasets")

  import java.nio.file.{Files, Path, Paths}
  private[this] val liveDataPath = Paths.get(s"data/example/$label/live")
  val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  val staticDataPath = Paths.get(s"data/example/$label/static")

  def saveOutputFile(name: String, contents: String): Try[Unit] = Try {
    val path = staticDataPath.resolve("out").resolve(name)
    val directory = path.getParent
    if (!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    Files.write(path, contents.getBytes())
  }

  def loadOutputFile(name: String): Try[List[String]] = Try {
    val path = staticDataPath.resolve("out").resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  def loadInputFile(name: String): Try[List[String]] = Try {
    val path = staticDataPath.resolve("in").resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  val sentences = Vector(
    "In the second half of the 1980s, Clarence Thomas is being groomed for a position on the Supreme Court, or senses he's being groomed.",
    "He's the head of the EEOC in the Reagan Administration and decides to beef up on his reading in political theory, constitutional law, and American history.",
    "He hires two Straussians, Ken Masugi and John Marini, to his staff on the EEOC.",
    "Their assignment is to give him a reading list, which they do and which he reads, and to serve as tutors and conversation partners in all things intellectual, which also they do."
  )
  val processedSentences = sentences
    .map(Tokenizer.tokenize)
    .map(PosTagger.posTag(_))
    .map(Text.addIndices(_))

  val allIds = (0 until 4).map(SentenceId(_)).toVector
  val trainIds = allIds.slice(0, 2)
  val devIds = allIds.slice(2, 3)
  val testIds = allIds.slice(3, 4)

  def isTrain(sid: SentenceId) = trainIds.contains(sid)
  def isDev(sid: SentenceId) = devIds.contains(sid)
  def isTest(sid: SentenceId) = testIds.contains(sid)

  lazy val Wiktionary = new WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  val getSentenceTokens = (id: SentenceId) => processedSentences(id.index)

  implicit lazy val inflections = {
    val tokens = for {
      id   <- allIds.iterator
      word <- getSentenceTokens(id)
    } yield word.token
    Wiktionary.getInflectionsForTokens(tokens)
  }

  def numGenerationAssignmentsForPrompt(p: QASRLGenerationPrompt[SentenceId]) = 1

  lazy val experiment = new QASRLAnnotationPipeline(
    allIds,
    getSentenceTokens,
    numGenerationAssignmentsForPrompt,
    liveAnnotationDataService,
    frozenGenerationHITTypeId = frozenGenerationHITTypeId,
    frozenValidationHITTypeId = frozenValidationHITTypeId,
    generationAccuracyDisqualTypeLabel = None,
    generationCoverageDisqualTypeLabel = None,
    validationAgreementDisqualTypeLabel = None
  )

  // def saveAnnotationData[A](
  //   filename: String,
  //   ids: Vector[SentenceId],
  //   genInfos: List[HITInfo[QASRLGenerationPrompt[SentenceId], List[VerbQA]]],
  //   valInfos: List[HITInfo[QASRLValidationPrompt[SentenceId], List[QASRLValidationAnswer]]],
  //   labelMapper: QuestionLabelMapper[String, A],
  //   labelRenderer: A => String
  // ) = {
  //   saveOutputFile(
  //     s"$filename.tsv",
  //     DataIO.makeQAPairTSV(
  //       ids.toList,
  //       SentenceId.toString,
  //       genInfos,
  //       valInfos,
  //       labelMapper,
  //       labelRenderer
  //     )
  //   )
  // }

  // def saveAnnotationDataReadable(
  //   filename: String,
  //   ids: Vector[SentenceId],
  //   genInfos: List[HITInfo[QASRLGenerationPrompt[SentenceId], List[VerbQA]]],
  //   valInfos: List[HITInfo[QASRLValidationPrompt[SentenceId], List[QASRLValidationAnswer]]]
  // ) = {
  //   saveOutputFile(
  //     s"$filename.tsv",
  //     DataIO.makeReadableQAPairTSV(
  //       ids.toList,
  //       SentenceId.toString,
  //       identity,
  //       genInfos,
  //       valInfos,
  //       (id: SentenceId, qa: VerbQA, responses: List[QASRLValidationAnswer]) =>
  //         responses.forall(_.isAnswer)
  //     )
  //   )
  // }
}
