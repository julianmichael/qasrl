package qasrl.bank.qanom

// import scala.collection.immutable.SortedSet
// import scala.collection.immutable.SortedMap
// import scala.util.Try

import java.nio.file.Path
// import java.nio.file.Files

import cats.Id
import cats.implicits._

// import qasrl.data.Dataset
// import qasrl.data.Sentence

import jjm.io.FileUtil

import fs2.Stream

import qasrl.bank.DocumentId
import cats.effect.IO
import cats.effect.ContextShift
import qasrl.bank.SentenceId

case class Data(
  qaNomSentencesById: Map[DocumentId, Map[SentenceId, QANomSentence]]
)
object Data {
  def readReformattedQANomData(
    path: Path)(
    implicit cs: ContextShift[IO]
  ): IO[Data] = {
    Stream[IO, String]("train", "dev", "test")
      .map(x => path.resolve(s"$x.jsonl"))
      .flatMap(FileUtil.readJsonLines[QANomSentence](_))
      .map { sent =>
        val sentId = SentenceId.fromString(sent.sentenceId)
        val docId = sentId.documentId
        Map(docId -> Vector(sentId -> sent))
      }.compile.foldMonoid
      .map(_.transform((_, y) => y.toMap))
      .map(Data(_))
  }
}
