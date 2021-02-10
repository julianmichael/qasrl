package qasrl.bank

import jjm.LowerCaseString
import jjm.implicits._

import qasrl.data.VerbEntry
import qasrl.data.Sentence

import io.circe.generic.JsonCodec
import scala.collection.immutable.SortedMap

@JsonCodec case class ConsolidatedSentence(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbEntries: SortedMap[Int, VerbEntry],
  nonPredicates: SortedMap[Int, LowerCaseString]
) {
  def toSentence: Sentence = Sentence(sentenceId, sentenceTokens, verbEntries)
}
object ConsolidatedSentence {
  def fromSentence(sentence: Sentence) = {
    ConsolidatedSentence(
      sentence.sentenceId,
      sentence.sentenceTokens,
      sentence.verbEntries,
      SortedMap()
    )
  }
}

