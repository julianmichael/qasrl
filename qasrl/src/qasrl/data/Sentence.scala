package qasrl.data

import scala.collection.immutable.SortedMap

import monocle.macros.Lenses
import io.circe.generic.JsonCodec

@Lenses @JsonCodec case class Sentence(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbEntries: SortedMap[Int, VerbEntry]
)
object Sentence
