package qasrl.data

import scala.collection.immutable.SortedMap

import monocle.macros.Lenses

@Lenses case class Sentence(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbEntries: SortedMap[Int, VerbEntry]
)
