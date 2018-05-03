package qasrl.data

import monocle.macros.Lenses

@Lenses case class Sentence(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbEntries: Map[Int, VerbEntry]
)
