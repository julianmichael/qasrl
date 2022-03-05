package qasrl.bank.qanom

import jjm.LowerCaseString
import jjm.implicits._

import qasrl.data.VerbEntry

import io.circe.generic.JsonCodec

@JsonCodec case class QANomSentence(
  sentenceId: String,
  sentenceTokens: Vector[String],
  verbEntries: Map[Int, VerbEntry],
  nonPredicates: Map[Int, LowerCaseString]
)

