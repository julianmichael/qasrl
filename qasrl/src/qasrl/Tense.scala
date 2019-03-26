package qasrl

import nlpdata.util.LowerCaseStrings._
import qasrl.util.implicits._

import io.circe.generic.JsonCodec

@JsonCodec sealed trait Tense
@JsonCodec case class Modal(modalVerb: LowerCaseString) extends Tense
case object PresentTense extends Tense
case object PastTense extends Tense
