package qasrl

import nlpdata.util.LowerCaseStrings._

sealed trait Tense
case class Modal(modalVerb: LowerCaseString) extends Tense
case object PresentTense extends Tense
case object PastTense extends Tense
