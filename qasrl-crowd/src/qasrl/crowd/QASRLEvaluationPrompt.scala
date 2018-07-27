package qasrl.crowd

case class SourcedQuestion(verbIndex: Int, question: String, sources: Set[String])
object SourcedQuestion {
  import upickle.default._
  implicit val reader = macroR[SourcedQuestion]
  implicit val writer = macroW[SourcedQuestion]
}

case class QASRLEvaluationPrompt[SID](id: SID, sourcedQuestions: List[SourcedQuestion])
object QASRLEvaluationPrompt {
  import upickle.default._
  implicit def reader[SID: Reader] = macroR[QASRLEvaluationPrompt[SID]]
  implicit def writer[SID: Writer] = macroW[QASRLEvaluationPrompt[SID]]
}

