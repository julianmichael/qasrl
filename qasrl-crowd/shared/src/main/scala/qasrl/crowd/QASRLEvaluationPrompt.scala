package qasrl.crowd

case class QASRLEvaluationPrompt[SID](
  id: SID,
  sourcedQuestions: List[SourcedQuestion])

case class SourcedQuestion(
  verbIndex: Int,
  question: String,
  sources: Set[String])

