package qasrl.crowd

case class QASRLValidationPrompt[SID](
  genPrompt: QASRLGenerationPrompt[SID],
  sourceHITTypeId: String,
  sourceHITId: String,
  sourceAssignmentId: String,
  qaPairs: List[VerbQA]
) {
  def id = genPrompt.id
}
object QASRLValidationPrompt {
  import upickle.default._
  implicit def reader[SID: Reader] = macroR[QASRLValidationPrompt[SID]]
  implicit def writer[SID: Writer] = macroW[QASRLValidationPrompt[SID]]
}
