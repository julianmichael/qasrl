package qasrl.crowd

@JsonCodec case class QASRLValidationPrompt[SID](
  genPrompt: QASRLGenerationPrompt[SID],
  sourceHITTypeId: String,
  sourceHITId: String,
  sourceAssignmentId: String,
  qaPairs: List[VerbQA]
) {
  def id = genPrompt.id
}
