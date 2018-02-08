package qasrl.crowd

case class QASRLEvaluationPrompt[SID](
  id: SID,
  sourceId: String,
  qaPairs: List[VerbQA])
