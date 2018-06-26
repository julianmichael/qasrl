package qasrl.crowd

case class QASRLValidationResult[SID](
  prompt: QASRLValidationPrompt[SID],
  validatorId: String,
  response: List[QASRLValidationAnswer]
)

case class ValidatorBlocked(badValidatorId: String)

case class QASRLValidationFinished[SID](valPrompt: QASRLValidationPrompt[SID], numValid: Int)
