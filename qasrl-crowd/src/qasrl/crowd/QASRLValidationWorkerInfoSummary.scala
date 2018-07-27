package qasrl.crowd

case class QASRLValidationWorkerInfoSummary(
  proportionInvalid: Double,
  numAssignmentsCompleted: Int,
  agreement: Double
)
object QASRLValidationWorkerInfoSummary {
  import upickle.default._
  implicit val reader = macroR[QASRLValidationWorkerInfoSummary]
  implicit val writer = macroW[QASRLValidationWorkerInfoSummary]
}
