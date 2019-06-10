package qasrl.crowd

import io.circe.generic.JsonCodec

@JsonCodec case class QASRLValidationWorkerInfoSummary(
  proportionInvalid: Double,
  numAssignmentsCompleted: Int,
  agreement: Double
)
