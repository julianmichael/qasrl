package qasrl.crowd

import qasrl.crowd.util.implicits._

import cats.implicits._

case class AccuracyJudgment(
  validatorId: String,
  isValid: Boolean
)

case class QASRLGenerationWorkerStats(
  workerId: String,
  numValidatorJudgments: Int,
  numAssignmentsCompleted: Int,
  accuracyJudgments: Vector[AccuracyJudgment],
  numBonusValids: Int,
  earnings: Double) {

  def numQAPairsWritten: Int = accuracyJudgments.size
  def numQAPairsValid: Int = accuracyJudgments.filter(_.isValid).size

  def accuracy = (Vector.fill(numBonusValids)(true) ++ accuracyJudgments.map(_.isValid)).proportion(identity)

  def addBonusValids(n: Int) = this.copy(
    numBonusValids = this.numBonusValids + n
  )

  def removeJudgmentsByWorker(badWorkerId: String) = this.copy(
    accuracyJudgments = this.accuracyJudgments.filter(_.validatorId != badWorkerId)
  )

  def addAccuracyJudgments(
    judgments: Vector[AccuracyJudgment]
  ) = this.copy(
    numValidatorJudgments = this.numValidatorJudgments + 1,
    accuracyJudgments = judgments ++ this.accuracyJudgments
  )

  def registerValidationFinished(
    totalReward: Double
  ) = this.copy(
    numAssignmentsCompleted = this.numAssignmentsCompleted + 1,
    earnings = this.earnings + totalReward
  )
}
object QASRLGenerationWorkerStats {
  def empty(workerId: String) = QASRLGenerationWorkerStats(workerId, 0, 0, Vector.empty[AccuracyJudgment], 0, 0.0)
}
