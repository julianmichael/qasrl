package qasrl.crowd

import qasrl.crowd.util.implicits._

import cats.implicits._

/** Data structure to keep track of a single worker's stats on the evaluation/validation task. */
case class QASRLEvaluationWorkerInfo(
  workerId: String,
  numAssignmentsCompleted: Int,
  numAnswerSpans: Int,
  numInvalids: Int,
  comparisons: List[Boolean],
  numBonusAgreements: Int,
  timeSpent: Long,
  earnings: Double) {

  def summary = QASRLValidationWorkerInfoSummary(proportionInvalid, numAssignmentsCompleted, agreement)

  def agreement = (List.fill(numBonusAgreements)(true) ++ comparisons).proportion(identity)

  def proportionInvalid = numInvalids.toDouble / (numAnswerSpans + numInvalids)

  def addBonusAgreements(n: Int) = this.copy(
    numBonusAgreements = this.numBonusAgreements + n
  )

  def addAssignment(response: List[QASRLValidationAnswer], timeTaken: Long, totalReward: Double) = this.copy(
    numAssignmentsCompleted = this.numAssignmentsCompleted + 1,
    numAnswerSpans = this.numAnswerSpans + response.filter(_.isAnswer).size,
    numInvalids = this.numInvalids + response.filter(_.isInvalid).size,
    timeSpent = this.timeSpent + timeTaken,
    earnings = this.earnings + totalReward)

  def addComparisons(newComparisons: List[Boolean]) = this.copy(
    comparisons = newComparisons ++ this.comparisons
  )

}

object QASRLEvaluationWorkerInfo {
  def empty(workerId: String) = QASRLEvaluationWorkerInfo(workerId, 0, 0, 0, Nil, 0, 0L, 0.0)
}
