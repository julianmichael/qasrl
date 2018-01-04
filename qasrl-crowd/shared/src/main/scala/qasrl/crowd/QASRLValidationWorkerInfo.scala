package qasrl.crowd

import qasrl.crowd.util.implicits._

import cats.implicits._

case class QASRLValidationWorkerInfoSummary(
  proportionInvalid: Double,
  numAssignmentsCompleted: Int,
  agreement: Double)

case class QASRLValidationResponseComparison(
  thisResponse: QASRLValidationAnswer,
  thatResponse: QASRLValidationAnswer,
  thatWorker: String) {
  def isAgreement = (thisResponse, thatResponse) match {
    case (InvalidQuestion, InvalidQuestion) => true
    case (Answer(spans1), Answer(spans2)) =>
      spans1.exists(span1 =>
        spans2.exists(span2 =>
          (span1.begin to span1.end).toSet.intersect((span2.begin to span2.end).toSet).nonEmpty
        )
      )
    case _ => false
  }
}

/** Data structure to keep track of a single worker's stats on the validation task. */
case class QASRLValidationWorkerInfo(
  workerId: String,
  numAssignmentsCompleted: Int,
  numAnswerSpans: Int,
  numInvalids: Int,
  comparisons: List[QASRLValidationResponseComparison],
  numBonusAgreements: Int,
  timeSpent: Long,
  earnings: Double) {

  def summary = QASRLValidationWorkerInfoSummary(proportionInvalid, numAssignmentsCompleted, agreement)

  def agreement = {
    val spanAgreements = for {
      cmp <- comparisons
      a1 <- cmp.thisResponse.getAnswer
      a2 <- cmp.thatResponse.getAnswer
    } yield a1.spans.exists(span1 =>
      a2.spans.exists(span2 =>
        (span1.begin to span1.end).toSet.intersect((span2.begin to span2.end).toSet).nonEmpty
      )
    )
    (List.fill(numBonusAgreements)(true) ++ spanAgreements).proportion(identity)
  }

  // def averageNumberOfSpans = comparisons.flatMap(_.thisResponse.getAnswer).map(_.spans.size).meanOpt.getOrElse(-1.0)

  def isLikelySpamming = comparisons.take(15)
    .filter(c => c.thisResponse.isInvalid && c.thatResponse.isAnswer)
    .size > 8

  def wasEverLikelySpamming = comparisons.sliding(15)
    .map(group =>
    group
      .filter(c => c.thisResponse.isInvalid && c.thatResponse.isAnswer)
      .size > 8
  ).exists(identity)

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

  def addComparisons(newComparisons: List[QASRLValidationResponseComparison]) = this.copy(
    comparisons = newComparisons ++ this.comparisons
  )

  // e.g. if a worker is blocked
  def removeComparisonsWithWorker(otherWorkerId: String) = this.copy(
    comparisons = this.comparisons.filterNot(_.thatWorker == otherWorkerId)
  )
}

object QASRLValidationWorkerInfo {
  def empty(workerId: String) = QASRLValidationWorkerInfo(workerId, 0, 0, 0, Nil, 0, 0L, 0.0)
}
