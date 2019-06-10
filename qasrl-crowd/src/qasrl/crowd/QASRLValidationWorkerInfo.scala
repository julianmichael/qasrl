package qasrl.crowd

// import jjm.implicits._

import cats.implicits._

import io.circe.generic.JsonCodec

@JsonCodec case class QASRLValidationResponseComparison(
  thisResponse: QASRLValidationAnswer,
  otherResponses: List[(String, QASRLValidationAnswer)]
) {

  def isAgreement =
    if (otherResponses.size == 1) {
      otherResponses.head._2.agreesWith(thisResponse)
    } else
      (
        // size of set of agreeing answers...
        otherResponses.map(_._2.agreesWith(thisResponse)).filter(identity).size + 1.0
      ) >= ((otherResponses.size + 1) / 2.0) // is a majority (or tie)
}

/** Data structure to keep track of a single worker's stats on the validation task. */
@JsonCodec case class QASRLValidationWorkerInfo(
  workerId: String,
  numAssignmentsCompleted: Int,
  numAnswerSpans: Int,
  numInvalids: Int,
  comparisons: List[QASRLValidationResponseComparison],
  numBonusAgreements: Int,
  timeSpent: Long,
  earnings: Double
) {

  def summary =
    QASRLValidationWorkerInfoSummary(proportionInvalid, numAssignmentsCompleted, agreement)

  def hardAgreement = {
    (List.fill(numBonusAgreements)(true) ++ comparisons.map(_.isAgreement)).proportion(identity)
  }

  def agreement = {
    val spanAgreements = for {
      cmp        <- comparisons
      thisAnswer <- cmp.thisResponse.getAnswer
      otherAnswers = cmp.otherResponses.flatMap(_._2.getAnswer)
      if otherAnswers.nonEmpty
    } yield {
      val agreements = otherAnswers.map(
        thatAnswer =>
          thisAnswer.spans.exists(
            span1 =>
              thatAnswer.spans.exists(
                span2 =>
                  (span1.begin to span1.end).toSet
                    .intersect((span2.begin to span2.end).toSet)
                    .nonEmpty
            )
        )
      )

      if (agreements.size == 1) agreements.head
      else (agreements.filter(identity).size + 1.0) >= ((agreements.size + 1) / 2.0)
    }
    (List.fill(numBonusAgreements)(true) ++ spanAgreements).proportion(identity)
  }

  // def averageNumberOfSpans = comparisons.flatMap(_.thisResponse.getAnswer).map(_.spans.size).meanOpt.getOrElse(-1.0)

  def isLikelySpamming =
    comparisons
      .take(15)
      .filter(c => c.thisResponse.isInvalid && !c.isAgreement)
      .size > 11

  def wasEverLikelySpamming =
    comparisons
      .sliding(15)
      .exists(group => group.filter(c => c.thisResponse.isInvalid && !c.isAgreement).size > 11)

  def proportionInvalid = numInvalids.toDouble / (numAnswerSpans + numInvalids)

  def addBonusAgreements(n: Int) = this.copy(
    numBonusAgreements = this.numBonusAgreements + n
  )

  def addAssignment(response: List[QASRLValidationAnswer], timeTaken: Long, totalReward: Double) =
    this.copy(
      numAssignmentsCompleted = this.numAssignmentsCompleted + 1,
      numAnswerSpans = this.numAnswerSpans + response.filter(_.isAnswer).size,
      numInvalids = this.numInvalids + response.filter(_.isInvalid).size,
      timeSpent = this.timeSpent + timeTaken,
      earnings = this.earnings + totalReward
    )

  def addComparisons(newComparisons: List[QASRLValidationResponseComparison]) = this.copy(
    comparisons = newComparisons ++ this.comparisons
  )

  // e.g. if a worker is blocked
  def removeComparisonsWithWorker(otherWorkerId: String) = this.copy(
    comparisons = this.comparisons
      .map(c => c.copy(otherResponses = c.otherResponses.filter(_._1 != otherWorkerId)))
  )
}

object QASRLValidationWorkerInfo {
  def empty(workerId: String) = QASRLValidationWorkerInfo(workerId, 0, 0, 0, Nil, 0, 0L, 0.0)
}
