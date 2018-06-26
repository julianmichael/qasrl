package qasrl.crowd

import qasrl.crowd.util.dollarsToCents

import spacro._
import spacro.tasks._
import spacro.util._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.services.mturk.model.AssociateQualificationWithWorkerRequest
import com.amazonaws.services.mturk.model.SendBonusRequest
import com.amazonaws.services.mturk.model.NotifyWorkersRequest
import com.amazonaws.services.mturk.model.CreateWorkerBlockRequest
import com.amazonaws.services.mturk.model.ListWorkersWithQualificationTypeRequest
import com.amazonaws.services.mturk.model.DisassociateQualificationFromWorkerRequest

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

class QASRLEvaluationHITManager[SID : Reader : Writer](
  valDisqualificationTypeId: String,
  helper: HITManager.Helper[QASRLEvaluationPrompt[SID], List[QASRLValidationAnswer]],
  numAssignmentsForPrompt: QASRLEvaluationPrompt[SID] => Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[QASRLEvaluationPrompt[SID]])(
  implicit annotationDataService: AnnotationDataService,
  settings: QASRLEvaluationSettings
) extends NumAssignmentsHITManager[QASRLEvaluationPrompt[SID], List[QASRLValidationAnswer]](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, _promptSource, false) {

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case SaveData => save
    case Pring => println("Evaluation manager pringed.")
    case ChristenWorker(workerId, numAgreementsToAdd) => christenWorker(workerId, numAgreementsToAdd)
  }

  override def promptFinished(prompt: QASRLEvaluationPrompt[SID]): Unit = {
    val assignments = helper.allCurrentHITInfos(prompt).flatMap(_.assignments)
    val numValid = QASRLValidationAnswer.numValidQuestions(assignments.map(_.response))
    evaluationStats = assignments.map(a => a.response.map(ans => a.workerId -> ans)).transpose :: evaluationStats
  }

  def christenWorker(workerId: String, numAgreementsToAdd: Int) = {
    allWorkerInfo = allWorkerInfo.get(workerId).fold(allWorkerInfo) { info =>
      val newInfo = info.addBonusAgreements(numAgreementsToAdd)
      allWorkerInfo.updated(workerId, newInfo)
    }
    assessQualification(workerId)
  }

  var evaluationStats = {
    helper.finishedHITInfosByPromptIterator.map { case (_, hitInfos) =>
      hitInfos.flatMap(_.assignments).map(a => a.response.map(ans => a.workerId -> ans)).transpose
    }.toList
  }

  val workerInfoFilename = "evaluationWorkerInfo"

  var allWorkerInfo = {
    annotationDataService.loadLiveData(workerInfoFilename)
      .map(_.mkString)
      .map(read[Map[String, QASRLValidationWorkerInfo]])
      .toOption.getOrElse {
      Map.empty[String, QASRLValidationWorkerInfo]
    }
  }

  val feedbackFilename = "valFeedback"

  var feedbacks =
    annotationDataService.loadLiveData(feedbackFilename)
      .map(_.mkString)
      .map(read[List[Assignment[List[QASRLValidationAnswer]]]])
      .toOption
      .getOrElse(List.empty[Assignment[List[QASRLValidationAnswer]]])

  val blockedValidatorsFilename = "blockedValidators"

  var blockedValidators =
    annotationDataService.loadLiveData(blockedValidatorsFilename)
      .map(_.mkString)
      .map(read[Set[String]])
      .toOption
      .getOrElse(Set.empty[String])

  private[this] def save = {
    annotationDataService.saveLiveData(
      workerInfoFilename,
      write[Map[String, QASRLValidationWorkerInfo]](allWorkerInfo))
    annotationDataService.saveLiveData(
      feedbackFilename,
      write[List[Assignment[List[QASRLValidationAnswer]]]](feedbacks))
    annotationDataService.saveLiveData(
      blockedValidatorsFilename,
      write[Set[String]](blockedValidators))
    logger.info("Evaluation data saved.")
  }

  import scala.collection.JavaConverters._

  def assessQualification(workerId: String): Unit =
    allWorkerInfo.get(workerId).foreach { worker =>
      if(worker.isLikelySpamming) blockWorker(worker.workerId)
      else Try {
        val workerIsDisqualified = helper.config.service
          .listAllWorkersWithQualificationType(valDisqualificationTypeId)
          .contains(worker.workerId)

        val workerShouldBeDisqualified = !worker.agreement.isNaN &&
          worker.agreement < settings.validationAgreementBlockingThreshold &&
          worker.numAssignmentsCompleted > settings.validationAgreementGracePeriod

        if(workerIsDisqualified && !workerShouldBeDisqualified) {
          helper.config.service.disassociateQualificationFromWorker(
            new DisassociateQualificationFromWorkerRequest()
              .withQualificationTypeId(valDisqualificationTypeId)
              .withWorkerId(worker.workerId)
              .withReason("Agreement went back high enough on the question answering task."))
        } else if(!workerIsDisqualified && workerShouldBeDisqualified) {
          helper.config.service.associateQualificationWithWorker(
            new AssociateQualificationWithWorkerRequest()
              .withQualificationTypeId(valDisqualificationTypeId)
              .withWorkerId(worker.workerId)
              .withIntegerValue(1)
              .withSendNotification(true))
        }
      }
    }

  def blockWorker(workerId: String) = {
    if(!blockedValidators.contains(workerId)) {
      helper.config.service.createWorkerBlock(
        new CreateWorkerBlockRequest()
          .withWorkerId(workerId)
          .withReason("You have been blocked because you were detected spamming the question answering task. If you believe this was in error, please contact the requester.")
      )
      // remove all comparisons with a blocked validator to prevent them from ruining people's stats
      blockedValidators = blockedValidators + workerId
      var workersToReassess = Set.empty[String]
      allWorkerInfo = allWorkerInfo.map {
        case (wid, info) =>
          if(wid == workerId) wid -> info else {
            val newInfo = info.removeComparisonsWithWorker(workerId)
            // update qualifications for the worker if any comparisons were removed
            if(info != newInfo) workersToReassess = workersToReassess + workerId
            wid -> newInfo
          }
      }
      workersToReassess.foreach(assessQualification)
    }
  }

  override def reviewAssignment(hit: HIT[QASRLEvaluationPrompt[SID]], assignment: Assignment[List[QASRLValidationAnswer]]): Unit = {
    helper.evaluateAssignment(hit, helper.startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      feedbacks = assignment :: feedbacks
      logger.info(s"Feedback: ${assignment.feedback}")
    }

    import assignment.workerId

    // grant bonus as appropriate
    val numQuestions = hit.prompt.sourcedQuestions.size
    val totalBonus = settings.validationBonus(numQuestions)
    if(totalBonus > 0.0) {
      helper.config.service.sendBonus(
        new SendBonusRequest()
          .withWorkerId(workerId)
          .withBonusAmount(f"$totalBonus%.2f")
          .withAssignmentId(assignment.assignmentId)
          .withReason(s"Bonus of ${dollarsToCents(totalBonus)}c awarded for validating $numQuestions questions.")
      )
    }

    val newWorkerInfo = allWorkerInfo
      .get(workerId)
      .getOrElse(QASRLValidationWorkerInfo.empty(workerId))
      .addAssignment(assignment.response,
                     assignment.submitTime - assignment.acceptTime,
                     helper.taskSpec.hitType.reward + totalBonus)
    allWorkerInfo = allWorkerInfo.updated(workerId, newWorkerInfo)

    val finishedAssignments = helper.allCurrentHITInfos(hit.prompt).flatMap(_.assignments).toList
    if(finishedAssignments.size == numAssignmentsForPrompt(hit.prompt)) {
      allWorkerInfo = QASRLEvaluationHITManager.updateStatsWithAllComparisons(
        finishedAssignments, blockedValidators)(allWorkerInfo)
    }
    finishedAssignments.map(_.workerId).foreach(assessQualification)
  }
}

object QASRLEvaluationHITManager {
  def updateStatsWithComparison(
    target: Assignment[List[QASRLValidationAnswer]],
    references: Set[Assignment[List[QASRLValidationAnswer]]],
    blockedWorkerIds: Set[String]
  ) = (stats: Map[String, QASRLValidationWorkerInfo]) => {
    val comparisons = target.response.zip(references.toList.map(a => a.response.map(a.workerId -> _)).transpose).map {
      case (givenAnswer, refPairs) =>
        QASRLValidationResponseComparison(
          givenAnswer,
          refPairs.filter(p => !blockedWorkerIds.contains(p._1))
        )
    }
    val targetInfo = stats(target.workerId)
    val newTargetInfo = targetInfo.addComparisons(comparisons)
    stats.updated(target.workerId, newTargetInfo)
  }

  def updateStatsWithAllComparisons(
    assignments: List[Assignment[List[QASRLValidationAnswer]]],
    blockedWorkerIds: Set[String]
  ) = {
    val assignmentSet = assignments.toSet
    assignments
      .map(a => updateStatsWithComparison(a, assignmentSet - a, blockedWorkerIds))
      .foldLeft(identity[Map[String, QASRLValidationWorkerInfo]](_))(_ compose _)
  }
}
