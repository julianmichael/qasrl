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

class QASRLValidationHITManager[SID : Reader : Writer](
  helper: HITManager.Helper[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]],
  valDisqualificationTypeId: String,
  accuracyStatsActor: ActorRef,
  numAssignmentsForPrompt: QASRLValidationPrompt[SID] => Int,
  initNumHITsToKeepActive: Int)(
  implicit annotationDataService: AnnotationDataService,
  settings: QASRLSettings
) extends NumAssignmentsHITManager[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, List.empty[QASRLValidationPrompt[SID]].iterator) {

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case SaveData => save
    case Pring => println("Validation manager pringed.")
    case ChristenWorker(workerId, numAgreementsToAdd) => christenWorker(workerId, numAgreementsToAdd)
  }

  override def promptFinished(prompt: QASRLValidationPrompt[SID]): Unit = {
    val assignments = promptToAssignments(prompt)
    val numValid = QASRLValidationAnswer.numValidQuestions(assignments.map(_.response))
    accuracyStatsActor ! QASRLValidationFinished(prompt, numValid)
    promptToAssignments = promptToAssignments - prompt
  }

  override def addPrompt(prompt: QASRLValidationPrompt[SID]): Unit = {
    if(prompt.qaPairs.nonEmpty) {
      super.addPrompt(prompt)
      allPrompts = prompt :: allPrompts
      refreshHITs
    } else {
      logger.warn(s"Validation prompt contains response with no QA pairs: $prompt")
    }
  }

  def christenWorker(workerId: String, numAgreementsToAdd: Int) = {
    allWorkerInfo = allWorkerInfo.get(workerId).fold(allWorkerInfo) { info =>
      val newInfo = info.addBonusAgreements(numAgreementsToAdd)
      assessQualification(newInfo)
      allWorkerInfo.updated(workerId, newInfo)
    }
  }

  val validationPromptsFilename = "validationPrompts"

  private[this] var allPrompts = {
    val prompts = annotationDataService.loadLiveData(validationPromptsFilename)
      .toOption
      .fold(List.empty[QASRLValidationPrompt[SID]])(lines => read[List[QASRLValidationPrompt[SID]]](lines.mkString))
    prompts.reverse.foreach(p =>
      if(p.qaPairs.nonEmpty) super.addPrompt(p) else ()
    ) // add them back while loading
    prompts
  }

  val workerInfoFilename = "validationWorkerInfo"

  var allWorkerInfo = {
    annotationDataService.loadLiveData(workerInfoFilename)
      .map(_.mkString)
      .map(read[Map[String, QASRLValidationWorkerInfo]])
      .toOption.getOrElse {
      Map.empty[String, QASRLValidationWorkerInfo]
    }
  }

  val promptToAssignmentsFilename = "promptToAssignments"

  private[this] var promptToAssignments = {
    annotationDataService.loadLiveData(promptToAssignmentsFilename)
      .map(_.mkString)
      .map(read[Map[QASRLValidationPrompt[SID], List[Assignment[List[QASRLValidationAnswer]]]]])
      .toOption.getOrElse {
      Map.empty[QASRLValidationPrompt[SID], List[Assignment[List[QASRLValidationAnswer]]]]
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
      promptToAssignmentsFilename,
      write[Map[QASRLValidationPrompt[SID], List[Assignment[List[QASRLValidationAnswer]]]]](promptToAssignments))
    annotationDataService.saveLiveData(
      validationPromptsFilename,
      write[List[QASRLValidationPrompt[SID]]](allPrompts))
    annotationDataService.saveLiveData(
      feedbackFilename,
      write[List[Assignment[List[QASRLValidationAnswer]]]](feedbacks))
    annotationDataService.saveLiveData(
      blockedValidatorsFilename,
      write[Set[String]](blockedValidators))
    logger.info("Validation data saved.")
  }

  import scala.collection.JavaConverters._

  def assessQualification(worker: QASRLValidationWorkerInfo): Unit = {
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
            .withReason("Agreement dropped too low on the question answering task."))
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
      accuracyStatsActor ! ValidatorBlocked(workerId)
      // remove all comparisons with a blocked validator to prevent them from ruining people's stats
      blockedValidators = blockedValidators + workerId
      allWorkerInfo = allWorkerInfo.map {
        case (wid, info) =>
          if(wid == workerId) wid -> info else {
            val newInfo = info.removeComparisonsWithWorker(workerId)
            // update qualifications for the worker if any comparisons were removed
            if(info != newInfo) assessQualification(newInfo)
            wid -> newInfo
          }
      }
    }
  }

  // override for more interesting review policy
  override def reviewAssignment(hit: HIT[QASRLValidationPrompt[SID]], assignment: Assignment[List[QASRLValidationAnswer]]): Unit = {
    helper.evaluateAssignment(hit, helper.startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      feedbacks = assignment :: feedbacks
      logger.info(s"Feedback: ${assignment.feedback}")
    }

    import assignment.workerId

    // grant bonus as appropriate
    val numQuestions = hit.prompt.qaPairs.size
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

    var newWorkerInfo = allWorkerInfo
      .get(workerId)
      .getOrElse(QASRLValidationWorkerInfo.empty(workerId))
      .addAssignment(assignment.response,
                     assignment.submitTime - assignment.acceptTime,
                     helper.taskSpec.hitType.reward + totalBonus)

    // spam detection
    if(newWorkerInfo.proportionInvalid > 0.7 && newWorkerInfo.numAssignmentsCompleted >= 8) {
      blockWorker(newWorkerInfo.workerId)
    }

    if(!blockedValidators.contains(assignment.workerId)) {
      accuracyStatsActor ! QASRLValidationResult(hit.prompt, assignment.workerId, assignment.response)
    }

    // TODO NEW VERSION
    // do comparisons with other workers
  //   promptToAssignments.get(hit.prompt).getOrElse(Nil).foreach { otherAssignment =>

  //     val otherWorkerId = otherAssignment.workerId

  //     // update this worker's stats if the other isn't blocked
  //     if(!blockedValidators.contains(otherWorkerId)) {
  //       val comparisons = (
  //         assignment.response,
  //         otherAssignment.response,
  //         List.fill(otherAssignment.response.size)(otherAssignment.workerId))
  //         .zipped
  //         .map(QASRLValidationResponseComparison(_, _, _))
  //       // update current worker with comparison
  //       newWorkerInfo = newWorkerInfo.addComparisons(comparisons)
  //     }

  //     // update the other worker's stats if this one isn't blocked
  //     if(!blockedValidators.contains(assignment.workerId)) {
  //       val reverseComparisons = (
  //         otherAssignment.response,
  //         assignment.response,
  //         List.fill(assignment.response.size)(assignment.workerId))
  //         .zipped
  //         .map(QASRLValidationResponseComparison(_, _, _))
  //       // update the other one and put back in data structure (blocking if necessary)
  //       val otherWorkerInfo = allWorkerInfo(otherWorkerId).addComparisons(reverseComparisons)
  //       assessQualification(otherWorkerInfo)
  //       allWorkerInfo = allWorkerInfo.updated(otherWorkerId, otherWorkerInfo)
  //     }
  //   }
  //   // now blocking the current worker if necessary before adding everything in
  //   assessQualification(newWorkerInfo)
  //   allWorkerInfo = allWorkerInfo.updated(workerId, newWorkerInfo)
  //   promptToAssignments = promptToAssignments.updated(
  //     hit.prompt,
  //     assignment :: promptToAssignments.get(hit.prompt).getOrElse(Nil))
  }
}
