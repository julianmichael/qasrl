package qasrl.crowd

import qasrl.crowd.util.implicits._
import qasrl.crowd.dollarsToCents

import spacro._
import spacro.tasks._
import spacro.util._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

import akka.actor.{Actor, ActorRef}

import com.amazonaws.services.mturk.model.AssignmentStatus
import com.amazonaws.services.mturk.model.HITStatus
import com.amazonaws.services.mturk.model.SendBonusRequest
import com.amazonaws.services.mturk.model.NotifyWorkersRequest
import com.amazonaws.services.mturk.model.AssociateQualificationWithWorkerRequest
import com.amazonaws.services.mturk.model.DisassociateQualificationFromWorkerRequest

import com.typesafe.scalalogging.StrictLogging

import io.circe.{Encoder, Decoder}
import io.circe.syntax._

class QASRLGenerationAccuracyManager[SID: Encoder : Decoder](genDisqualificationTypeId: String)(
  implicit annotationDataService: AnnotationDataService,
  config: TaskConfig,
  settings: QASRLSettings
) extends Actor
    with StrictLogging {

  import config._

  val workerStatsFilename = "generationWorkerStats"

  var allWorkerStats =
    annotationDataService
      .loadLiveData(workerStatsFilename)
      .map(_.mkString)
      .map(x => io.circe.parser.decode[Map[String, QASRLGenerationWorkerStats]](x).right.get)
      .toOption
      .getOrElse {
        Map.empty[String, QASRLGenerationWorkerStats]
      }

  def christenWorker(workerId: String, numAgreementsToAdd: Int) = {
    allWorkerStats = allWorkerStats.get(workerId).fold(allWorkerStats) { stats =>
      allWorkerStats.updated(workerId, stats.addBonusValids(numAgreementsToAdd))
    }
    assessQualification(workerId)
  }

  private[this] def save = {
    Try(
      annotationDataService.saveLiveData(
        workerStatsFilename,
        (allWorkerStats: Map[String, QASRLGenerationWorkerStats]).asJson.noSpaces
      )
    ).toOptionLogging(logger).foreach(_ => logger.info("Worker stats data saved."))
  }

  private def getAssignmentFromValPrompt(
    valPrompt: QASRLValidationPrompt[SID]
  ): Option[Assignment[List[VerbQA]]] = {
    val assignmentsForHIT = for {
      hit <- hitDataService
        .getHIT[QASRLGenerationPrompt[SID]](valPrompt.sourceHITTypeId, valPrompt.sourceHITId)
        .toOptionLogging(logger)
        .toList
      assignment <- hitDataService
        .getAssignmentsForHIT[List[VerbQA]](valPrompt.sourceHITTypeId, valPrompt.sourceHITId)
        .get
    } yield assignment
    assignmentsForHIT.find(_.assignmentId == valPrompt.sourceAssignmentId)
  }

  def assessQualification(workerId: String): Unit = {
    Try {
      allWorkerStats.get(workerId).foreach { stats =>
        val workerIsDisqualified = config.service
          .listAllWorkersWithQualificationType(genDisqualificationTypeId)
          .contains(stats.workerId)

        val workerShouldBeDisqualified = !stats.accuracy.isNaN &&
        stats.accuracy < settings.generationAccuracyBlockingThreshold &&
        (stats.numValidatorJudgments / 2) > settings.generationAccuracyGracePeriod

        if (workerIsDisqualified && !workerShouldBeDisqualified) {
          config.service.disassociateQualificationFromWorker(
            new DisassociateQualificationFromWorkerRequest()
              .withQualificationTypeId(genDisqualificationTypeId)
              .withWorkerId(stats.workerId)
              .withReason("Accuracy dropped too low on the question writing task.")
          )
        } else if (!workerIsDisqualified && workerShouldBeDisqualified) {
          config.service.associateQualificationWithWorker(
            new AssociateQualificationWithWorkerRequest()
              .withQualificationTypeId(genDisqualificationTypeId)
              .withWorkerId(stats.workerId)
              .withIntegerValue(1)
              .withSendNotification(true)
          )
        }
      }
    }
  }

  override def receive = {
    case SaveData => save
    case ChristenWorker(workerId, numAgreementsToAdd) =>
      christenWorker(workerId, numAgreementsToAdd)
    case ValidatorBlocked(badValidatorId) =>
      allWorkerStats = allWorkerStats.map {
        case (wid, stats) => wid -> stats.removeJudgmentsByWorker(badValidatorId)
      }
      allWorkerStats.keys.foreach(assessQualification)
    case vr: QASRLValidationResult[SID] =>
      vr match {
        case QASRLValidationResult(valPrompt, valWorker, valResponse) =>
          getAssignmentFromValPrompt(valPrompt).foreach { assignment =>
            val accuracyJudgments =
              valResponse.map(r => AccuracyJudgment(valWorker, r.isAnswer)).toVector

            allWorkerStats = allWorkerStats.updated(
              assignment.workerId,
              allWorkerStats
                .get(assignment.workerId)
                .getOrElse(QASRLGenerationWorkerStats.empty(assignment.workerId))
                .addAccuracyJudgments(accuracyJudgments)
            )

            assessQualification(assignment.workerId)
          }
      }
    case vr: QASRLValidationFinished[SID] =>
      vr match {
        case QASRLValidationFinished(valPrompt, numQAsValid) =>
          getAssignmentFromValPrompt(valPrompt).foreach { assignment =>
            // award bonuses
            val numQAsProvided = assignment.response.size
            val bonusAwarded = settings.generationBonus(numQAsValid)
            val bonusCents = dollarsToCents(bonusAwarded)
            if (bonusAwarded > 0.0) {
              Try(
                service.sendBonus(
                  new SendBonusRequest()
                    .withWorkerId(assignment.workerId)
                    .withBonusAmount(f"$bonusAwarded%.2f")
                    .withAssignmentId(assignment.assignmentId)
                    .withReason(
                      s"""$numQAsValid out of $numQAsProvided question-answer pairs were judged to be valid, for a bonus of ${bonusCents}c."""
                    )
                )
              ).toOptionLogging(logger)
                .ifEmpty(
                  logger
                    .error(s"Failed to grant bonus of $bonusCents to worker ${assignment.workerId}")
                )
            }

            allWorkerStats = allWorkerStats.updated(
              assignment.workerId,
              allWorkerStats
                .get(assignment.workerId)
                .getOrElse(QASRLGenerationWorkerStats.empty(assignment.workerId))
                .registerValidationFinished(settings.generationReward + bonusAwarded)
            )
          }
      }
  }
}
