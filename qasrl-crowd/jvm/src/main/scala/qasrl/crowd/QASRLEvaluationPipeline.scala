package qasrl.crowd

import qasrl.crowd.util.PosTagger
import qasrl.crowd.util.implicits._

import cats.implicits._

import akka.actor._
import akka.stream.scaladsl.{Flow, Source}

import com.amazonaws.services.mturk.model.QualificationRequirement
import com.amazonaws.services.mturk.model.QualificationTypeStatus
import com.amazonaws.services.mturk.model.Locale
import com.amazonaws.services.mturk.model.ListQualificationTypesRequest
import com.amazonaws.services.mturk.model.ListWorkersWithQualificationTypeRequest
import com.amazonaws.services.mturk.model.CreateQualificationTypeRequest
import com.amazonaws.services.mturk.model.AssociateQualificationWithWorkerRequest
import com.amazonaws.services.mturk.model.DisassociateQualificationFromWorkerRequest

import nlpdata.structure._
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.util.Text
import nlpdata.util.PosTags
import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.Inflections

import spacro._
import spacro.tasks._
import spacro.util.Span

import upickle.default._

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.JavaConverters._

import com.typesafe.scalalogging.StrictLogging

class QASRLEvaluationPipeline[SID : Reader : Writer : HasTokens](
  val allPrompts: Vector[QASRLEvaluationPrompt[SID]], // IDs of sentences to annotate
  frozenEvaluationHITTypeId: Option[String] = None,
  validationAgreementDisqualTypeLabel: Option[String] = None)(
  implicit config: TaskConfig,
  annotationDataService: AnnotationDataService,
  settings: QASRLEvaluationSettings,
  inflections: Inflections
) extends StrictLogging {

  import config.hitDataService

  val approvalRateQualificationTypeID = "000000000000000000L0"
  val approvalRateRequirement = new QualificationRequirement()
    .withQualificationTypeId(approvalRateQualificationTypeID)
    .withComparator("GreaterThanOrEqualTo")
    .withIntegerValues(95)
    .withRequiredToPreview(false)

  val localeQualificationTypeID = "00000000000000000071"
  val localeRequirement = new QualificationRequirement()
    .withQualificationTypeId(localeQualificationTypeID)
    .withComparator("NotEqualTo")
    .withLocaleValues(new Locale().withCountry("IN"))
    .withRequiredToPreview(false)

  val valAgrDisqualTypeLabelString = validationAgreementDisqualTypeLabel.fold("")(x => s"[$x] ")
  val valAgrDisqualTypeName = s"${valAgrDisqualTypeLabelString}Question answering agreement disqualification"
  val valAgrDisqualType = config.service.listQualificationTypes(
    new ListQualificationTypesRequest()
      .withQuery(valAgrDisqualTypeName)
      .withMustBeOwnedByCaller(true)
      .withMustBeRequestable(false)
      .withMaxResults(100)
  ).getQualificationTypes.asScala.toList.find(_.getName == valAgrDisqualTypeName).getOrElse {
    System.out.println("Generating validation disqualification type...")
    config.service.createQualificationType(
      new CreateQualificationTypeRequest()
        .withName(valAgrDisqualTypeName)
        .withKeywords("language,english,question answering")
        .withDescription("""Agreement with other annotators on answers and validity judgments
          in our question answering task is too low.""".replaceAll("\\s+", " "))
        .withQualificationTypeStatus(QualificationTypeStatus.Active)
        .withAutoGranted(false)
    ).getQualificationType
  }
  val valAgrDisqualTypeId = valAgrDisqualType.getQualificationTypeId
  val valAgreementRequirement = new QualificationRequirement()
    .withQualificationTypeId(valAgrDisqualTypeId)
    .withComparator("DoesNotExist")
    .withRequiredToPreview(false)

  // NOTE may need to call multiple times to cover all workers... sigh TODO pagination
  def resetAllQualificationValues = {
    def revokeAllWorkerQuals(qualTypeId: String) = {
      val quals = config.service.listWorkersWithQualificationType(
        new ListWorkersWithQualificationTypeRequest()
          .withQualificationTypeId(qualTypeId)
          .withMaxResults(100)
      ).getQualifications.asScala.toList
      quals.foreach(qual =>
        config.service.disassociateQualificationFromWorker(
          new DisassociateQualificationFromWorkerRequest()
            .withQualificationTypeId(qualTypeId)
            .withWorkerId(qual.getWorkerId)
        )
      )
    }
    revokeAllWorkerQuals(valAgrDisqualTypeId)
  }

  lazy val (taskPageHeadLinks, taskPageBodyLinks) = {
    import scalatags.Text.all._
    val headLinks = List(
      link(
        rel := "stylesheet",
        href := "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css",
        attr("integrity") := "sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ",
        attr("crossorigin") := "anonymous"))
    val bodyLinks = List(
      script(
        src := "https://code.jquery.com/jquery-3.1.1.slim.min.js",
        attr("integrity") := "sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n",
        attr("crossorigin") := "anonymous"),
      script(
        src := "https://cdnjs.cloudflare.com/ajax/libs/jquery-cookie/1.4.1/jquery.cookie.min.js",
        attr("integrity") := "sha256-1A78rJEdiWTzco6qdn3igTBv9VupN3Q1ozZNTR4WE/Y=",
        attr("crossorigin") := "anonymous"),
      script(
        src := "https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js",
        attr("integrity") := "sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb",
        attr("crossorigin") := "anonymous"),
      script(
        src := "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/js/bootstrap.min.js",
        attr("integrity") := "sha384-vBWWzlZJ8ea9aCX4pEW3rVHjgjt7zpkNpZk+02D9phzyeVkE+jo0ieGizqPLForn",
        attr("crossorigin") := "anonymous"))
    (headLinks, bodyLinks)
  }

  // validation task definition

  val valHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and several questions about it,
      highlight the part of the sentence that answers each question,
      and mark questions that are invalid or redundant.
      Maintain high agreement with others to stay qualified.
    """.trim,
    reward = settings.validationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, localeRequirement, valAgreementRequirement
    ),
    autoApprovalDelay: Long = 2592000L // 30 days
  )

  lazy val valAjaxService = new Service[QASRLValidationAjaxRequest[SID]] {
    override def processRequest(request: QASRLValidationAjaxRequest[SID]) = request match {
      case QASRLValidationAjaxRequest(workerIdOpt, id) =>
        val workerInfoSummaryOpt = for {
          valManagerP <- Option(valManagerPeek)
          workerId <- workerIdOpt
          info <- valManagerP.allWorkerInfo.get(workerId)
        } yield info.summary

        QASRLValidationAjaxResponse(workerInfoSummaryOpt, id.tokens)
    }
  }

  lazy val sampleValPrompt = allPrompts.head

  lazy val valTaskSpec = TaskSpecification.NoWebsockets[
    QASRLEvaluationPrompt[SID], List[QASRLValidationAnswer], QASRLValidationAjaxRequest[SID]](
    settings.evaluationTaskKey, valHITType, valAjaxService, Vector(sampleValPrompt),
    taskPageHeadElements = taskPageHeadLinks,
    taskPageBodyElements = taskPageBodyLinks,
    frozenHITTypeId = frozenEvaluationHITTypeId)

  import config.actorSystem

  var valManagerPeek: QASRLEvaluationHITManager[SID] = null

  lazy val valHelper = new HITManager.Helper(valTaskSpec)
  lazy val valManager: ActorRef = actorSystem.actorOf(
    Props {
      valManagerPeek = new QASRLEvaluationHITManager(
        valHelper,
        valAgrDisqualTypeId,
        if(config.isProduction) 100 else 3,
        allPrompts.iterator)
      valManagerPeek
    })

  lazy val valActor = actorSystem.actorOf(Props(new TaskManager(valHelper, valManager)))

  lazy val server = new Server(List(valTaskSpec))

  // used to schedule data-saves
  private[this] var schedule: List[Cancellable] = Nil
  def startSaves(interval: FiniteDuration = 5 minutes): Unit = {
    if(schedule.exists(_.isCancelled) || schedule.isEmpty) {
      schedule = List(valManager).map(actor =>
        config.actorSystem.scheduler.schedule(
          2 seconds, interval, actor, SaveData)(
          config.actorSystem.dispatcher, actor)
      )
    }
  }
  def stopSaves = schedule.foreach(_.cancel())

  def setValHITsActive(n: Int) = {
    valManager ! SetNumHITsActive(n)
  }

  import TaskManager.Message._
  def start(interval: FiniteDuration = 30 seconds) = {
    server
    startSaves()
    valActor ! Start(interval, delay = 3 seconds)
  }
  def stop() = {
    valActor ! Stop
    stopSaves
  }
  def delete() = {
    valActor ! Delete
  }
  def expire() = {
    valActor ! Expire
  }
  def update() = {
    server
    valActor ! Update
  }
  def save() = {
    valManager ! SaveData
  }

  // for use while it's running. Ideally instead of having to futz around at the console calling these functions,
  // in the future you could have a nice dashboard UI that will help you examine common sources of issues

  def allInfos = hitDataService.getAllHITInfo[QASRLEvaluationPrompt[SID], List[QASRLValidationAnswer]](valTaskSpec.hitTypeId).get

  def allWorkers = allInfos.flatMap(_.assignments).map(_.workerId).toSet.toList

  def latestInfos(n: Int = 5) = allInfos
    .filter(_.assignments.nonEmpty)
    .sortBy(_.assignments.map(_.submitTime).max)
    .takeRight(n)

  // sorted increasing by submit time
  def infosForWorker(workerId: String) = {
    val scored = for {
      hi <- allInfos
      if hi.assignments.exists(_.workerId == workerId)
      workerAssignment = hi.assignments.find(_.workerId == workerId).get
      nonWorkerAssignments = hi.assignments.filter(_.workerId != workerId)
    } yield (HITInfo(hi.hit, workerAssignment :: nonWorkerAssignments), workerAssignment.submitTime)
    scored.sortBy(_._2).map(_._1)
  }

  def renderValidation(info: HITInfo[QASRLEvaluationPrompt[SID], List[QASRLValidationAnswer]]) = {
    val sentence = info.hit.prompt.id.tokens
    val genWorkerString = info.hit.prompt.sourceId
    Text.render(sentence) + "\n" +
      info.hit.prompt.qaPairs.zip(info.assignments.map(_.response).transpose).map {
        case (VerbQA(verbIndex, question, answers), validationAnswers) =>
          val answerString = answers.map(s => Text.renderSpan(sentence, (s.begin to s.end).toSet)).mkString(" / ")
          val validationRenderings = validationAnswers.map(QASRLValidationAnswer.render(sentence, _, info.hit.prompt.qaPairs))
          val allValidationsString = validationRenderings.toList match {
            case Nil => ""
            case head :: tail => f"$head%20s(${tail.mkString("; ")}%s)"
          }
          f"$genWorkerString%-20s $question%-35s --> $answerString%20s | $allValidationsString"
      }.mkString("\n") + "\n"
  }

  def printLatestInfos(workerId: String, n: Int = 5) =
    infosForWorker(workerId)
      .takeRight(n)
      .map(renderValidation)
      .foreach(println)

  def printWorstInfos(workerId: String, n: Int = 5) =
    infosForWorker(workerId)
      .sortBy(hi => hi.hit.prompt.qaPairs.size.toDouble - (
                hi.assignments.tail.map(
                  a => (hi.assignments.head.response, a.response, List.fill(a.response.size)(a.workerId)).zipped.map(QASRLValidationResponseComparison(_, _, _)).filter(_.isAgreement).size
                ).meanOpt.getOrElse(hi.hit.prompt.qaPairs.size + 1.0))) // if no other answers, put at top
      .takeRight(n)
      .map(renderValidation)
      .foreach(println)

  // case class StatSummary(
  //   workerId: String,
  //   numVerbs: Option[Int],
  //   numQs: Option[Int],
  //   accuracy: Option[Double],
  //   numAs: Option[Int],
  //   numInvalidAnswers: Option[Int],
  //   pctBad: Option[Double],
  //   agreement: Option[Double],
  //   earnings: Double)

  // case class AggregateStatSummary(
  //   numVerbs: Int,
  //   numQs: Int,
  //   numAs: Int,
  //   numInvalidAnswers: Int,
  //   totalCost: Double) {
  //   def combine(worker: StatSummary) = AggregateStatSummary(
  //     numVerbs + worker.numVerbs.getOrElse(0),
  //     numQs + worker.numQs.getOrElse(0),
  //     numAs + worker.numAs.getOrElse(0) + worker.numInvalidAnswers.getOrElse(0),
  //     numInvalidAnswers + worker.numInvalidAnswers.getOrElse(0),
  //     totalCost + worker.earnings
  //   )
  // }
  // object AggregateStatSummary {
  //   def empty = AggregateStatSummary(0, 0, 0, 0, 0.0)
  // }

  // object StatSummary {
  //   def makeFromStatsAndInfo(
  //     stats: Option[QASRLGenerationWorkerStats],
  //     info: Option[QASRLValidationWorkerInfo]
  //   ) = stats.map(_.workerId).orElse(info.map(_.workerId)).map { wid =>
  //     StatSummary(
  //       workerId = wid,
  //       numVerbs = stats.map(_.numAssignmentsCompleted),
  //       numQs = stats.map(_.numQAPairsWritten),
  //       accuracy = stats.map(_.accuracy),
  //       numAs = info.map(i => i.numAnswerSpans + i.numInvalids),
  //       numInvalidAnswers = info.map(_.numInvalids),
  //       pctBad = info.map(_.proportionInvalid * 100.0),
  //       agreement = info.map(_.agreement),
  //       earnings = stats.fold(0.0)(_.earnings) + info.fold(0.0)(_.earnings)
  //     )
  //   }
  // }

  // def allStatSummaries = {
  //   val allStats = accuracyTrackerPeek.allWorkerStats
  //   val allInfos = valManagerPeek.allWorkerInfo
  //   (allStats.keys ++ allInfos.keys).toSet.toList.flatMap((wid: String) =>
  //     StatSummary.makeFromStatsAndInfo(allStats.get(wid), allInfos.get(wid))
  //   )
  // }

  // def printStatsHeading =
  //   println(f"${"Worker ID"}%14s  ${"Verbs"}%5s  ${"Qs"}%5s  ${"Acc"}%4s  ${"As"}%5s  ${"%Bad"}%5s  ${"Agr"}%4s  $$")

  // def printSingleStatSummary(ss: StatSummary): Unit = ss match {
  //   case StatSummary(wid, numVerbsOpt, numQsOpt, accOpt, numAsOpt, numInvalidsOpt, pctBadOpt, agrOpt, earnings)=>
  //     val numVerbs = numVerbsOpt.getOrElse("")
  //     val numQs = numQsOpt.getOrElse("")
  //     val acc = accOpt.foldMap(pct => f"$pct%.2f")
  //     val numAs = numAsOpt.getOrElse("")
  //     val pctBad = pctBadOpt.foldMap(pct => f"$pct%4.2f")
  //     val agr = agrOpt.foldMap(pct => f"$pct%.2f")
  //     println(f"$wid%14s  $numVerbs%5s  $numQs%5s  $acc%4s  $numAs%5s  $pctBad%5s  $agr%4s  $earnings%.2f")
  // }

  // def statsForWorker(workerId: String): Option[StatSummary] = allStatSummaries.find(_.workerId == workerId)

  // def printStatsForWorker(workerId: String) = statsForWorker(workerId) match {
  //   case None => println("No stats for worker.")
  //   case Some(ss) =>
  //     printStatsHeading
  //     printSingleStatSummary(ss)
  // }

  // def printStats[B : Ordering](sortFn: StatSummary => B) = {
  //   val summaries = allStatSummaries.sortBy(sortFn)
  //   printStatsHeading
  //   summaries.foreach(printSingleStatSummary)
  // }

  // def printQStats = printStats(-_.numQs.getOrElse(0))
  // def printAStats = printStats(-_.numAs.getOrElse(0))

  // def printCoverageStats = genManagerPeek.coverageStats.toList
  //   .sortBy(-_._2.size)
  //   .map { case (workerId, numQs) => f"$workerId%s\t${numQs.size}%d\t${numQs.sum.toDouble / numQs.size}%.2f" }
  //   .foreach(println)

  // def printGenFeedback(n: Int) = genManagerPeek.feedbacks.take(n).foreach(a =>
  //   println(a.workerId + " " + a.feedback)
  // )
  // def printValFeedback(n: Int) = valManagerPeek.feedbacks.take(n).foreach(a =>
  //   println(a.workerId + " " + a.feedback)
  // )

  // def printAllFeedbacks(n: Int = Int.MaxValue) = {
  //   println("Generation:")
  //   printGenFeedback(n)
  //   println("\nValidation:")
  //   printValFeedback(n)
  // }

  // def aggregateStats = allStatSummaries.foldLeft(AggregateStatSummary.empty)(_ combine _)

  // def printAggregateStats = aggregateStats match {
  //   case AggregateStatSummary(numVerbs, numQs, numAs, numInvalidAnswers, totalCost) =>
  //     println(f"${"Num verbs:"}%-20s$numVerbs%d")
  //     println(f"${"Num questions:"}%-20s$numQs%d")
  //     println(f"${"Num answers:"}%-20s$numAs%d")
  //     println(f"${"Num invalids:"}%-20s$numInvalidAnswers%d")
  //     println(f"${"Total cost:"}%-20s$totalCost%.2f")
  // }
}
