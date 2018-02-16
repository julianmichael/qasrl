package qasrl.crowd

import qasrl.crowd.util.PosTagger
import qasrl.crowd.util.implicits._
import qasrl.labeling.SlotBasedLabel

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

class QASRLAnnotationPipeline[SID : Reader : Writer : HasTokens](
  val allIds: Vector[SID], // IDs of sentences to annotate
  numGenerationAssignmentsForPrompt: QASRLGenerationPrompt[SID] => Int,
  annotationDataService: AnnotationDataService,
  frozenGenerationHITTypeId: Option[String] = None,
  frozenValidationHITTypeId: Option[String] = None,
  generationAccuracyDisqualTypeLabel: Option[String] = None,
  generationCoverageDisqualTypeLabel: Option[String] = None,
  validationAgreementDisqualTypeLabel: Option[String] = None)(
  implicit val config: TaskConfig,
  val settings: QASRLSettings,
  val inflections: Inflections
) extends StrictLogging {

  def getKeyIndices(id: SID): Set[Int] = {
    val posTaggedTokens = PosTagger.posTag(id.tokens)
    posTaggedTokens.collect {
      case Word(index, pos, token) if PosTags.verbPosTags.contains(pos) =>
        if( // detect if "have"-verb is an auxiliary
          Inflections.haveVerbs.contains(token.lowerCase) &&
            (posTaggedTokens.lift(index + 1).map(_.token.lowerCase).nonEmptyAnd(Inflections.negationWords.contains) || // negation appears directly after, or
               posTaggedTokens.drop(index + 1).forall(_.pos != "VBN") || // there is no past-participle verb afterward, or
               posTaggedTokens.drop(index + 1) // after the "have" verb,
               .takeWhile(_.pos != "VBN") // until the next past-participle form verb,
               .forall(w => Inflections.negationWords.contains(w.token.lowerCase) || PosTags.adverbPosTags.contains(w.pos)) // everything is an adverb or negation (though I guess negs are RB)
            )
        ) None else if( // detect if "do"-verb is an auxiliary
          Inflections.doVerbs.contains(token.lowerCase) &&
            (posTaggedTokens.lift(index + 1).map(_.token.lowerCase).nonEmptyAnd(Inflections.negationWords.contains) || // negation appears directly after, or
               posTaggedTokens.drop(index + 1).forall(w => w.pos != "VB" && w.pos != "VBP") || // there is no stem or non-3rd-person present verb afterward (to mitigate pos tagger mistakes), or
               posTaggedTokens.drop(index + 1) // after the "do" verb,
               .takeWhile(w => w.pos != "VBP" && w.pos != "VBP") // until the next VB or VBP verb,
               .forall(w => Inflections.negationWords.contains(w.token.lowerCase) || PosTags.adverbPosTags.contains(w.pos)) // everything is an adverb or negation (though I guess negs are RB)
            )
        ) None else inflections.getInflectedForms(token.lowerCase).map(_ => index)
    }.flatten.toSet
  }

  lazy val allPrompts: Vector[QASRLGenerationPrompt[SID]] = for {
    id <- allIds
    verbIndex <- getKeyIndices(id).toList.sorted
  } yield QASRLGenerationPrompt(id, verbIndex)

  implicit val ads = annotationDataService

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

  val genAccDisqualTypeLabelString = generationAccuracyDisqualTypeLabel.fold("")(x => s"[$x] ")
  val genAccDisqualTypeName = s"${genAccDisqualTypeLabelString}Question-answer writing accuracy disqualification"
  val genAccDisqualType = config.service.listQualificationTypes(
    new ListQualificationTypesRequest()
      .withQuery(genAccDisqualTypeName)
      .withMustBeOwnedByCaller(true)
      .withMustBeRequestable(false)
      .withMaxResults(100)
  ).getQualificationTypes.asScala.toList.find(_.getName == genAccDisqualTypeName).getOrElse {
    System.out.println("Generating generation accuracy disqualification type...")
    config.service.createQualificationType(
      new CreateQualificationTypeRequest()
        .withName(genAccDisqualTypeName)
        .withKeywords("language,english,question answering")
        .withDescription("""Accuracy on the question-answer writing task is too low.""".replaceAll("\\s+", " "))
        .withQualificationTypeStatus(QualificationTypeStatus.Active)
    ).getQualificationType
  }
  val genAccDisqualTypeId = genAccDisqualType.getQualificationTypeId
  val genAccuracyRequirement = new QualificationRequirement()
    .withQualificationTypeId(genAccDisqualTypeId)
    .withComparator("DoesNotExist")
    .withRequiredToPreview(false)

  val genCoverageDisqualTypeLabelString = generationCoverageDisqualTypeLabel.fold("")(x => s"[$x] ")
  val genCoverageDisqualTypeName = s"${genCoverageDisqualTypeLabelString} Questions asked per verb disqualification"
  val genCoverageDisqualType = config.service.listQualificationTypes(
    new ListQualificationTypesRequest()
      .withQuery(genCoverageDisqualTypeName)
      .withMustBeOwnedByCaller(true)
      .withMustBeRequestable(false)
      .withMaxResults(100)
  ).getQualificationTypes.asScala.toList.find(_.getName == genCoverageDisqualTypeName).getOrElse {
    System.out.println("Generating generation coverage disqualification type...")
    config.service.createQualificationType(
      new CreateQualificationTypeRequest()
        .withName(genCoverageDisqualTypeName)
        .withKeywords("language,english,question answering")
        .withDescription("""Number of questions asked for each verb
          in our question-answer pair generation task is too low.""".replaceAll("\\s+", " "))
        .withQualificationTypeStatus(QualificationTypeStatus.Active)
        .withAutoGranted(false)
    ).getQualificationType
  }
  val genCoverageDisqualTypeId = genCoverageDisqualType.getQualificationTypeId
  val genCoverageRequirement = new QualificationRequirement()
    .withQualificationTypeId(genCoverageDisqualTypeId)
    .withComparator("DoesNotExist")
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
    revokeAllWorkerQuals(genAccDisqualTypeId)
    revokeAllWorkerQuals(genCoverageDisqualTypeId)
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

  val genHITType = HITType(
    title = s"Write question-answer pairs about a verb",
    description = s"""
      Given a sentence and a verb from that sentence,
      write questions and answers about that verb.
      Questions must adhere to a certain template,
      provided by autocomplete functionality.
      Maintain high accuracy to stay qualified.
    """.trim.replace("\\s+", " "),
    reward = settings.generationReward,
    keywords = "language,english,question answering",
    qualRequirements = Array[QualificationRequirement](
      approvalRateRequirement, localeRequirement, genAccuracyRequirement, genCoverageRequirement
    ),
    autoApprovalDelay = 2592000L, // 30 days
    assignmentDuration = 600L)

  lazy val genAjaxService = new Service[QASRLGenerationAjaxRequest[SID]] {
    override def processRequest(request: QASRLGenerationAjaxRequest[SID]) = request match {
      case QASRLGenerationAjaxRequest(workerIdOpt, QASRLGenerationPrompt(id, verbIndex)) =>
        val questionListsOpt = for {
          genManagerP <- Option(genManagerPeek)
          workerId <- workerIdOpt
          qCounts <- genManagerP.coverageStats.get(workerId)
        } yield qCounts
        val questionLists = questionListsOpt.getOrElse(Nil)

        val workerStatsOpt = for {
          accTrackP <- Option(accuracyTrackerPeek)
          workerId <- workerIdOpt
          stats <- accTrackP.allWorkerStats.get(workerId)
        } yield stats

        val stats = GenerationStatSummary(
          numVerbsCompleted = questionLists.size,
          numQuestionsWritten = questionLists.sum,
          workerStatsOpt = workerStatsOpt)

        val tokens = id.tokens
        val inflectedForms = inflections.getInflectedForms(tokens(verbIndex).lowerCase).get
        QASRLGenerationAjaxResponse(stats, tokens, inflectedForms)
    }
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
    autoApprovalDelay = 2592000L, // 30 days
    assignmentDuration = 600L)

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

  lazy val sampleValPrompt = QASRLValidationPrompt[SID](
    allPrompts.head, "", "", "",
    List(VerbQA(0, "Who did someone look at?", List(Span(4, 4))),
         VerbQA(1, "Who looked at someone?", List(Span(0, 1))),
         VerbQA(1, "How did someone look at someone?", List(Span(5, 5)))))

  lazy val valTaskSpec = TaskSpecification.NoWebsockets[QASRLValidationPrompt[SID], List[QASRLValidationAnswer], QASRLValidationAjaxRequest[SID]](
    settings.validationTaskKey, valHITType, valAjaxService, Vector(sampleValPrompt),
    taskPageHeadElements = taskPageHeadLinks,
    taskPageBodyElements = taskPageBodyLinks,
    frozenHITTypeId = frozenValidationHITTypeId)

  // hit management --- circularly defined so they can communicate

  import config.actorSystem

  var accuracyTrackerPeek: QASRLGenerationAccuracyManager[SID] = null

  lazy val accuracyTracker: ActorRef = actorSystem.actorOf(
    Props {
      accuracyTrackerPeek = new QASRLGenerationAccuracyManager[SID](genAccDisqualTypeId)
      accuracyTrackerPeek
    }
  )

  var valManagerPeek: QASRLValidationHITManager[SID] = null

  lazy val valHelper = new HITManager.Helper(valTaskSpec)
  lazy val valManager: ActorRef = actorSystem.actorOf(
    Props {
      valManagerPeek = new QASRLValidationHITManager(
        valHelper,
        valAgrDisqualTypeId,
        accuracyTracker,
        // sentenceTracker,
        if(config.isProduction) (_ => 2) else (_ => 1),
        if(config.isProduction) 100 else 3)
      valManagerPeek
    })

  lazy val valActor = actorSystem.actorOf(Props(new TaskManager(valHelper, valManager)))

  val genTaskSpec = TaskSpecification.NoWebsockets[QASRLGenerationPrompt[SID], List[VerbQA], QASRLGenerationAjaxRequest[SID]](
    settings.generationTaskKey, genHITType, genAjaxService, allPrompts,
    taskPageHeadElements = taskPageHeadLinks,
    taskPageBodyElements = taskPageBodyLinks,
    frozenHITTypeId = frozenGenerationHITTypeId)

  var genManagerPeek: QASRLGenerationHITManager[SID] = null

  val genHelper = new HITManager.Helper(genTaskSpec)
  val genManager: ActorRef = actorSystem.actorOf(
    Props {
      genManagerPeek = new QASRLGenerationHITManager(
        genHelper,
        valHelper,
        valManager,
        genCoverageDisqualTypeId,
        // sentenceTracker,
        if(config.isProduction) numGenerationAssignmentsForPrompt else (_ => 1),
        if(config.isProduction) 100 else 3,
        allPrompts.iterator)
      genManagerPeek
    }
  )
  val genActor = actorSystem.actorOf(Props(new TaskManager(genHelper, genManager)))

  lazy val server = new Server(List(genTaskSpec, valTaskSpec))

  // used to schedule data-saves
  private[this] var schedule: List[Cancellable] = Nil
  def startSaves(interval: FiniteDuration = 5 minutes): Unit = {
    if(schedule.exists(_.isCancelled) || schedule.isEmpty) {
      schedule = List(genManager, valManager, accuracyTracker).map(actor =>
        config.actorSystem.scheduler.schedule(
          2 seconds, interval, actor, SaveData)(
          config.actorSystem.dispatcher, actor)
      )
    }
  }
  def stopSaves = schedule.foreach(_.cancel())

  def setGenHITsActiveEach(n: Int) = {
    genManager ! SetNumHITsActive(n)
  }
  def setValHITsActive(n: Int) = {
    valManager ! SetNumHITsActive(n)
  }

  import TaskManager.Message._
  def start(interval: FiniteDuration = 30 seconds) = {
    server
    startSaves()
    genActor ! Start(interval, delay = 0 seconds)
    valActor ! Start(interval, delay = 3 seconds)
  }
  def stop() = {
    genActor ! Stop
    valActor ! Stop
    stopSaves
  }
  def delete() = {
    genActor ! Delete
    valActor ! Delete
  }
  def expire() = {
    genActor ! Expire
    valActor ! Expire
  }
  def update() = {
    server
    genActor ! Update
    valActor ! Update
  }
  def save() = {
    // sentenceTracker ! SaveData
    accuracyTracker ! SaveData
    genManager ! SaveData
    valManager ! SaveData
  }

  // for use while it's running. Ideally instead of having to futz around at the console calling these functions,
  // in the future you could have a nice dashboard UI that will help you examine common sources of issues

  def allGenInfos = hitDataService.getAllHITInfo[QASRLGenerationPrompt[SID], List[VerbQA]](genTaskSpec.hitTypeId).get

  def allValInfos = hitDataService.getAllHITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]](valTaskSpec.hitTypeId).get

  def currentGenSentences: List[(SID, String)] = {
    genHelper.activeHITInfosByPromptIterator.map(_._1.id).map(id =>
      id -> Text.render(id.tokens)
    ).toList
  }

  def latestValInfos(n: Int = 5) = allValInfos
    .filter(_.assignments.nonEmpty)
    .sortBy(_.assignments.map(_.submitTime).max)
    .takeRight(n)

  // sorted increasing by submit time
  def infosForGenWorker(workerId: String) = {
    val scored = for {
      hi <- allValInfos
      sourceAssignment <- hitDataService.getAssignmentsForHIT[List[VerbQA]](genTaskSpec.hitTypeId, hi.hit.prompt.sourceHITId).toOptionLogging(logger).toList.flatten
      if sourceAssignment.assignmentId == hi.hit.prompt.sourceAssignmentId
      if sourceAssignment.workerId == workerId
    } yield (hi, sourceAssignment.submitTime)
    scored.sortBy(_._2).map(_._1)
  }

  // sorted increasing by submit time
  def infosForValWorker(workerId: String) = {
    val scored = for {
      hi <- allValInfos
      if hi.assignments.exists(_.workerId == workerId)
      workerAssignment = hi.assignments.find(_.workerId == workerId).get
      nonWorkerAssignments = hi.assignments.filter(_.workerId != workerId)
    } yield (HITInfo(hi.hit, workerAssignment :: nonWorkerAssignments), workerAssignment.submitTime)
    scored.sortBy(_._2).map(_._1)
  }

  def renderValidation(info: HITInfo[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]]) = {
    val sentence = info.hit.prompt.genPrompt.id.tokens
    val genWorkerString = hitDataService
      .getAssignmentsForHIT[List[VerbQA]](genTaskSpec.hitTypeId, info.hit.prompt.sourceHITId).get
      .find(_.assignmentId == info.hit.prompt.sourceAssignmentId)
      .fold("")(_.workerId)
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

  // print LATEST and WORST n entries for val or gen worker, n default Int.MaxValue

  def printLatestGenInfos(workerId: String, n: Int = 5) =
    infosForGenWorker(workerId)
      .takeRight(n)
      .map(renderValidation)
      .foreach(println)

  def printWorstGenInfos(workerId: String, n: Int = 5) =
    infosForGenWorker(workerId)
      .sortBy(_.assignments.flatMap(_.response).filter(_.isInvalid).size)
      .takeRight(n)
      .map(renderValidation)
      .foreach(println)

  def printLatestValInfos(workerId: String, n: Int = 5) =
    infosForValWorker(workerId)
      .takeRight(n)
      .map(renderValidation)
      .foreach(println)

  def printWorstValInfos(workerId: String, n: Int = 5) =
    infosForValWorker(workerId)
      .sortBy { hi =>
      if(hi.assignments.size <= 1) Int.MinValue else {
        val totalQAPairs = hi.hit.prompt.qaPairs.size.toDouble
        val agreedQAPairs = hi.assignments.head.response
          .zip(hi.assignments.tail.map(a => a.response.map(a.workerId -> _)).transpose)
          .map { case (givenAnswer, refPairs) =>
            QASRLValidationResponseComparison(
              givenAnswer,
              refPairs.filter(p => !valManagerPeek.blockedValidators.contains(p._1))
            ) }
          .filter(_.isAgreement).size
        totalQAPairs - agreedQAPairs } }
      .takeRight(n)
      .map(renderValidation)
      .foreach(println)

  case class StatSummary(
    workerId: String,
    numVerbs: Option[Int],
    numQs: Option[Int],
    accuracy: Option[Double],
    numAs: Option[Int],
    numInvalidAnswers: Option[Int],
    pctBad: Option[Double],
    agreement: Option[Double],
    earnings: Double)

  case class AggregateStatSummary(
    numVerbs: Int,
    numQs: Int,
    numAs: Int,
    numInvalidAnswers: Int,
    totalCost: Double) {
    def combine(worker: StatSummary) = AggregateStatSummary(
      numVerbs + worker.numVerbs.getOrElse(0),
      numQs + worker.numQs.getOrElse(0),
      numAs + worker.numAs.getOrElse(0) + worker.numInvalidAnswers.getOrElse(0),
      numInvalidAnswers + worker.numInvalidAnswers.getOrElse(0),
      totalCost + worker.earnings
    )
  }
  object AggregateStatSummary {
    def empty = AggregateStatSummary(0, 0, 0, 0, 0.0)
  }

  object StatSummary {
    def makeFromStatsAndInfo(
      stats: Option[QASRLGenerationWorkerStats],
      info: Option[QASRLValidationWorkerInfo]
    ) = stats.map(_.workerId).orElse(info.map(_.workerId)).map { wid =>
      StatSummary(
        workerId = wid,
        numVerbs = stats.map(_.numAssignmentsCompleted),
        numQs = stats.map(_.numQAPairsWritten),
        accuracy = stats.map(_.accuracy),
        numAs = info.map(i => i.numAnswerSpans + i.numInvalids),
        numInvalidAnswers = info.map(_.numInvalids),
        pctBad = info.map(_.proportionInvalid * 100.0),
        agreement = info.map(_.agreement),
        earnings = stats.fold(0.0)(_.earnings) + info.fold(0.0)(_.earnings)
      )
    }
  }

  def allStatSummaries = {
    val allStats = accuracyTrackerPeek.allWorkerStats
    val allInfos = valManagerPeek.allWorkerInfo
    (allStats.keys ++ allInfos.keys).toSet.toList.flatMap((wid: String) =>
      StatSummary.makeFromStatsAndInfo(allStats.get(wid), allInfos.get(wid))
    )
  }

  def printStatsHeading =
    println(f"${"Worker ID"}%14s  ${"Verbs"}%5s  ${"Qs"}%5s  ${"Acc"}%4s  ${"As"}%5s  ${"%Bad"}%5s  ${"Agr"}%4s  $$")

  def printSingleStatSummary(ss: StatSummary): Unit = ss match {
    case StatSummary(wid, numVerbsOpt, numQsOpt, accOpt, numAsOpt, numInvalidsOpt, pctBadOpt, agrOpt, earnings)=>
      val numVerbs = numVerbsOpt.getOrElse("")
      val numQs = numQsOpt.getOrElse("")
      val acc = accOpt.foldMap(pct => f"$pct%.2f")
      val numAs = numAsOpt.getOrElse("")
      val pctBad = pctBadOpt.foldMap(pct => f"$pct%4.2f")
      val agr = agrOpt.foldMap(pct => f"$pct%.2f")
      println(f"$wid%14s  $numVerbs%5s  $numQs%5s  $acc%4s  $numAs%5s  $pctBad%5s  $agr%4s  $earnings%.2f")
  }

  def statsForWorker(workerId: String): Option[StatSummary] = allStatSummaries.find(_.workerId == workerId)

  def printStatsForWorker(workerId: String) = statsForWorker(workerId) match {
    case None => println("No stats for worker.")
    case Some(ss) =>
      printStatsHeading
      printSingleStatSummary(ss)
  }

  def printStats[B : Ordering](sortFn: StatSummary => B) = {
    val summaries = allStatSummaries.sortBy(sortFn)
    printStatsHeading
    summaries.foreach(printSingleStatSummary)
  }

  def printQStats = printStats(-_.numQs.getOrElse(0))
  def printAStats = printStats(-_.numAs.getOrElse(0))

  def printCoverageStats = genManagerPeek.coverageStats.toList
    .sortBy(-_._2.size)
    .map { case (workerId, numQs) => f"$workerId%s\t${numQs.size}%d\t${numQs.sum.toDouble / numQs.size}%.2f" }
    .foreach(println)

  def printGenFeedback(n: Int) = genManagerPeek.feedbacks.take(n).foreach(a =>
    println(a.workerId + " " + a.feedback)
  )
  def printValFeedback(n: Int) = valManagerPeek.feedbacks.take(n).foreach(a =>
    println(a.workerId + " " + a.feedback)
  )

  def printAllFeedbacks(n: Int = Int.MaxValue) = {
    println("Generation:")
    printGenFeedback(n)
    println("\nValidation:")
    printValFeedback(n)
  }

  def aggregateStats = allStatSummaries.foldLeft(AggregateStatSummary.empty)(_ combine _)

  def printAggregateStats = aggregateStats match {
    case AggregateStatSummary(numVerbs, numQs, numAs, numInvalidAnswers, totalCost) =>
      println(f"${"Num verbs:"}%-20s$numVerbs%d")
      println(f"${"Num questions:"}%-20s$numQs%d")
      println(f"${"Num answers:"}%-20s$numAs%d")
      println(f"${"Num invalids:"}%-20s$numInvalidAnswers%d")
      println(f"${"Total cost:"}%-20s$totalCost%.2f")
  }
}
