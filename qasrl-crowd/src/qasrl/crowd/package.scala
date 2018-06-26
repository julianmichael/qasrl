package qasrl

import spacro.tasks.ResponseRW

import nlpdata.datasets.wiktionary.InflectedForms

package object crowd {

  case class QASRLGenerationPrompt[SID](id: SID, verbIndex: Int)

  case class GenerationStatSummary(
    numVerbsCompleted: Int, // before validation: used to calculate coverage
    numQuestionsWritten: Int, // before validation: "
    workerStatsOpt: Option[QASRLGenerationWorkerStats]
  )

  case class QASRLGenerationAjaxRequest[SID](
    workerIdOpt: Option[String],
    prompt: QASRLGenerationPrompt[SID]
  ) {
    type Response = QASRLGenerationAjaxResponse
  }

  object QASRLGenerationAjaxRequest {
    import upickle.default._
    implicit def responseRW[SID] = new ResponseRW[QASRLGenerationAjaxRequest[SID]] {
      override def getReader(request: QASRLGenerationAjaxRequest[SID]) =
        implicitly[Reader[QASRLGenerationAjaxResponse]]
      override def getWriter(request: QASRLGenerationAjaxRequest[SID]) =
        implicitly[Writer[QASRLGenerationAjaxResponse]]
    }
  }

  case class QASRLGenerationAjaxResponse(
    stats: GenerationStatSummary,
    tokens: Vector[String],
    inflectedForms: InflectedForms
  )

  case class QASRLValidationAjaxRequest[SID](workerIdOpt: Option[String], id: SID) {
    type Response = QASRLValidationAjaxResponse
  }

  object QASRLValidationAjaxRequest {
    import upickle.default._
    implicit def responseRW[SID] = new ResponseRW[QASRLValidationAjaxRequest[SID]] {
      override def getReader(request: QASRLValidationAjaxRequest[SID]) =
        implicitly[Reader[QASRLValidationAjaxResponse]]
      override def getWriter(request: QASRLValidationAjaxRequest[SID]) =
        implicitly[Writer[QASRLValidationAjaxResponse]]
    }
  }

  case class QASRLValidationAjaxResponse(
    workerInfoOpt: Option[QASRLValidationWorkerInfoSummary],
    sentence: Vector[String]
  )

  import nlpdata.util.LowerCaseStrings._

  implicit val lowerCaseStringReader = upickle.default.Reader[LowerCaseString] {
    case upickle.Js.Str(s) => s.lowerCase // just for typing. whatever
  }
  implicit val lowerCaseStringWriter = upickle.default.Writer[LowerCaseString] {
    case s => upickle.Js.Str(s.toString)
  }

}
