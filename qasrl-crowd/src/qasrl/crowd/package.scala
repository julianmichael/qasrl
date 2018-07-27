package qasrl

import spacro.tasks.ResponseRW

import nlpdata.datasets.wiktionary.InflectedForms

import upickle.default._

package object crowd {

  import nlpdata.util.LowerCaseStrings._

  implicit val lowerCaseStringReader = upickle.default.Reader[LowerCaseString] {
    case upickle.Js.Str(s) => s.toString.lowerCase // just for typing. whatever
  }
  implicit val lowerCaseStringWriter = upickle.default.Writer[LowerCaseString] {
    case s => upickle.Js.Str(s.toString)
  }
  implicit val inflectedFormsReader = macroR[InflectedForms]
  implicit val inflectedFormsWriter = macroW[InflectedForms]

  case class QASRLGenerationPrompt[SID](id: SID, verbIndex: Int)
  object QASRLGenerationPrompt {
    implicit def reader[SID: Reader] = macroR[QASRLGenerationPrompt[SID]]
    implicit def writer[SID: Writer] = macroW[QASRLGenerationPrompt[SID]]
  }

  case class GenerationStatSummary(
    numVerbsCompleted: Int, // before validation: used to calculate coverage
    numQuestionsWritten: Int, // before validation: "
    workerStatsOpt: Option[QASRLGenerationWorkerStats]
  )
  object GenerationStatSummary {
    implicit val reader = macroR[GenerationStatSummary]
    implicit val writer = macroW[GenerationStatSummary]
  }

  case class QASRLGenerationAjaxRequest[SID](
    workerIdOpt: Option[String],
    prompt: QASRLGenerationPrompt[SID]
  ) {
    type Response = QASRLGenerationAjaxResponse
  }

  case class QASRLGenerationAjaxResponse(
    stats: GenerationStatSummary,
    tokens: Vector[String],
    inflectedForms: InflectedForms
  )
  object QASRLGenerationAjaxResponse {
    implicit val reader = macroR[QASRLGenerationAjaxResponse]
    implicit val writer = macroW[QASRLGenerationAjaxResponse]
  }

  object QASRLGenerationAjaxRequest {
    import upickle.default._
    implicit def responseRW[SID] = new ResponseRW[QASRLGenerationAjaxRequest[SID]] {
      override def getReader(request: QASRLGenerationAjaxRequest[SID]) =
        QASRLGenerationAjaxResponse.reader
      override def getWriter(request: QASRLGenerationAjaxRequest[SID]) =
        QASRLGenerationAjaxResponse.writer
    }
    implicit def reader[SID: Reader] = macroR[QASRLGenerationAjaxRequest[SID]]
    implicit def writer[SID: Writer] = macroW[QASRLGenerationAjaxRequest[SID]]
  }

  case class QASRLValidationAjaxResponse(
    workerInfoOpt: Option[QASRLValidationWorkerInfoSummary],
    sentence: Vector[String]
  )
  object QASRLValidationAjaxResponse {
    implicit val reader = macroR[QASRLValidationAjaxResponse]
    implicit val writer = macroW[QASRLValidationAjaxResponse]
  }

  case class QASRLValidationAjaxRequest[SID](workerIdOpt: Option[String], id: SID) {
    type Response = QASRLValidationAjaxResponse
  }
  object QASRLValidationAjaxRequest {
    import upickle.default._
    implicit def responseRW[SID] = new ResponseRW[QASRLValidationAjaxRequest[SID]] {
      override def getReader(request: QASRLValidationAjaxRequest[SID]) =
        QASRLValidationAjaxResponse.reader
      override def getWriter(request: QASRLValidationAjaxRequest[SID]) =
        QASRLValidationAjaxResponse.writer
    }
    implicit def reader[SID: Reader] = macroR[QASRLValidationAjaxRequest[SID]]
    implicit def writer[SID: Writer] = macroW[QASRLValidationAjaxRequest[SID]]
  }

}
