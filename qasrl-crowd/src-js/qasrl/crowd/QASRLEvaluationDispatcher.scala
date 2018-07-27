package qasrl.crowd

import spacro.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import upickle.default._

import japgolly.scalajs.react.vdom.html_<^.VdomTag

abstract class QASRLEvaluationDispatcher[SID: Reader: Writer](
  implicit settings: QASRLEvaluationSettings
) extends TaskDispatcher {

  def evaluationInstructions: VdomTag

  lazy val evalClient = new QASRLEvaluationClient[SID](evaluationInstructions)

  final override lazy val taskMapping = Map[String, () => Unit](
    settings.evaluationTaskKey -> (() => evalClient.main)
  )

}
