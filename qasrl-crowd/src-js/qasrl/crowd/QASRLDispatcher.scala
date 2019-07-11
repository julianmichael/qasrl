package qasrl.crowd

import spacro.tasks._

import scalajs.js
import scalajs.js.JSApp
import org.scalajs.jquery.jQuery

import japgolly.scalajs.react.vdom.html_<^.VdomTag

import io.circe.{Encoder, Decoder}

abstract class QASRLDispatcher[SID: Encoder : Decoder](implicit settings: QASRLSettings)
    extends TaskDispatcher {

  def generationInstructions: VdomTag
  def validationInstructions: VdomTag

  lazy val genClient = new QASRLGenerationClient[SID](generationInstructions)

  lazy val valClient = new QASRLValidationClient[SID](validationInstructions)

  final override lazy val taskMapping = Map[String, () => Unit](
    settings.generationTaskKey -> (() => genClient.main),
    settings.validationTaskKey -> (() => valClient.main)
  )

}
