package qasrl.labeling

import qasrl.ArgStructure
import qasrl.ArgumentSlot
import qasrl.Frame

import cats.Order
import cats.implicits._

import io.circe.generic.JsonCodec

import monocle.macros.Lenses

@Lenses @JsonCodec case class ClausalQuestion(
  frame: Frame,
  slot: ArgumentSlot
) {
  def questionString = frame.questionsForSlot(slot).head
  def clauseTemplate = ArgStructure(frame.args, frame.isPassive).forgetAnimacy
}
object ClausalQuestion {
  implicit val clausalQuestionOrder: Order[ClausalQuestion] = Order.whenEqual(
    Order.by[ClausalQuestion, String](_.frame.toString),
    Order.by[ClausalQuestion, String](_.slot.toString)
  )
}
