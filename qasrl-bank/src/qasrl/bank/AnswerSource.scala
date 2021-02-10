package qasrl.bank

import cats.Order
import cats.implicits._

case class AnswerSource(turkerId: String, round: AnnotationRound)

object AnswerSource {
  private[this] val QasrlTurkerMatch = "turk-qasrl2.0-([0-9]+)-?(.*)".r
  private[this] val QANomTurkerMatch = "turk-qanom-([0-9]+)".r
  import AnnotationRound._

  def fromString(s: String) = s match {
    case QasrlTurkerMatch(id, round) =>
      AnswerSource(
        id,
        round match {
          case ""          => Original
          case "expansion" => Expansion
          case "eval"      => Eval
        }
      )
    case QANomTurkerMatch(id) => AnswerSource(id, QANom)
  }

  implicit val answerSourceOrder = Order.whenEqual(
    Order.by[AnswerSource, AnnotationRound](_.round),
    Order.by[AnswerSource, String](_.turkerId)
  )
}
