package qasrl.bank

import cats.Order
import cats.implicits._

sealed trait QuestionSource {
  import QuestionSource._
  def getModel = Some(this).collect { case x: Model => x }
  def isModel = getModel.nonEmpty
  def getQasrlTurker = Some(this).collect { case x: QasrlTurker => x }
  def isQasrlTurker = getQasrlTurker.nonEmpty
  def getQANomTurker = Some(this).collect { case x: QANomTurker => x }
  def isQANomTurker = getQANomTurker.nonEmpty
}

object QuestionSource {
  case class Model(version: String) extends QuestionSource
  case class QasrlTurker(turkerId: String) extends QuestionSource
  case class QANomTurker(turkerId: String) extends QuestionSource

  private[this] val ModelMatch = "model-qasrl2.0-(.+)".r
  private[this] val QasrlTurkerMatch = "turk-qasrl2.0-([0-9]+)".r
  private[this] val QANomTurkerMatch = "turk-qanom-([0-9]+)".r

  def fromString(s: String): QuestionSource = s match {
    case ModelMatch(version)      => Model(version)
    case QasrlTurkerMatch(id)     => QasrlTurker(id)
    case QANomTurkerMatch(id)     => QANomTurker(id)
  }

  implicit val questionSourceOrder: Order[QuestionSource] = Order.whenEqual(
    Order.by[QuestionSource, Int] {
      case QasrlTurker(_) => 0
      case Model(_)  => 1
      case QANomTurker(_) => 0
    },
    Order.by[QuestionSource, String] {
      case QasrlTurker(id) => id
      case Model(ver) => ver
      case QANomTurker(id) => id
    }
  )
}
