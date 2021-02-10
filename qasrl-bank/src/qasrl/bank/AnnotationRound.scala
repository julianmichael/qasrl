package qasrl.bank

import cats.Order
import cats.implicits._

sealed trait AnnotationRound {
  import AnnotationRound._
  def isQasrlOriginal = this == Original
  def isQasrlExpansion = this == Expansion
  def isQasrlEval = this == Eval
  def isQANom = this == QANom
}

object AnnotationRound {
  case object Original extends AnnotationRound
  case object Expansion extends AnnotationRound
  case object Eval extends AnnotationRound
  case object QANom extends AnnotationRound

  implicit val annotationRoundOrder = Order.by[AnnotationRound, Int] {
    case Original  => 0
    case Expansion => 1
    case Eval      => 2
    case QANom     => 3
  }
}
