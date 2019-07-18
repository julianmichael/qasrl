package qasrl.data

import cats.data.NonEmptySet

import jjm.ling.ESpan

import monocle.macros.{GenPrism, Lenses}

sealed trait AnswerJudgment {

  def isInvalid = this match {
    case InvalidQuestion => true
    case _               => false
  }

  def getAnswer = this match {
    case a @ Answer(_) => Some(a)
    case _             => None
  }
  def isAnswer = getAnswer.nonEmpty
}
case object InvalidQuestion extends AnswerJudgment
@Lenses case class Answer(spans: NonEmptySet[ESpan]) extends AnswerJudgment

object AnswerJudgment {
  val invalidQuestion = GenPrism[AnswerJudgment, InvalidQuestion.type]
  val answer = GenPrism[AnswerJudgment, Answer]
}
