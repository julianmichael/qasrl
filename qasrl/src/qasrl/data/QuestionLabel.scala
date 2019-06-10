package qasrl.data

import monocle.macros.Lenses

import qasrl.Tense
import qasrl.labeling.SlotBasedLabel

import jjm.ling.en.VerbForm
import jjm.implicits._

@Lenses case class QuestionLabel(
  questionString: String,
  questionSources: Set[String],
  answerJudgments: Set[AnswerLabel],
  questionSlots: SlotBasedLabel[VerbForm],
  tense: Tense,
  isPerfect: Boolean,
  isProgressive: Boolean,
  isNegated: Boolean,
  isPassive: Boolean
) {

  // requires that questions are about the same verb with same inflected forms
  def combineWithLike(other: QuestionLabel): Either[String, QuestionLabel] = {
    val cmps = List(
      questionString != other.questionString,
      questionSlots != other.questionSlots,
      tense != other.tense,
      isPerfect != other.isPerfect,
      isProgressive != other.isProgressive,
      isNegated != other.isNegated,
      isPassive != isPassive
    )
    if (cmps.exists(identity)) {
      val thisQString = questionString + " (" + questionSlots.renderWithSeparator(
        vf => JsonCodecs.verbFormToString(vf).lowerCase,
        ","
      ) + ")"
      val otherQString = other.questionString + " (" + other.questionSlots.renderWithSeparator(
        vf => JsonCodecs.verbFormToString(vf).lowerCase,
        ","
      ) + ")"
      Left(
        s"""Can only combine like questions; attempted to combine $thisQString and $otherQString"""
      )
    } else
      Right(
        QuestionLabel(
          questionString,
          questionSources ++ other.questionSources,
          answerJudgments ++ other.answerJudgments,
          questionSlots,
          tense,
          isPerfect,
          isProgressive,
          isNegated,
          isPassive
        )
      )
  }
}
