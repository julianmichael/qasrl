package qasrl

import qasrl.util.implicits._

import cats.Order
import cats.data.Ior
import cats.data.NonEmptyList

import nlpdata.util.LowerCaseStrings._

import QuestionProcessor.InProgressState
import QuestionProcessor.ValidState

import Autocomplete.Suggestion

class Autocomplete(questionProcessor: QuestionProcessor) {

  private[this] def createSuggestion(ips: InProgressState): Suggestion =
    Suggestion(ips.fullText, questionProcessor.isAlmostComplete(ips))

  private[this] def partitionResults(
    goodStates: NonEmptyList[ValidState]
  ): Ior[NonEmptyList[Suggestion], NonEmptyList[QuestionProcessor.CompleteState]] =
    goodStates
      .partition(ValidState.eitherIso.get)
      .leftMap(ipss => ipss.map(createSuggestion).distinct.sorted)

  def apply(
    question: LowerCaseString,
    completeQuestions: Set[QuestionProcessor.CompleteState]
  ): Autocomplete.Result = {
    val allLowercaseQuestionStrings = completeQuestions.map(_.fullText.lowerCase).toSet
    questionProcessor.processStringFully(question) match {
      case Left(QuestionProcessor.AggregatedInvalidState(lastGoodStates, badStartIndex)) =>
        partitionResults(lastGoodStates).swap.toEither.fold(
          completeStates =>
            Autocomplete.incomplete(
              NonEmptyList.of(Suggestion(completeStates.head.fullText, true)),
              Some(badStartIndex)
          ),
          suggestions => Autocomplete.incomplete(suggestions, Some(badStartIndex))
        )
      case Right(goodStates) =>
        val framesWithAnswerSlots = completeQuestions.map { state =>
          state.frame -> state.answerSlot
        }

        val framesByCountDecreasing = framesWithAnswerSlots
          .map(_._1)
          .groupBy(identity)
          .map(p => p._1 -> p._2.size)
          .toVector
          .sortBy { case (frame, count) => -10 * count + math.abs(frame.args.size - 2) }
          .map(_._1)

        val frameToFilledAnswerSlots = framesWithAnswerSlots
          .foldLeft(Map.empty[Frame, Set[ArgumentSlot]].withDefaultValue(Set.empty[ArgumentSlot])) {
            case (acc, (frame, slot)) => acc.updated(frame, acc(frame) + slot)
          }

        val allQuestions = framesByCountDecreasing
          .flatMap { frame =>
            val unAnsweredSlots = frame.args.keys.toSet -- frameToFilledAnswerSlots(frame)
            val coreArgQuestions = unAnsweredSlots.toList.flatMap(frame.questionsForSlot)
            val advQuestions = ArgumentSlot.allAdvSlots
              .filterNot(frameToFilledAnswerSlots(frame).contains)
              .flatMap(frame.questionsForSlot)
            coreArgQuestions ++ advQuestions
          }
          .filter(_.toLowerCase.startsWith(question.toLowerCase))
          .filterNot(q => allLowercaseQuestionStrings.contains(q.lowerCase))
          .distinct

        val questionSuggestions = allQuestions
          .flatMap(
            q =>
              questionProcessor.processStringFully(q) match {
                case Right(goodStates) if goodStates.exists(_.isComplete) =>
                  Some(Suggestion(q, true))
                case _ => None
            }
          )
          .take(4) // number of suggested questions capped at 4 to filter out crowd of bad ones

        partitionResults(goodStates).toEither.fold(
          suggestions =>
            Autocomplete.incomplete(
              NonEmptyList
                .fromList(questionSuggestions.toList)
                .fold(suggestions)(sugg => (sugg ++ suggestions.toList).distinct.sorted),
              None
          ),
          completeStates => Autocomplete.complete(completeStates.toList.toSet)
        )
    }
  }
}

object Autocomplete {

  sealed trait Result
  case class Incomplete(
    suggestions: NonEmptyList[Suggestion],
    badStartIndexOpt: Option[Int]
  ) extends Result
  case class Complete(
    completionFrames: Set[QuestionProcessor.CompleteState]
  ) extends Result

  def incomplete(
    suggestions: NonEmptyList[Suggestion],
    badStartIndexOpt: Option[Int]
  ): Result = Incomplete(suggestions, badStartIndexOpt)

  def complete(completionFrames: Set[QuestionProcessor.CompleteState]): Result =
    Complete(completionFrames)

  case class Suggestion(fullText: String, isComplete: Boolean)

  object Suggestion {
    implicit val autocompleteSuggestionOrder: Order[Suggestion] = new Order[Suggestion] {
      override def compare(x: Suggestion, y: Suggestion) =
        if (x.isComplete == y.isComplete) {
          x.fullText.compare(y.fullText)
        } else if (x.isComplete) -1
        else 1
    }
  }
}
