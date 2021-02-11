package qasrl.crowd

import jjm.OrWrapped
import jjm.ling.Text
import jjm.ling.ISpan

import qasrl.crowd.util.MultiContigSpanHighlightableSentenceComponent
import qasrl.crowd.util.Styles

import spacro.tasks._

import cats.implicits._

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.raw._
import org.scalajs.dom.ext.KeyCode
import org.scalajs.jquery.jQuery

import scala.concurrent.ExecutionContext.Implicits.global

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._
import japgolly.scalajs.react.MonocleReact._

import io.circe.{Encoder, Decoder}

import jjm.ui.SpanSelection
import jjm.ui.CacheCallContent

class QASRLValidationClient[SID: Encoder : Decoder](instructions: VdomTag)(
  implicit settings: QASRLSettings,
  promptDecoder: Decoder[QASRLValidationPrompt[SID]], // macro serializers don't work for superclass constructor parameters
  responseEncoder: Encoder[List[QASRLValidationAnswer]], // same as above
  ajaxRequestEncoder: Encoder[QASRLValidationAjaxRequest[SID]] // "
) extends TaskClient[QASRLValidationPrompt[SID], List[QASRLValidationAnswer], QASRLValidationAjaxRequest[
      SID
    ]] {

  def main(): Unit = jQuery { () =>
    Styles.addToDocument()
    FullUI().renderIntoDOM(dom.document.getElementById(FieldLabels.rootClientDivLabel))
  }

  val AjaxFetch = new CacheCallContent[QASRLValidationAjaxRequest[SID], QASRLValidationAjaxResponse]
  val AnswerSelection = new SpanSelection[Int] // by question index
  import MultiContigSpanHighlightableSentenceComponent._

  lazy val questions = prompt.qaPairs.map(_.question)

  @Lenses case class State(
    curQuestion: Int,
    isInterfaceFocused: Boolean,
    answers: List[QASRLValidationAnswer]
  )

  object State {
    def initial = State(0, false, questions.map(_ => Answer(List.empty[ISpan])))
  }

  def answerSpanOptics(questionIndex: Int) =
    State.answers
      .composeOptional(Optics.index(questionIndex))
      .composePrism(QASRLValidationAnswer.answer)
      .composeLens(Answer.spans)

  class FullUIBackend(scope: BackendScope[Unit, State]) {

    def updateResponse: Callback = scope.state.map { state =>
      setResponse(state.answers)
    }

    def updateCurrentAnswers(highlightingState: AnswerSelection.State) =
      scope.state >>= (
        st =>
          scope.modState(
            answerSpanOptics(st.curQuestion).set(
              highlightingState.spans(st.curQuestion)
            )
          )
      )

    def toggleInvalidAtIndex(highlightedAnswers: Map[Int, Answer])(questionIndex: Int) =
      scope.modState(
        State.answers.modify(
          answers =>
            answers.updated(
              questionIndex,
              if (answers(questionIndex).isInvalid) highlightedAnswers(questionIndex)
              else InvalidQuestion
          )
        )
      )

    def handleKey(highlightedAnswers: Map[Int, Answer])(e: ReactKeyboardEvent): Callback = {
      def nextQuestion = scope.modState(State.curQuestion.modify(i => (i + 1) % questions.size))
      def prevQuestion =
        scope.modState(State.curQuestion.modify(i => (i + questions.size - 1) % questions.size))
      def toggleInvalid =
        scope.zoomStateL(State.curQuestion).state >>= toggleInvalidAtIndex(highlightedAnswers)

      if (isNotAssigned) {
        Callback.empty
      } else
        CallbackOption.keyCodeSwitch(e) {
          case KeyCode.Up | KeyCode.W   => prevQuestion
          case KeyCode.Down | KeyCode.S => nextQuestion
          case KeyCode.Space            => toggleInvalid
        } >> e.preventDefaultCB
    }

    def qaField(s: State, sentence: Vector[String], highlightedAnswers: Map[Int, Answer])(
      index: Int
    ) = {
      val isFocused = s.curQuestion == index
      val answer = s.answers(index)

      <.div(
        ^.overflow := "hidden",
        <.div(
          Styles.unselectable,
          ^.float := "left",
          ^.minHeight := "1px",
          ^.border := "1px solid",
          ^.borderRadius := "2px",
          ^.textAlign := "center",
          ^.width := "55px",
          (^.backgroundColor := "#E01010").when(answer.isInvalid),
          ^.onClick --> toggleInvalidAtIndex(highlightedAnswers)(index),
          "Invalid"
        ),
        <.span(
          Styles.bolded.when(isFocused),
          Styles.unselectable,
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          ^.onClick --> scope.modState(State.curQuestion.set(index)),
          questions(index)
        ),
        <.div(
          Styles.answerIndicator,
          Styles.unselectable,
          ^.float := "left",
          ^.minHeight := "1px",
          ^.width := "25px",
          "-->".when(isFocused)
        ),
        <.div(
          ^.float := "left",
          ^.margin := "1px",
          ^.padding := "1px",
          answer match {
            case InvalidQuestion =>
              <.span(
                ^.color := "#CCCCCC",
                "N/A"
              )
            case Answer(spans) if spans.isEmpty && isFocused =>
              <.span(^.color := "#CCCCCC", "Highlight answer above, move with arrow keys or mouse")
            case Answer(spans) if isFocused => // spans nonempty
              (spans.flatMap { span =>
                List(
                  <.span(Text.renderSpan(sentence, span)),
                  <.span(" / ")
                )
              } ++ List(<.span(^.color := "#CCCCCC", "Highlight to add an answer"))).toVdomArray
            case Answer(spans) =>
              spans.map(s => Text.renderSpan(sentence, s)).mkString(" / ")
          }
        )
      )
    }

    def render(state: State) = {
      AjaxFetch.make(
        request = QASRLValidationAjaxRequest(workerIdOpt, prompt.id),
        sendRequest = r => OrWrapped.wrapped(AsyncCallback.fromFuture(makeAjaxRequest(r)))) {
        case AjaxFetch.Loading => <.div("Retrieving data...")
        case AjaxFetch.Loaded(QASRLValidationAjaxResponse(workerInfoSummaryOpt, sentence)) =>
          import state._

          def getRemainingInAgreementGracePeriodOpt(summary: QASRLValidationWorkerInfoSummary) =
            Option(settings.validationAgreementGracePeriod - summary.numAssignmentsCompleted)
              .filter(_ > 0)

          AnswerSelection.make(
            isEnabled = !isNotAssigned && answers(curQuestion).isAnswer,
            enableSpanOverlap = false,
            update = updateCurrentAnswers) {
            case (hs @ AnswerSelection.State(spans, status),
                  AnswerSelection.Context(_, hover, touch, cancelHighlight)
            ) =>
              val curVerbIndex = prompt.qaPairs(curQuestion).verbIndex
              val inProgressAnswerOpt =
                AnswerSelection.Status.selecting.getOption(status).map {
                  case AnswerSelection.Selecting(_, anchor, endpoint) => ISpan(anchor, endpoint)
                }
              val curAnswers = spans(curQuestion)
              val otherAnswers = (spans - curQuestion).values.flatten
              val highlightedAnswers =
                prompt.qaPairs.indices.map(i => i -> Answer(spans(i))).toMap

              val isCurrentInvalid = answers(curQuestion).isInvalid
              val touchWord = touch(curQuestion)
                <.div(
                  ^.classSet1("container-fluid"),
                  ^.onClick --> cancelHighlight,
                  <.div(
                    instructions,
                    ^.margin := "5px"
                  ),
                  workerInfoSummaryOpt.whenDefined(
                    summary =>
                    <.div(
                      ^.classSet1("card"),
                      ^.margin := "5px",
                      <.p(
                        f"""You have marked ${summary.proportionInvalid * 100.0}%.1f%% of questions as invalid.
                               In general, you should expect this to be around 10%%
                               unless you are getting an unusually good set of questions. """,
                        (
                          if (summary.proportionInvalid < 0.6)
                            " Consider being harsher on bad questions if necessary. "
                          else ""
                        )
                      ).when(!summary.proportionInvalid.isNaN),
                      <.p(
                        """Your responses agree with others """,
                        <.span(
                          if (summary.agreement <= settings.validationAgreementBlockingThreshold) {
                            Styles.badRed
                          } else if (summary.agreement <= settings.validationAgreementBlockingThreshold + 0.05) {
                            TagMod(Styles.uncomfortableOrange, Styles.bolded)
                          } else {
                            Styles.goodGreen
                          },
                          f"${summary.agreement * 100.0}%.1f%%"
                        ),
                        f""" of the time. This must remain above ${settings.validationAgreementBlockingThreshold * 100.0}%.1f%%""",
                        getRemainingInAgreementGracePeriodOpt(summary).fold(".")(
                          remaining =>
                          s" after the end of a grace period ($remaining verbs remaining)."
                        )
                      ).when(!summary.agreement.isNaN)
                    )
                  ),
                  <.div(
                    ^.classSet1("card"),
                    ^.margin := "5px",
                    ^.padding := "5px",
                    ^.tabIndex := 0,
                    ^.onFocus --> scope.modState(State.isInterfaceFocused.set(true)),
                    ^.onBlur --> scope.modState(State.isInterfaceFocused.set(false)),
                    ^.onKeyDown ==> (
                      (e: ReactKeyboardEvent) =>
                      handleKey(highlightedAnswers)(e) >> cancelHighlight
                    ),
                    ^.position := "relative",
                    <.div(
                      ^.position := "absolute",
                      ^.top := "20px",
                      ^.left := "0px",
                      ^.width := "100%",
                      ^.height := "100%",
                      ^.textAlign := "center",
                      ^.color := "rgba(48, 140, 20, .3)",
                      ^.fontSize := "48pt",
                      (if (isNotAssigned) "Accept assignment to start"
                       else "Click here to start")
                    ).when(!isInterfaceFocused),
                    MultiContigSpanHighlightableSentence(
                      MultiContigSpanHighlightableSentenceProps(
                        sentence = sentence,
                        styleForIndex = i =>
                        TagMod(Styles.specialWord, Styles.niceBlue).when(i == curVerbIndex),
                        highlightedSpans =
                          (inProgressAnswerOpt.map(_ -> (^.backgroundColor := "#FF8000")) ::
                             (curAnswers.map(_  -> (^.backgroundColor := "#FFFF00")) ++
                                otherAnswers.map(_ -> (^.backgroundColor := "#DDDDDD")))
                             .map(Some(_))).flatten,
                        hover = hover(state.curQuestion),
                        touch = touch(state.curQuestion),
                        render = (
                          elements =>
                          <.p(Styles.largeText, Styles.unselectable, elements.toVdomArray)
                        )
                      )
                    ),
                    <.ul(
                      ^.classSet1("list-unstyled"),
                      (0 until questions.size).toVdomArray { index =>
                        <.li(
                          ^.key := s"question-$index",
                          ^.display := "block",
                          qaField(state, sentence, highlightedAnswers)(index)
                        )
                      }
                    ),
                    <.p(
                      s"Bonus: ${dollarsToCents(settings.validationBonus(questions.size))}c"
                    )
                  ),
                  <.div(
                    ^.classSet1("form-group"),
                    ^.margin := "5px",
                    <.textarea(
                      ^.classSet1("form-control"),
                      ^.name := FieldLabels.feedbackLabel,
                      ^.rows := 3,
                      ^.placeholder := "Feedback? (Optional)"
                    )
                  ),
                  <.input(
                    ^.classSet1("btn btn-primary btn-lg btn-block"),
                    ^.margin := "5px",
                    ^.`type` := "submit",
                    ^.disabled := !state.answers.forall(_.isComplete),
                    ^.id := FieldLabels.submitButtonLabel,
                    ^.value := (
                      if (isNotAssigned) "You must accept the HIT to submit results"
                      else if (!state.answers.forall(_.isComplete))
                        "You must respond to all questions to submit results"
                      else "Submit"
                    )
                  )
                )
          }
      }
    }
  }

  val FullUI = ScalaComponent
    .builder[Unit]("Full UI")
    .initialState(State.initial)
    .renderBackend[FullUIBackend]
    .componentDidUpdate(_.backend.updateResponse)
    .build

}
