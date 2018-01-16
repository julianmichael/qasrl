package qasrl.labeling

import qasrl._
import qasrl.util._

import cats.Id
import cats.implicits._
import cats.arrow.Arrow
import cats.data.NonEmptyList

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

case class DiscreteRoleLabel(value: LowerCaseString)
object DiscreteRoleLabel {
  val coreArgDiscreteLabels = Set(
    "subj-transitive", "subj-intransitive", "obj", "obj-dative"
  ).map(_.lowerCase)

  val advDiscreteLabels = Set(
    "when", "where", "why", "how", "how much", "how long"
  ).map(_.lowerCase)

  // TODO test: nonempty for good QA-SRL questions
  val getAllDiscreteRoleLabels: QuestionLabelMapper[String, NonEmptyList[DiscreteRoleLabel]] =
    QuestionLabelMapper[String, NonEmptyList[DiscreteRoleLabel]](
      (sentenceTokens: Vector[String],
       verbInflectedForms: InflectedForms,
       questions: List[String]
      ) => questions.flatMap { question =>
        val stateMachine = new TemplateStateMachine(sentenceTokens, verbInflectedForms)
        val template = new QuestionProcessor(stateMachine)
        template.processStringFully(question) match {
          case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => None
          case Right(goodStates) =>
            val framesWithAnswerSlots = goodStates.toList.collect {
              case QuestionProcessor.CompleteState(_, frame, answerSlot) => (frame, answerSlot)
            }.toSet
            val labels = framesWithAnswerSlots.toList.map {
              case (frame, Adv(whWord)) => whWord
              case (frame, Obj2) => frame.args.get(Obj2).get match {
                case Prep(preposition, _) =>
                  if(frame.isPassive && preposition == "by".lowerCase) "subj-transitive".lowerCase
                  else preposition
                case Noun(isAnimate) => (if(isAnimate) "obj-dative" else "obj").lowerCase // obj/2 ambiguous
                case Locative => "where".lowerCase
              }
              case (frame, Obj) => frame.args.get(Obj).get match {
                case Noun(isAnimate) =>
                  if(frame.args.get(Obj2).exists(_.isNoun) || frame.isPassive) { // obj/2 ambiguous
                    (if(isAnimate) "obj-dative" else "obj").lowerCase
                  } else "obj".lowerCase
              }
              case (frame, Subj) => frame.args.get(Subj).get match {
                case Noun(isAnimate) =>
                  if(frame.isPassive) { // obj/2 ambiguous
                    (if(frame.args.get(Obj).nonEmpty) "obj-dative" else "obj").lowerCase
                  } else if(frame.args.get(Obj).nonEmpty) {
                    "subj-transitive".lowerCase
                  } else "subj-intransitive".lowerCase
              }
            }
            val res = labels.toSet.flatMap((label: LowerCaseString) =>
              if(stateMachine.prepositionBigrams.contains(label)) {
                label.split(" ").map(_.lowerCase).toSet
              } else Set(label)
            )
            NonEmptyList.fromList(res.toList.map(DiscreteRoleLabel(_))).map(question -> _)
        }
      }.toMap: Map[String, NonEmptyList[DiscreteRoleLabel]]
    )

  // TODO test: should always return a result for a properly QA-SRL formatted question
  val getDiscreteRoleLabels = getAllDiscreteRoleLabels >>> Arrow[QuestionLabelMapper].lift(
    (labels: NonEmptyList[DiscreteRoleLabel]) => {
      val objs = Set("obj", "obj-dative").map(s => DiscreteRoleLabel(s.lowerCase))
      // prefer the prep as the label when ambiguous between prep and object, at least in these caes
      labels.find(x => !objs.contains(x))
        .filter(_ => labels.size == 2 && labels.exists(objs.contains))
        .getOrElse(labels.head)
    }
  )

  // TODO test: round-trip characteristics
  val getQuestionsForDiscreteLabels = QuestionLabelMapper[DiscreteRoleLabel, LowerCaseString](
    (sentenceTokens: Vector[String],
     verbInflectedForms: InflectedForms,
     discreteLabels: List[DiscreteRoleLabel]
    ) => {
      val givenLabelSet = discreteLabels.map(_.value).toSet
      val coreNounArgs = coreArgDiscreteLabels.filter(givenLabelSet.contains)
      val advArgs = advDiscreteLabels.filter(givenLabelSet.contains)
      val prepArgs = givenLabelSet -- coreNounArgs -- advArgs

      val initFrame = Frame(
        verbInflectedForms,
        DependentMap.empty[ArgumentSlot.Aux, Id],
        PastTense,
        false, false, false, false)

      discreteLabels.map(_.value.toString).map { label =>
        val question = label match {
          case "subj-intransitive" =>
            val theseArgs = List(
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
              prepArgs.headOption.map(prep =>
                DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
              )
            ).flatten
            initFrame.copy(
              args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
            ).questionsForSlot(Subj).head
          case "subj-transitive" | "obj" =>
            val argSlot = if(label == "subj-transitive") Subj else Obj
            val theseArgs = List(
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(false))),
              prepArgs.headOption.map(prep =>
                DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
              )
            ).flatten
            initFrame.copy(
              args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
            ).questionsForSlot(argSlot).head
          case "obj-dative" =>
            val theseArgs = List(
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
              Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(true))),
              Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Noun(false)))
            ).flatten
            initFrame.copy(
              args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
            ).questionsForSlot(Obj).head
          case adv if advDiscreteLabels.contains(adv.lowerCase) =>
            val theseArgs = if(givenLabelSet.contains("subj-transitive".lowerCase)) {
              List(
                Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
                Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(false))),
                prepArgs.headOption.map(prep =>
                  DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
                )
              ).flatten
            } else {
              List(
                Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
                prepArgs.headOption.map(prep =>
                  DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
                )
              ).flatten
            }

            val newFrameIsPassive = givenLabelSet.contains("obj".lowerCase) &&
              !givenLabelSet.contains("subj-intransitive".lowerCase) &&
              !givenLabelSet.contains("subj-transitive".lowerCase)

            initFrame.copy(
              args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _),
              isPassive = newFrameIsPassive
            ).questionsForSlot(Adv(adv.lowerCase)).head
          case prep => // all others are prepositions
            val theseArgs = if(givenLabelSet.contains("subj-transitive".lowerCase)) {
              List(
                Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
                Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(true))),
                Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep.lowerCase, Some(Noun(false)))))
              ).flatten
            } else {
              List(
                Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
                Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep.lowerCase, Some(Noun(false)))))
              ).flatten
            }

            val newFrameIsPassive = givenLabelSet.contains("obj".lowerCase) &&
              !givenLabelSet.contains("subj-intransitive".lowerCase)
            initFrame.copy(
              args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _),
              isPassive = newFrameIsPassive
            ).questionsForSlot(Obj2).head
        }
        DiscreteRoleLabel(label.lowerCase) -> question.lowerCase
      }.toMap
    }
  )

}
