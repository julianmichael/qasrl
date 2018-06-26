package qasrl.labeling

import qasrl._
import qasrl.util._

import cats.Id
import cats.implicits._
import cats.arrow.Arrow
import cats.data.NonEmptyList

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

sealed trait DiscreteLabel {
  def label: LowerCaseString

  def render: String = this match {
    case NounRole(label, isAnimate) =>
      val animacyMarker = if(isAnimate) "+" else "-"
      s"$label/$animacyMarker".lowerCase
    case AdvRole(label) => label
  }
}
case class NounRole(override val label: LowerCaseString, isAnimate: Boolean) extends DiscreteLabel
case class AdvRole(override val label: LowerCaseString) extends DiscreteLabel

object DiscreteLabel {

  def fromRenderedString(s: LowerCaseString): Either[String, DiscreteLabel] = {
    if(advDiscreteLabels.contains(s)) Right(AdvRole(s))
    else if(!s.contains("/".lowerCase)) Left(s"Invalid adverbial label: $s")
    else {
      val getField = s.split("/").lift
      for {
        label <- getField(0).toRight(s"Could not read noun label before slash: $s")
        animacyMarker <- getField(1).toRight(s"Could not read animacy marker after slash: $s")
        animacy <- animacyMarker match {
          case "+" => Right(true)
          case "-" => Right(false)
          case x => Left(s"Invalid animacy marker (must be + or -): $animacyMarker")
        }
      } yield NounRole(label.lowerCase, animacy)
    }
  }

  val coreArgDiscreteLabels = Set(
    "subj-transitive", "subj-intransitive", "obj", "obj-dative"
  ).map(_.lowerCase)

  val advDiscreteLabels = Set(
    "when", "where", "why", "how", "how much", "how long"
  ).map(_.lowerCase)

  // TODO test: nonempty for good QA-SRL questions
  val getAllDiscreteLabels: QuestionLabelMapper[String, NonEmptyList[DiscreteLabel]] =
    QuestionLabelMapper[String, NonEmptyList[DiscreteLabel]](
      (sentenceTokens: Vector[String],
       verbInflectedForms: InflectedForms,
       questions: List[String]
      ) => questions.flatMap { question =>
        val questionTokensIsh = question.init.split(" ").toVector.map(_.lowerCase)
        val qPreps = questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
        val qPrepBigrams = questionTokensIsh.sliding(2)
          .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
          .map(_.mkString(" ").lowerCase)
          .toSet
        val stateMachine = new TemplateStateMachine(sentenceTokens, verbInflectedForms, Some(qPreps ++ qPrepBigrams))
        val template = new QuestionProcessor(stateMachine)
        template.processStringFully(question) match {
          case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => None
          case Right(goodStates) =>
            val framesWithAnswerSlots = goodStates.toList.collect {
              case QuestionProcessor.CompleteState(_, frame, answerSlot) => (frame, answerSlot)
            }.toSet
            val labels = framesWithAnswerSlots.toList.map {
              case (frame, Adv(whWord)) => AdvRole(whWord)
              case (frame, Obj2) => frame.args.get(Obj2).get match {
                case Prep(preposition, prepObj) =>
                  val isObjAnimate = prepObj match {
                    case Some(Noun(isAnimate)) => isAnimate
                    case None =>
                      System.err.println("Answer prep obj not present in frame?")
                      false
                  }
                  NounRole(
                    (if(frame.isPassive && preposition == "by".lowerCase) {
                       "subj-transitive".lowerCase
                     } else preposition),
                    isObjAnimate)
                case Noun(isAnimate) =>
                  NounRole(
                    (if(isAnimate) "obj-dative" else "obj").lowerCase, // obj/2 ambiguous
                    isAnimate)
                case Locative => AdvRole("where".lowerCase)
              }
              case (frame, Obj) => frame.args.get(Obj).get match {
                case Noun(isAnimate) =>
                  if(frame.args.get(Obj2).exists(_.isNoun) || frame.isPassive) { // obj/2 ambiguous
                    NounRole((if(isAnimate) "obj-dative" else "obj").lowerCase, isAnimate)
                  } else NounRole("obj".lowerCase, isAnimate)
              }
              case (frame, Subj) => frame.args.get(Subj).get match {
                case Noun(isAnimate) =>
                  NounRole(
                    (if(frame.isPassive) { // obj/2 ambiguous
                      (if(frame.args.get(Obj).nonEmpty) "obj-dative" else "obj").lowerCase
                    } else if(frame.args.get(Obj).nonEmpty) {
                      "subj-transitive".lowerCase
                    } else "subj-intransitive".lowerCase),
                    isAnimate
                  )
              }
            }
            // TODO why was I doing this again?
            // val res = labels.toSet.flatMap((label: LowerCaseString) =>
            //   if(stateMachine.prepositionBigrams.contains(label)) {
            //     label.split(" ").map(_.lowerCase).toSet
            //   } else Set(label)
            // )
            NonEmptyList.fromList(labels).map(question -> _)
        }
      }.toMap: Map[String, NonEmptyList[DiscreteLabel]]
    )

  // TODO test: should always return a result for a properly QA-SRL formatted question
  val getDiscreteLabels = getAllDiscreteLabels >>> Arrow[QuestionLabelMapper].lift(
    (labels: NonEmptyList[DiscreteLabel]) => {
      val objLabels = Set("obj", "obj-dative")
      // prefer the prep as the label when ambiguous between prep and object, at least in these caes
      labels.find(x => !objLabels.contains(x.label))
        .filter(_ => labels.size == 2 && labels.exists(x => objLabels.contains(x.label)))
        .getOrElse(labels.head)
    }
  )

  // TODO test: round-trip characteristics
  // val getQuestionsForDiscreteLabels = QuestionLabelMapper[DiscreteRoleLabel, LowerCaseString](
  //   (sentenceTokens: Vector[String],
  //    verbInflectedForms: InflectedForms,
  //    discreteLabels: List[DiscreteRoleLabel]
  //   ) => {
  //     val givenLabelSet = discreteLabels.map(_.value).toSet
  //     val coreNounArgs = coreArgDiscreteLabels.filter(givenLabelSet.contains)
  //     val advArgs = advDiscreteLabels.filter(givenLabelSet.contains)
  //     val prepArgs = givenLabelSet -- coreNounArgs -- advArgs

  //     val initFrame = Frame(
  //       verbInflectedForms,
  //       DependentMap.empty[ArgumentSlot.Aux, Id],
  //       PastTense,
  //       false, false, false, false)

  //     discreteLabels.map(_.value.toString).map { label =>
  //       val question = label match {
  //         case "subj-intransitive" =>
  //           val theseArgs = List(
  //             Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
  //             prepArgs.headOption.map(prep =>
  //               DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
  //             )
  //           ).flatten
  //           initFrame.copy(
  //             args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
  //           ).questionsForSlot(Subj).head
  //         case "subj-transitive" | "obj" =>
  //           val argSlot = if(label == "subj-transitive") Subj else Obj
  //           val theseArgs = List(
  //             Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
  //             Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(false))),
  //             prepArgs.headOption.map(prep =>
  //               DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
  //             )
  //           ).flatten
  //           initFrame.copy(
  //             args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
  //           ).questionsForSlot(argSlot).head
  //         case "obj-dative" =>
  //           val theseArgs = List(
  //             Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
  //             Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(true))),
  //             Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Noun(false)))
  //           ).flatten
  //           initFrame.copy(
  //             args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _)
  //           ).questionsForSlot(Obj).head
  //         case adv if advDiscreteLabels.contains(adv.lowerCase) =>
  //           val theseArgs = if(givenLabelSet.contains("subj-transitive".lowerCase)) {
  //             List(
  //               Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
  //               Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(false))),
  //               prepArgs.headOption.map(prep =>
  //                 DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
  //               )
  //             ).flatten
  //           } else {
  //             List(
  //               Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
  //               prepArgs.headOption.map(prep =>
  //                 DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep, Some(Noun(false))))
  //               )
  //             ).flatten
  //           }

  //           val newFrameIsPassive = givenLabelSet.contains("obj".lowerCase) &&
  //             !givenLabelSet.contains("subj-intransitive".lowerCase) &&
  //             !givenLabelSet.contains("subj-transitive".lowerCase)

  //           initFrame.copy(
  //             args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _),
  //             isPassive = newFrameIsPassive
  //           ).questionsForSlot(Adv(adv.lowerCase)).head
  //         case prep => // all others are prepositions
  //           val theseArgs = if(givenLabelSet.contains("subj-transitive".lowerCase)) {
  //             List(
  //               Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
  //               Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Obj, Noun(true))),
  //               Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep.lowerCase, Some(Noun(false)))))
  //             ).flatten
  //           } else {
  //             List(
  //               Option(DependentPair[ArgumentSlot.Aux, Id, Noun](Subj, Noun(false))),
  //               Option(DependentPair[ArgumentSlot.Aux, Id, Argument](Obj2, Prep(prep.lowerCase, Some(Noun(false)))))
  //             ).flatten
  //           }

  //           val newFrameIsPassive = givenLabelSet.contains("obj".lowerCase) &&
  //             !givenLabelSet.contains("subj-intransitive".lowerCase)
  //           initFrame.copy(
  //             args = theseArgs.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id])(_ put _),
  //             isPassive = newFrameIsPassive
  //           ).questionsForSlot(Obj2).head
  //       }
  //       DiscreteRoleLabel(label.lowerCase) -> question.lowerCase
  //     }.toMap
  //   }
  // )

}
