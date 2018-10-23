package qasrl.labeling

import qasrl._
import qasrl.util._
import qasrl.util.implicits._

import cats.Id
import cats.Functor
import cats.arrow.Arrow
import cats.data.NonEmptyList
import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.datasets.wiktionary.VerbForm
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

import scala.util.Try

// for accordance with original QA-SRL format
case class SlotBasedLabel[A](
  wh: LowerCaseString,
  aux: Option[LowerCaseString],
  subj: Option[LowerCaseString],
  verbPrefix: List[LowerCaseString],
  verb: A,
  obj: Option[LowerCaseString],
  prep: Option[LowerCaseString],
  obj2: Option[LowerCaseString]
) {

  def slotStrings(renderVerb: A => LowerCaseString) = List(
    wh,
    aux.getOrElse("_".lowerCase),
    subj.getOrElse("_".lowerCase),
    (verbPrefix ++ List(renderVerb(verb))).mkString(" ").lowerCase,
    obj.getOrElse("_".lowerCase),
    prep.getOrElse("_".lowerCase),
    obj2.getOrElse("_".lowerCase)
  )

  def renderWithSeparator(renderVerb: A => LowerCaseString, sep: String) =
    slotStrings(renderVerb).mkString(sep)

  private[this] def slotTokens(renderVerb: A => LowerCaseString) =
    List(
      List(wh),
      aux.toList,
      subj.toList,
      verbPrefix,
      List(renderVerb(verb)),
      obj.toList,
      prep.toList,
      obj2.toList
    ).flatten.filter(_.toString.nonEmpty)

  def renderQuestionString(renderVerb: A => LowerCaseString): String =
    Text.render(slotTokens(renderVerb).map(_.toString)).capitalize + "?"

}

object SlotBasedLabel {

  implicit val slotBasedLabelFunctor: Functor[SlotBasedLabel] = new Functor[SlotBasedLabel] {
    override def map[A, B](fa: SlotBasedLabel[A])(f: A => B): SlotBasedLabel[B] =
      fa.copy(verb = f(fa.verb))
  }

  def fromRenderedString[A](readVerb: LowerCaseString => A, sep: String)(str: String) = Try {
    val fields = str.split(sep)
    def readField(i: Int) =
      if (fields(i) == "_" || fields(i).isEmpty) None else Some(fields(i).lowerCase)
    val (verbPrefix, verb) = {
      val verbWords = fields(3).split(" ").toList.map(_.lowerCase)
      (verbWords.init, readVerb(verbWords.last))
    }
    SlotBasedLabel(
      wh = readField(0).get,
      aux = readField(1),
      subj = readField(2),
      verbPrefix = verbPrefix,
      verb = verb,
      obj = readField(4),
      prep = readField(5),
      obj2 = readField(6)
    )
  }

  val mainAuxVerbs = {
    val negContractibleAuxes = Set(
      "has",
      "had",
      "might",
      "would",
      "should",
      "does",
      "did",
      "is",
      "was"
    )
    val allAuxes = negContractibleAuxes ++
    negContractibleAuxes.map(_ + "n't") ++
    Set(
      "can",
      "will",
      "can't",
      "won't"
    )
    allAuxes.map(_.lowerCase)
  }

  def getSlotsForQuestionStructure(frame: Frame, answerSlot: ArgumentSlot): SlotBasedLabel[LowerCaseString] = {
    val wh = {
      val whStr = answerSlot match {
        case Subj => if (frame.args.get(Subj).get.isAnimate) "who" else "what"
        case Obj  => if (frame.args.get(Obj).get.isAnimate) "who" else "what"
        case Obj2 =>
          frame.args.get(Obj2).get match {
            case Noun(isAnimate)                => if (isAnimate) "who" else "what"
            case Prep(_, Some(Noun(isAnimate))) => if (isAnimate) "who" else "what"
            case Locative                       => "where"
            case _                              => "what" // extra case for objless prep; shouldn't happen
          }
        case Adv(wh) => wh.toString
      }
      whStr.lowerCase
    }
    val subj = {
      if (answerSlot == Subj) None
      else
        frame.args.get(Subj).map {
          case Noun(isAnimate) =>
            (if (isAnimate) "someone" else "something").lowerCase
        }
    }
    val verbStack = if (subj.isEmpty) {
      frame.getVerbStack.map(_.lowerCase)
    } else {
      frame.splitVerbStackIfNecessary(frame.getVerbStack).map(_.lowerCase)
    }
    val (aux, verbTokens) = NonEmptyList
      .fromList(verbStack.tail)
      .filter(_ => mainAuxVerbs.contains(verbStack.head)) match {
      case None       => None                 -> verbStack
      case Some(tail) => Some(verbStack.head) -> tail
    }
    val verbPrefix = verbTokens.init
    val verb = verbTokens.last

    val obj = {
      if (answerSlot == Obj) None
      else
        frame.args.get(Obj).map {
          case Noun(isAnimate) =>
            (if (isAnimate) "someone" else "something").lowerCase
        }
    }

    val (prep, obj2) = {
      frame.args
        .get(Obj2)
        .fold(Option.empty[LowerCaseString] -> Option.empty[LowerCaseString]) {
        arg =>
        if (answerSlot == Obj2) arg match {
          case Noun(isAnimate) => None -> None
          case Prep(preposition, _) =>
            val vec = preposition.split(" ").toVector
            if (vec.last == "do" || vec.last == "doing") {
              Some(vec.init.mkString(" ").lowerCase) -> Some(vec.last.lowerCase)
            } else Some(preposition) -> None
          case Locative => None -> None
        } else
          arg match {
            case Noun(isAnimate) =>
              None -> Some(
                (if (isAnimate) "someone" else "something").lowerCase
              )
            case Prep(preposition, Some(Noun(isAnimate))) =>
              val vec = preposition.split(" ").toVector.init
              if (vec.size > 0 && (vec.last == "do" || vec.last == "doing")) {
                Some(vec.init.mkString(" ").lowerCase) -> Some(
                  (vec.last + " something").lowerCase
                )
              } else
                Some(preposition) -> Some(
                  (if (isAnimate) "someone" else "something").lowerCase
                )
            case Prep(preposition, None) =>
              Some(preposition) -> None
            case Locative => None -> Some("somewhere".lowerCase)
          }
      }
    }
    SlotBasedLabel(wh, aux, subj, verbPrefix, verb, obj, prep, obj2)
  }

  val getSlotsForQuestion = QuestionLabelMapper(
    (
      sentenceTokens: Vector[String],
      verbInflectedForms: InflectedForms,
      questions: List[String]
    ) => {
      questions.flatMap {
        question =>
          val questionTokensIsh = question.init.split(" ").toVector.map(_.lowerCase)
          val qPreps = questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
          val qPrepBigrams = questionTokensIsh
            .sliding(2)
            .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
            .map(_.mkString(" ").lowerCase)
            .toSet
          val stateMachine = new TemplateStateMachine(
            sentenceTokens,
            verbInflectedForms,
            Some(qPreps ++ qPrepBigrams)
          )
          val template = new QuestionProcessor(stateMachine)
          val resOpt = template.processStringFully(question) match {
            case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => None
            case Right(goodStates) =>
              goodStates.toList
                .collect { case QuestionProcessor.CompleteState(_, frame, answerSlot) =>
                  getSlotsForQuestionStructure(frame, answerSlot) }
                .toSet
                .headOption
          }
          resOpt.map(question -> _)
      }.toMap: Map[String, SlotBasedLabel[LowerCaseString]]
    }
  )

  val instantiateVerbForTenseSlots = QuestionLabelMapper.liftWithContext(
    (_: Vector[String], verbInflectedForms: InflectedForms, slots: SlotBasedLabel[VerbForm]) =>
      slots.map(verbInflectedForms)
  )

  val getVerbTenseAbstractedSlotsForQuestion = (
    Arrow[QuestionLabelMapper].id merge getSlotsForQuestion
  ) >>> QuestionLabelMapper.liftOptionalWithContext(
    (
      sentenceTokens: Vector[String],
      verbInflectedForms: InflectedForms,
      pair: (String, SlotBasedLabel[LowerCaseString])
    ) => {
      val questionTokensIsh = pair._1.init.split(" ").toVector.map(_.lowerCase)
      val qPreps = questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
      val qPrepBigrams = questionTokensIsh
        .sliding(2)
        .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
        .map(_.mkString(" ").lowerCase)
        .toSet
      val stateMachine =
        new TemplateStateMachine(sentenceTokens, verbInflectedForms, Some(qPreps ++ qPrepBigrams))
      val template = new QuestionProcessor(stateMachine)
      val (question, rawSlots) = pair
      val frameWithAnswerSlotOpt = template.processStringFully(question) match {
        case Left(QuestionProcessor.AggregatedInvalidState(_, _)) => None
        case Right(goodStates) =>
          goodStates.toList.collect {
            case QuestionProcessor.CompleteState(_, frame, answerSlot) => frame -> answerSlot
          }.headOption
      }
      frameWithAnswerSlotOpt.map {
        case (firstFrame, answerSlot) =>
          rawSlots.copy(
            verb =
              firstFrame.getVerbConjugation(firstFrame.args.get(Subj).isEmpty || answerSlot != Subj)
          )
      }
    }
  )
}
