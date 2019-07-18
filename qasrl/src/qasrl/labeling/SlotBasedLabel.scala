package qasrl.labeling

import qasrl._

import jjm.LowerCaseString
import jjm.ling.Text
import jjm.ling.en._
import jjm.implicits._

import cats.Id
import cats.Functor
import cats.arrow.Arrow
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Try

import io.circe.{Encoder, Decoder}
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.Json

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

  def verbInfoFromString(s: String): Decoder.Result[(List[LowerCaseString], VerbForm)] = {
    val vec = s.split(" ").toVector
    val prefix = vec.init.toList.map(_.lowerCase)
    val verbFormResult = vec.last match {
      case "stem"               => Right(VerbForm.Stem)
      case "presentSingular3rd" => Right(VerbForm.PresentSingular3rd)
      case "presentParticiple"  => Right(VerbForm.PresentParticiple)
      case "past"               => Right(VerbForm.Past)
      case "pastParticiple"     => Right(VerbForm.PastParticiple)
      case x                    => Left(DecodingFailure(s"Invalid string for verb form: $x", Nil))
    }
    verbFormResult.right.map(prefix -> _)
  }

  implicit val slotBasedLabelWithVerbFormEncoder: Encoder[SlotBasedLabel[VerbForm]] =
    new Encoder[SlotBasedLabel[VerbForm]] {
      final def apply(label: SlotBasedLabel[VerbForm]): Json = Json.obj(
        "wh"   -> Json.fromString(label.wh),
        "aux"  -> Json.fromString(label.aux.fold("_")(_.toString)),
        "subj" -> Json.fromString(label.subj.fold("_")(_.toString)),
        "verb" -> Json.fromString(
          (label.verbPrefix ++ List(label.verb.toString)).mkString(" ")
        ),
        "obj"  -> Json.fromString(label.obj.fold("_")(_.toString)),
        "prep" -> Json.fromString(label.prep.fold("_")(_.toString)),
        "obj2" -> Json.fromString(label.obj2.fold("_")(_.toString))
      )
    }

  val readOptionalSlotString = (s: String) => if (s == "_") None else Some(s.lowerCase)

  implicit val slotBasedLabelWithVerbFormDecoder: Decoder[SlotBasedLabel[VerbForm]] =
    new Decoder[SlotBasedLabel[VerbForm]] {
      final def apply(c: HCursor): Decoder.Result[SlotBasedLabel[VerbForm]] =
        for {
          wh                <- c.downField("wh").as[String].right.map(_.lowerCase).right
          aux               <- c.downField("aux").as[String].right.map(readOptionalSlotString).right
          subj              <- c.downField("subj").as[String].right.map(readOptionalSlotString).right
          verbString        <- c.downField("verb").as[String].right
          verbPrefixAndForm <- verbInfoFromString(verbString).right
          obj               <- c.downField("obj").as[String].right.map(readOptionalSlotString).right
          prep              <- c.downField("prep").as[String].right.map(readOptionalSlotString).right
          obj2              <- c.downField("obj2").as[String].right.map(readOptionalSlotString).right
        } yield
          SlotBasedLabel(wh, aux, subj, verbPrefixAndForm._1, verbPrefixAndForm._2, obj, prep, obj2)
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

    def getPrepAndMiscPrefix(prepString: LowerCaseString) = {
      val prepTokens = prepString.split(" ").toList
      prepTokens.reverse match {
        case (doWord @ ("do"|"doing")) :: "to" :: rest =>
          val prep = if(rest.isEmpty) None else Some(rest.reverse.mkString(" ").lowerCase)
          val miscPrefix = Some(s"to $doWord".lowerCase)
          prep -> miscPrefix
        case (doWord @ ("do"|"doing")) :: rest =>
          val prep = if(rest.isEmpty) None else Some(rest.reverse.mkString(" ").lowerCase)
          val miscPrefix = Some(doWord.lowerCase)
          prep -> miscPrefix
        case _ => Some(prepString) -> None
      }
    }

    val (prep, obj2) = {
      frame.args
        .get(Obj2)
        .fold(Option.empty[LowerCaseString] -> Option.empty[LowerCaseString]) {
        arg =>
        if (answerSlot == Obj2) arg match {
          case Noun(isAnimate) => None -> None
          case Prep(preposition, _) => getPrepAndMiscPrefix(preposition)
          case Locative => None -> None
        } else
          arg match {
            case Noun(isAnimate) =>
              None -> Some((if (isAnimate) "someone" else "something").lowerCase)
            case Prep(preposition, Some(Noun(isAnimate))) =>
              val (_prep, miscPrefix) = getPrepAndMiscPrefix(preposition)
              val miscObj = if(isAnimate) "someone" else "something"
              val _misc = Some((miscPrefix.fold("")(_.toString + " ") + miscObj).lowerCase)
              _prep -> _misc
            case Prep(preposition, None) =>
              Some(preposition) -> None
            case Locative => None -> Some("somewhere".lowerCase)
          }
      }
    }
    SlotBasedLabel(wh, aux, subj, verbPrefix, verb, obj, prep, obj2)
  }

  def getPreferredCompleteState(states: NonEmptyList[QuestionProcessor.ValidState]) = {
    // prioritize a frame with Obj (but no Obj2) over one with Obj2 (but no Obj)
    val completeStates = states.collect { case cs @ QuestionProcessor.CompleteState(_, _, _) => cs }
    completeStates.find(_.frame.args.get(Obj).nonEmpty).orElse(completeStates.headOption)
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
              val completeStateOpt = getPreferredCompleteState(goodStates)
              completeStateOpt.map(s =>
                getSlotsForQuestionStructure(s.frame, s.answerSlot)
              )
          }
          resOpt.map(question -> _)
      }.toMap: Map[String, SlotBasedLabel[LowerCaseString]]
    }
  )

  val instantiateVerbForTenseSlots = QuestionLabelMapper.liftWithContext(
    (_: Vector[String], verbInflectedForms: InflectedForms, slots: SlotBasedLabel[VerbForm]) =>
      slots.map(verbInflectedForms.apply)
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
        case Right(goodStates) => getPreferredCompleteState(goodStates)
      }
      frameWithAnswerSlotOpt.map {
        case QuestionProcessor.CompleteState(_, frame, answerSlot) =>
          rawSlots.copy(
            verb = frame.getVerbConjugation(frame.args.get(Subj).isEmpty || answerSlot != Subj)
          )
      }
    }
  )
}
