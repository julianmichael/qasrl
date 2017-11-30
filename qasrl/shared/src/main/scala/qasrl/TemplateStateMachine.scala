package qasrl

import qasrl.util.implicits._

import cats.data.NonEmptyList
import cats.data.StateT
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary.InflectedForms

import monocle.macros._

object TemplateStateMachine {

  @Lenses case class FrameState(
    whWord: Option[LowerCaseString],
    preposition: Option[LowerCaseString],
    answerSlot: Option[ArgumentSlot],
    frame: Frame)

  object FrameState {
    def initial(verbForms: InflectedForms) = FrameState(None, None, None, Frame.empty(verbForms))
  }

  sealed trait TemplateState
  case object TemplateComplete extends TemplateState
  case class TemplateProgress(
    transitions: NonEmptyList[TemplateTransition]
  ) extends TemplateState

  // type TemplatingOp[A] = StateT[Option, FrameState, A]

  type TemplateTransition = (String, StateT[Option, FrameState, TemplateState])

  def pure[A](a: A) = StateT.pure[Option, FrameState, A](a)
  def abort[A] = StateT.lift[Option, FrameState, A](None)
  def get = StateT.get[Option, FrameState]
  def set(fs: FrameState) = StateT.set[Option, FrameState](fs)
  def modify(f: FrameState => FrameState) = StateT.modify[Option, FrameState](f)
  def modFrame(f: Frame => Frame) = StateT.modify[Option, FrameState](FrameState.frame.modify(f))
  def lift[A](aOpt: Option[A]) = StateT.lift[Option, FrameState, A](aOpt)

  def progress(first: TemplateTransition, rest: TemplateTransition*) = TemplateProgress(
    NonEmptyList.of(first, rest: _*)
  )

  def markPlaceholderSlot[A](slot: ArgumentSlot.Aux[A], arg: A) = for {
    fs <- get
    newFrame <- lift(
      fs.frame.args.get(slot).ifEmpty(Frame.args.modify(_.put(slot, arg))(fs.frame))
    )
    _ <- set(fs.copy(frame = newFrame))
  } yield ()

  def markAnswerSlot[A](slot: ArgumentSlot.Aux[A], makeArg: LowerCaseString => StateT[Option, FrameState, A]) = for {
    fs <- get
    _ <- lift(fs.answerSlot.ifEmpty(())) // only works if we don't already have an answer
    whWord <- lift(fs.whWord)
    arg <- makeArg(whWord)
    newFrame <- lift(
      fs.frame.args.get(slot).ifEmpty(
        Frame.args.modify(_.put(slot, arg))(fs.frame)
      ))
    _ <- set(fs.copy(frame = newFrame, answerSlot = Some(slot)))
  } yield ()

  // neither of these should contain "to", which is handled specially

  val mostCommonPrepositions = Set(
    "by", "for", "with",
    // "about", // too many spurious questions from this
    "in", "from", "to", "as" // added my own on this line
  ).map(_.lowerCase)

  val lotsOfPrepositions = Set(
		"aboard", "about", "above", "across", "afore", "after", "against", "ahead", "along", "alongside", "amid",
		"amidst", "among", "amongst", "around", "as", "aside", "astride", "at", "atop", "before",
		"behind", "below", "beneath", "beside", "besides", "between", "beyond", "by", "despite", "down",
		"during", "except", "for", "from", "given", "in", "inside", "into", "near", "next",
		"of", "off", "on", "onto", "opposite", "out", "outside", "over", "pace", "per",
		"round", "since", "than", "through", "throughout", "till", "times", "to", "toward", "towards",
		"under", "underneath", "until", "unto", "up", "upon", "versus", "via", "with ", "within",
		"without"
  ).map(_.lowerCase)
}

class TemplateStateMachine(
  tokens: Vector[String],
  verbInflectedForms: InflectedForms) {

  import TemplateStateMachine._

  val initialFrameState = FrameState.initial(verbInflectedForms)

  // process prepositions
  val lowerTokens = tokens.map(_.lowerCase)
  def isPreposition(lcs: LowerCaseString): Boolean = TemplateStateMachine.lotsOfPrepositions.contains(lcs)
  val newPrepositions = lowerTokens.filter(isPreposition)
  val prepositionBigrams = lowerTokens.sliding(2)
    .filter(_.forall(isPreposition))
    .map(_.mkString(" ").lowerCase)

  implicit val lcsOrder = cats.Order.by[LowerCaseString, String](_.toString)

  val allChosenPrepositions: Set[LowerCaseString] = (
    newPrepositions.iterator ++
      prepositionBigrams ++
      TemplateStateMachine.mostCommonPrepositions.iterator
  ).toSet

  val nonToEndingPrepositions = NonEmptyList.of(
    TemplateStateMachine.mostCommonPrepositions.head,
    allChosenPrepositions.filterNot(_.endsWith("to".lowerCase)).toSeq: _*
  ).distinct

  val toEndingPrepositions = NonEmptyList.of(
    "to".lowerCase,
    allChosenPrepositions.filter(_.endsWith("to".lowerCase)).toSeq: _*
  ).distinct

  import verbInflectedForms._

  val qMark = progress("?" -> pure(TemplateComplete))

  def makeObj2ForWh(wh: LowerCaseString): StateT[Option, FrameState, Argument] =
    lift(
      if(wh == "who".lowerCase) Some(Noun(true))
      else if(wh == "what".lowerCase) Some(Noun(false))
      else if(wh == "where".lowerCase) Some(Locative)
      else None
    )

  val noPrepObj = progress(
    "" -> markAnswerSlot(Obj2, makeObj2ForWh).as(qMark),
    " someone" -> markPlaceholderSlot(Obj2, Noun(isAnimate = true)).as(qMark),
    " something" -> markPlaceholderSlot(Obj2, Noun(isAnimate = false)).as(qMark),
    " somewhere" -> markPlaceholderSlot(Obj2, Locative).as(qMark)
  )

  def makePrepForWhObject(preposition: LowerCaseString)(wh: LowerCaseString): StateT[Option, FrameState, Argument] =
    if(wh == "who".lowerCase) pure(Prep(preposition, Some(Noun(true))))
    else if(wh == "what".lowerCase) pure(Prep(preposition, Some(Noun(false))))
    else abort[Argument]

  val prepObjOpt = progress(
    "" -> (
      for { // prep has no obj
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep(preposition, None))
      } yield qMark),
    "" -> (
      for { // asking about obj of prep
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markAnswerSlot(Obj2, makePrepForWhObject(preposition))
      } yield qMark),
    " someone" -> (
      for { // prep has animate placeholder obj
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep(preposition, Some(Noun(true))))
      } yield qMark),
    " something" -> (
      for { // prep has inanimate placeholder obj
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep(preposition, Some(Noun(false))))
      } yield qMark),
    " doing" -> (
      for { // asking about VP argument to prep
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markAnswerSlot(Obj2, _ => pure[Argument](Prep((preposition.toString + " doing").lowerCase, Some(Noun(false)))))
      } yield qMark),
    " doing something" -> (
      for { // prep has inanimate placeholder VP argument
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep((preposition.toString + " doing").lowerCase, Some(Noun(false))))
      } yield qMark)

  )


  val postToObj = progress(
    "" -> ( // asking about obj of prep ("to" must have an object)
      for {
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markAnswerSlot(Obj2, makePrepForWhObject(preposition))
      } yield qMark),
    " someone" -> (
      for { // prep has animate placeholder obj
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep(preposition, Some(Noun(true))))
      } yield qMark),
    " something" -> (
      for { // prep has inanimate placeholder obj
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep(preposition, Some(Noun(false))))
      } yield qMark),
    " do" -> (
      for { // asking about VP argument to "to"
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markAnswerSlot(Obj2, _ => pure[Argument](Prep((preposition.toString + " do").lowerCase, Some(Noun(false)))))
      } yield qMark),
    " do something" -> (
      for { // prep has inanimate placeholder VP argument
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep((preposition.toString + " do").lowerCase, Some(Noun(false))))
      } yield qMark)

  )

  val postDoObj = progress(
    "" -> ( // asking about obj of prep ("do" must have an object)
      for {
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markAnswerSlot(Obj2, makePrepForWhObject(preposition))
      } yield qMark),
    " someone" -> (
      for { // prep has animate placeholder obj
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep(preposition, Some(Noun(true))))
      } yield qMark),
    " something" -> (
      for { // prep has inanimate placeholder obj
        fs <- get
        preposition <- lift(fs.preposition)
        _ <- markPlaceholderSlot(Obj2, Prep(preposition, Some(Noun(false))))
      } yield qMark)
  )


  val nonToPrep = TemplateProgress(
    nonToEndingPrepositions.map(_.toString).map(prep =>
      (" " + prep) -> (
        for {
          fs <- get
          _ <- lift(fs.preposition.ifEmpty(())) // must have no preposition already, though that should always be true
          _ <- set(fs.copy(preposition = Some(prep.lowerCase)))
        } yield prepObjOpt
      )
    )
  )

  val to = TemplateProgress(
    toEndingPrepositions.map(_.toString).map(prep =>
      (" " + prep) -> (
        for {
          fs <- get
          _ <- lift(fs.preposition.ifEmpty(())) // must have no preposition already, though that should always be true
          _ <- set(fs.copy(preposition = Some(prep.lowerCase)))
        } yield postToObj
      )
    )
  )

  val doPrep = TemplateProgress(
    NonEmptyList.of(" do", " doing").map(doVerb =>
      (doVerb) -> (
        for {
          fs <- get
          _ <- lift(fs.preposition.ifEmpty(())) // must have no preposition already, though that should always be true
          _ <- set(fs.copy(preposition = Some(doVerb.lowerCase)))
        } yield postDoObj
      )
    )
  )

  val prep = progress(
    "" -> pure(to),
    "" -> pure(doPrep),
    "" -> pure(nonToPrep),
    "" -> pure(noPrepObj),
    "" -> pure(qMark)
  )

  def makeNounForWh(wh: LowerCaseString): StateT[Option, FrameState, Noun] =
    lift(
      if(wh == "who".lowerCase) Some(Noun(true))
      else if(wh == "what".lowerCase) Some(Noun(false))
      else None
    )

  val obj = progress(
    "" -> pure(prep),
    "" -> markAnswerSlot(Obj, makeNounForWh).as(prep),
    " someone" -> markPlaceholderSlot(Obj, Noun(true)).as(prep),
    " something" -> markPlaceholderSlot(Obj, Noun(false)).as(prep)
  )

  // follows a have-aux. assume already isPerfect
  val pastParticipleVerb = progress(
    s" been $presentParticiple" -> modify((FrameState.frame composeLens Frame.isProgressive).set(true)).as(obj),
    s" been $pastParticiple" -> modify((FrameState.frame composeLens Frame.isPassive).set(true)).as(obj),
    (" " + pastParticiple.toString) -> pure(obj)
  )

  // follows a modal
  val infinitiveVerb = progress(
    (" " + stem.toString) -> pure(obj),
    s" be $presentParticiple" -> modify((FrameState.frame composeLens Frame.isProgressive).set(true)).as(obj),
    s" have been $presentParticiple" -> modify(
      (FrameState.frame composeLens Frame.isPerfect).set(true)
        andThen (FrameState.frame composeLens Frame.isProgressive).set(true)
    ).as(obj),
    s" be $pastParticiple" -> modify((FrameState.frame composeLens Frame.isPassive).set(true)).as(obj),
    s" have $pastParticiple" -> modify((FrameState.frame composeLens Frame.isPerfect).set(true)).as(obj),
    s" have been $pastParticiple" -> modify(
      (FrameState.frame composeLens Frame.isPerfect).set(true)
        andThen (FrameState.frame composeLens Frame.isPassive).set(true)
    ).as(obj)
  )

  // follows a do-aux
  val stemVerb = progress(
    " " + stem.toString -> pure(obj)
  )

  // follows a be-aux
  val presentParticipleOrPassiveVerb = progress(
    (" " + presentParticiple.toString) -> modify((FrameState.frame composeLens Frame.isProgressive).set(true)).as(obj),
    (s" being $pastParticiple") -> modify(
      (FrameState.frame composeLens Frame.isProgressive).set(true)
        andThen (FrameState.frame composeLens Frame.isPassive).set(true)
    ).as(obj),
    (" " + pastParticiple.toString) -> modify((FrameState.frame composeLens Frame.isPassive).set(true)).as(obj)
  )

  // follows no aux
  val tensedVerb = progress(
    (" " + present.toString) -> modFrame(Frame.tense.set(PresentTense)).as(obj),
    (" " + past.toString) -> modFrame(Frame.tense.set(PastTense)).as(obj)
  )

  // neg/subj states carry the verb form through; so, all need to be constructed at construction time

  def postSubjectNegation(targetVerbState: TemplateState) = progress(
    "" -> pure(targetVerbState),
    " not" -> modify((FrameState.frame composeLens Frame.isNegated).set(true)).as(targetVerbState)
  )

  def subj(targetVerbState: TemplateState, alreadyNegated: Boolean) = {
    val target = if(alreadyNegated) targetVerbState else postSubjectNegation(targetVerbState)
    progress(
      " someone" -> markPlaceholderSlot(Subj, Noun(true)).as(target),
      " something" -> markPlaceholderSlot(Subj, Noun(false)).as(target),
      " it" -> markPlaceholderSlot(Subj, Noun(false)).as(target)
    )
  }

  def optionalSubj(targetVerbState: TemplateState, alreadyNegated: Boolean) = {
    val skipSubjTarget = if(alreadyNegated) targetVerbState else postSubjectNegation(targetVerbState)
    progress(
      "" -> pure(subj(targetVerbState, alreadyNegated)),
      "" -> markAnswerSlot(Subj, makeNounForWh).as(skipSubjTarget) // can skip directly to verb if we make subj the answer
    )
  }

  def negContraction(subjRequired: Boolean, targetVerbState: TemplateState) = {
    def target(negate: Boolean) = if(subjRequired) subj(targetVerbState, negate) else optionalSubj(targetVerbState, negate)
    progress(
      "" -> pure(target(false)),
      "n't" -> modify((FrameState.frame composeLens Frame.isNegated).set(true)).as(target(true))
    )
  }

  def haveAux(subjRequired: Boolean) = {
    val target = negContraction(subjRequired, pastParticipleVerb)
    progress(
      " has" -> modFrame(Frame.tense.set(PresentTense) andThen Frame.isPerfect.set(true)).as(target),
      " had" -> modFrame(Frame.tense.set(PastTense) andThen Frame.isPerfect.set(true)).as(target)
    )
  }

  def infNegContraction(subjRequired: Boolean) = negContraction(subjRequired, infinitiveVerb)

  def modalAux(subjRequired: Boolean) = {
    def infSubj(negate: Boolean) = if(subjRequired) subj(infinitiveVerb, negate) else optionalSubj(infinitiveVerb, negate)
    val infNegContraction = negContraction(subjRequired, infinitiveVerb)
    progress(
      " can't" -> modFrame(Frame.tense.set(Modal("can".lowerCase)) andThen Frame.isNegated.set(true)).as(infSubj(true)),
      " can" -> modFrame(Frame.tense.set(Modal("can".lowerCase))).as(infSubj(false)),
      " won't" -> modFrame(Frame.tense.set(Modal("will".lowerCase)) andThen Frame.isNegated.set(true)).as(infSubj(true)),
      " will" -> modFrame(Frame.tense.set(Modal("will".lowerCase))).as(infSubj(false)),
      " might" -> modFrame(Frame.tense.set(Modal("might".lowerCase))).as(infSubj(false)),
      " would" -> modFrame(Frame.tense.set(Modal("would".lowerCase))).as(infNegContraction),
      " should" -> modFrame(Frame.tense.set(Modal("should".lowerCase))).as(infNegContraction)
    )
  }

  def doAux(subjRequired: Boolean) = {
    val target = negContraction(subjRequired, stemVerb)
    progress(
      " does" -> modFrame(Frame.tense.set(PresentTense)).as(target),
      " did" -> modFrame(Frame.tense.set(PastTense)).as(target)
    )
  }

  def beAux(subjRequired: Boolean) =  {
    val target = negContraction(subjRequired, presentParticipleOrPassiveVerb)
    progress(
      " is" -> modFrame(Frame.tense.set(PresentTense)).as(target),
      " was" -> modFrame(Frame.tense.set(PastTense)).as(target)
    )
  }

  def preAux(subjRequired: Boolean) = {
    val tail = NonEmptyList.of[TemplateTransition](
      "" -> pure(beAux(subjRequired)),
      "" -> pure(modalAux(subjRequired)),
      "" -> pure(doAux(subjRequired)),
      "" -> pure(haveAux(subjRequired)))
    val straightToVerb: TemplateTransition =
      "" -> markAnswerSlot(Subj, makeNounForWh).as(tensedVerb)
    val transitions = if(subjRequired) tail else straightToVerb :: tail
    TemplateProgress(transitions)
  }

  val wh = {
    val aux = preAux(subjRequired = false)
    val auxRequiringSubject = preAux(subjRequired = true)
    progress(
      "Who" -> modify(FrameState.whWord.set(Some("who".lowerCase))).as(aux),
      "What" -> modify(FrameState.whWord.set(Some("what".lowerCase))).as(aux),
      "When" -> modify(FrameState.whWord.set(Some("when".lowerCase))).as(auxRequiringSubject),
      "Where" -> modify(FrameState.whWord.set(Some("where".lowerCase))).as(auxRequiringSubject),
      "Why" -> modify(FrameState.whWord.set(Some("why".lowerCase))).as(auxRequiringSubject),
      "How" -> modify(FrameState.whWord.set(Some("how".lowerCase))).as(auxRequiringSubject),
      "How much" -> modify(FrameState.whWord.set(Some("how much".lowerCase))).as(auxRequiringSubject),
      "How long" -> modify(FrameState.whWord.set(Some("how long".lowerCase))).as(auxRequiringSubject)
    )
  }

  def start = wh
}
