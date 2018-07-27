package qasrl

import qasrl.util.DependentMap

import cats.Id
import cats.Foldable
import cats.data.NonEmptyList
import cats.data.StateT
import cats.data.State
import cats.implicits._

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.wiktionary._

import monocle.macros._

@Lenses case class Frame(
  verbInflectedForms: InflectedForms,
  args: DependentMap[ArgumentSlot.Aux, Id],
  tense: Tense,
  isPerfect: Boolean,
  isProgressive: Boolean,
  isPassive: Boolean,
  isNegated: Boolean
) {

  private[this] def modalTokens(modal: LowerCaseString) =
    if (isNegated) {
      if (modal.toString == "will") NonEmptyList.of("won't")
      else if (modal.toString == "can") NonEmptyList.of("can't")
      else if (modal.toString == "might") NonEmptyList.of("might", "not")
      else NonEmptyList.of(s"${modal}n't")
    } else {
      NonEmptyList.of(modal.toString)
    }

  private[this] def getForms(s: LowerCaseString) = {
    if (verbInflectedForms.allForms.contains(s)) Some(verbInflectedForms)
    else if (InflectedForms.beSingularForms.allForms.contains(s))
      Some(InflectedForms.beSingularForms)
    else if (InflectedForms.doForms.allForms.contains(s)) Some(InflectedForms.doForms)
    else if (InflectedForms.haveForms.allForms.contains(s)) Some(InflectedForms.haveForms)
    else None
  }

  private[this] def push(s: String) =
    State.modify[NonEmptyList[String]](s :: _)
  private[this] def pushAll(ss: NonEmptyList[String]) =
    State.modify[NonEmptyList[String]](x => ss ++ x.toList)
  private[this] def modTop(f: String => String) =
    State.modify[NonEmptyList[String]](l => NonEmptyList(f(l.head), l.tail))
  private[this] def modForm(form: VerbForm) =
    modTop(w => getForms(w.lowerCase).fold(w)(_(form)))

  // should always agree with what's produced on the verb stack.
  // ideally they would share code somehow but this is easiest for now and probably works.
  def getVerbConjugation(subjectPresent: Boolean): VerbForm = {
    if (isPassive) PastParticiple
    else if (isProgressive) PresentParticiple
    else if (isPerfect) PastParticiple
    else
      tense match {
        case Modal(_)                           => Stem
        case _ if (isNegated || subjectPresent) => Stem
        case PastTense                          => Past
        case PresentTense                       => PresentSingular3rd
      }
  }

  def getVerbStack = {
    def pass = State.pure[NonEmptyList[String], Unit](())

    val stackState = for {
      // start with verb stem
      _               <- (if (isPassive) modForm(PastParticiple) >> push("be") else pass)
      _               <- (if (isProgressive) modForm(PresentParticiple) >> push("be") else pass)
      _               <- (if (isPerfect) modForm(PastParticiple) >> push("have") else pass)
      postAspectStack <- State.get[NonEmptyList[String]]
      _ <- tense match {
        case Modal(m) => pushAll(modalTokens(m))
        case PastTense =>
          if (isNegated) {
            if (postAspectStack.size == 1) push("didn't")
            else (modForm(Past) >> modTop(_ + "n't"))
          } else modForm(Past)
        case PresentTense =>
          if (isNegated) {
            if (postAspectStack.size == 1) push("doesn't")
            else (modForm(PresentSingular3rd) >> modTop(_ + "n't"))
          } else modForm(PresentSingular3rd)
      }
    } yield ()

    stackState.runS(NonEmptyList.of(verbInflectedForms.stem)).value
  }

  def splitVerbStackIfNecessary(verbStack: NonEmptyList[String]) = {
    if (verbStack.size > 1) {
      verbStack
    } else
      tense match {
        case Modal(_)     => verbStack // should never happen, since a modal adds another token
        case PastTense    => (modForm(Stem) >> push("did")).runS(verbStack).value
        case PresentTense => (modForm(Stem) >> push("does")).runS(verbStack).value
      }
  }

  private[this] def append(word: String): StateT[List, List[String], Unit] =
    StateT.modify[List, List[String]](word :: _)
  private[this] def appendAll[F[_]: Foldable](fs: F[String]): StateT[List, List[String], Unit] =
    fs.foldM[StateT[List, List[String], ?], Unit](()) { case (_, s) => append(s) }
  private[this] def choose[F[_]: Foldable, A](as: F[A]): StateT[List, List[String], A] =
    StateT.liftF[List, List[String], A](as.toList)
  private[this] def pass: StateT[List, List[String], Unit] =
    StateT.pure[List, List[String], Unit](())
  private[this] def abort: StateT[List, List[String], Unit] =
    choose(List[Unit]())

  private[this] def renderNecessaryNoun(slot: ArgumentSlot.Aux[Noun]) = args.get(slot) match {
    case None       => choose(List("someone", "something")) >>= append
    case Some(noun) => appendAll(noun.placeholder)
  }

  private[this] def renderWhNoun(slot: ArgumentSlot.Aux[Noun]) = args.get(slot) match {
    case None       => choose(List("Who", "What")) >>= append
    case Some(noun) => choose(noun.wh) >>= append
  }

  private[this] def renderWhOrAbort[Arg <: Argument](slot: ArgumentSlot.Aux[Arg]) =
    choose(args.get(slot) >>= (_.wh)) >>= append

  private[this] def renderArgIfPresent[Arg <: Argument](slot: ArgumentSlot.Aux[Arg]) =
    appendAll(args.get(slot).toList >>= (_.placeholder))

  private[this] def renderGap[Arg <: Argument](slot: ArgumentSlot.Aux[Arg]) =
    appendAll(args.get(slot) >>= (_.gap))

  private[this] def renderAuxThroughVerb(includeSubject: Boolean) = {
    val verbStack = getVerbStack
    if (includeSubject) {
      val splitVerbStack = splitVerbStackIfNecessary(verbStack)
      val (aux, verb) = (splitVerbStack.head, splitVerbStack.tail)
      append(aux) >> renderNecessaryNoun(Subj) >> appendAll(verb)
    } else appendAll(verbStack)
  }

  def questionsForSlot(slot: ArgumentSlot) = {
    val qStateT = slot match {
      case Subj =>
        renderWhNoun(Subj) >>
        renderAuxThroughVerb(includeSubject = false) >>
        renderArgIfPresent(Obj) >>
        renderArgIfPresent(Obj2)
      case Obj =>
        renderWhNoun(Obj) >>
        renderAuxThroughVerb(includeSubject = true) >>
        renderGap(Obj) >>
        renderArgIfPresent(Obj2)
      case Obj2 =>
        renderWhOrAbort(Obj2) >>
        renderAuxThroughVerb(includeSubject = true) >>
        renderArgIfPresent(Obj) >>
        renderGap(Obj2)
      case Adv(wh) =>
        append(wh.toString.capitalize) >>
        renderAuxThroughVerb(includeSubject = true) >>
        renderArgIfPresent(Obj) >>
        renderArgIfPresent(Obj2)
    }
    qStateT.runS(List.empty[String]).map(_.reverse.mkString(" ") + "?")
  }
}

object Frame {

  def empty(verbForms: InflectedForms) =
    Frame(
      verbForms,
      DependentMap.empty[ArgumentSlot.Aux, Id],
      PastTense,
      false,
      false,
      false,
      false
    )
}
