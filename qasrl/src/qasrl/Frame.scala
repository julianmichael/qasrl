package qasrl

import jjm.DependentMap
import jjm.LowerCaseString
import jjm.ling.en._
import jjm.implicits._
import VerbForm._

import cats.Id
import cats.Foldable
import cats.data.NonEmptyList
import cats.data.StateT
import cats.data.State
import cats.implicits._

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

  type ArgMap[A] = Map[ArgumentSlot, A]

  private[this] def append[A](a: A): StateT[List, List[Either[String, A]], Unit] =
    StateT.modify[List, List[Either[String, A]]](Right(a) :: _)
  private[this] def appendString[A](word: String): StateT[List, List[Either[String, A]], Unit] =
    StateT.modify[List, List[Either[String, A]]](Left(word) :: _)
  private[this] def appendEither[A](e: Either[String, A]): StateT[List, List[Either[String, A]], Unit] =
    StateT.modify[List, List[Either[String, A]]](e :: _)
  private[this] def appendAllStrings[F[_]: Foldable, A](fs: F[String]): StateT[List, List[Either[String, A]], Unit] =
    fs.foldM[StateT[List, List[Either[String, A]], ?], Unit](()) { case (_, s) => appendString(s) }
  private[this] def appendAll[F[_]: Foldable, A](fs: F[Either[String, A]]): StateT[List, List[Either[String, A]], Unit] =
    fs.foldM[StateT[List, List[Either[String, A]], ?], Unit](()) { case (_, s) => appendEither(s) }
  private[this] def choose[A, B](as: List[A]): StateT[List, List[Either[String, B]], A] =
    StateT.liftF[List, List[Either[String, B]], A](as)
  private[this] def pass[A]: StateT[List, List[Either[String, A]], Unit] =
    StateT.pure[List, List[Either[String, A]], Unit](())
  // private[this] def abort[A]: StateT[List, List[Either[String, A]], Unit] =
  //   choose[Unit, A](List[Unit]())

  private[this] def renderNecessaryNoun[A](slot: ArgumentSlot.Aux[Noun], argValues: ArgMap[A]) = args.get(slot) match {
    case None       => choose[String, A](List("someone", "something")) >>= appendString[A]
    case Some(noun) => argValues.get(slot).fold(appendAllStrings[List, A](noun.placeholder))(append[A])
  }

  private[this] def renderWhNoun[A](slot: ArgumentSlot.Aux[Noun]) = args.get(slot) match {
    case None       => choose[String, A](List("Who", "What")) >>= appendString[A]
    case Some(noun) => choose[String, A](noun.wh.toList) >>= appendString[A]
  }

  private[this] def renderWhOrAbort[Arg <: Argument, A](slot: ArgumentSlot.Aux[Arg]) =
    choose[String, A]((args.get(slot) >>= (_.wh)).toList) >>= appendString[A]

  // TODO do the un-gap thing in the arguments to get "to do" and "doing" rendering correct more properly
  val doPlaceholders = Set("do", "doing")
  def getUngap(gap: Option[String]) = gap.toList.flatMap(_.split(" ").toList.filter(s => !doPlaceholders.contains(s)))
  private[this] def renderArgIfPresent[Arg <: Argument, A](slot: ArgumentSlot.Aux[Arg], argValues: ArgMap[A]) =
    args.get(slot).fold(pass[A])(argSlotValue =>
      argValues.get(slot).fold(appendAllStrings[List, A](argSlotValue.gap.toList ++ argSlotValue.placeholder))(argMapValue =>
        appendAll[List, A](getUngap(argSlotValue.gap).map(Left[String, A](_)) ++ List(Right[String, A](argMapValue)))
      )
    )

  private[this] def renderGap[Arg <: Argument, A](slot: ArgumentSlot.Aux[Arg]) =
    appendAllStrings[List, A](args.get(slot).toList >>= (_.gap.toList))

  private[this] def renderAuxThroughVerb[A](includeSubject: Boolean, argValues: ArgMap[A]) = {
    val verbStack = getVerbStack
    if (includeSubject) {
      val splitVerbStack = splitVerbStackIfNecessary(verbStack)
      val (aux, verb) = (splitVerbStack.head, splitVerbStack.tail)
      appendString[A](aux) >> renderNecessaryNoun(Subj, argValues) >> appendAllStrings[List, A](verb)
    } else appendAllStrings[NonEmptyList, A](verbStack)
  }

  def questionsForSlot(slot: ArgumentSlot) = questionsForSlotWithArgs(slot, Map())
  def questionsForSlotWithArgs(slot: ArgumentSlot, argMap: ArgMap[String]) = {
    val qStateT = slot match {
      case Subj =>
        renderWhNoun[String](Subj) >>
        renderAuxThroughVerb(includeSubject = false, argMap) >>
        renderArgIfPresent(Obj, argMap) >>
        renderArgIfPresent(Obj2, argMap)
      case Obj =>
        renderWhNoun[String](Obj) >>
        renderAuxThroughVerb(includeSubject = true, argMap) >>
        renderGap(Obj) >>
        renderArgIfPresent(Obj2, argMap)
      case Obj2 =>
        renderWhOrAbort[Argument, String](Obj2) >>
        renderAuxThroughVerb(includeSubject = true, argMap) >>
        renderArgIfPresent(Obj, argMap) >>
        renderGap(Obj2)
      case Adv(wh) =>
        append[String](wh.toString.capitalize) >>
        renderAuxThroughVerb(includeSubject = true, argMap) >>
        renderArgIfPresent(Obj, argMap) >>
        renderArgIfPresent(Obj2, argMap)
    }
    qStateT.runS(List.empty[Either[String, String]]).map(_.reverse.map(_.merge).mkString(" ") + "?")
  }

  def genClausesWithArgs[A](
    argValues: Map[ArgumentSlot, A]
  ): List[List[Either[String, A]]] = {
    val qStateT = {
      renderNecessaryNoun(Subj, argValues) >>
        renderAuxThroughVerb(includeSubject = false, argValues) >>
        renderArgIfPresent(Obj  , argValues) >>
        renderArgIfPresent(Obj2, argValues)
    }
    qStateT.runS(List.empty[Either[String, A]]).map(_.reverse)
  }

  def clausesWithArgMarkers = {
    genClausesWithArgs(args.keys.map(a => a.asInstanceOf[ArgumentSlot.Aux[Argument]]).map(a => a -> a).toMap)
  }

  def clauses = {
    genClausesWithArgs[String](Map()).map(_.map(_.merge).mkString(" "))
    // val qStateT = {
    //   renderNecessaryNoun(Subj) >>
    //     renderAuxThroughVerb(includeSubject = false) >>
    //     renderArgIfPresent(Obj) >>
    //     renderArgIfPresent(Obj2)
    // }
    // qStateT.runS(List.empty[String]).map(_.reverse.mkString(" "))
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
