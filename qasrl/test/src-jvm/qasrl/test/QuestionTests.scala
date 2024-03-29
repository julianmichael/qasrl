package qasrl.test

import qasrl._
import qasrl.labeling.SlotBasedLabel

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.implicits._

import cats.implicits._

import scala.util.{Success, Try}

import java.nio.file.Paths

import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop._

class QuestionTests extends AnyFunSuite with Matchers {

  import org.scalatest.Inside._
  import org.scalatest.AppendedClues._

  val questionsTry = Try(scala.io.Source.fromURL(getClass.getResource("/question-strings.txt")).getLines.toList)

  test("reference questions can be read from resources") {
    inside(questionsTry) {
      case Success(lines) => lines should have size 13616
    }
  }

  val questions = questionsTry.get

  val genericInflectedForms = InflectedForms(
    stem = "stem".lowerCase,
    presentSingular3rd = "presentSingular3rd".lowerCase,
    presentParticiple = "presentParticiple".lowerCase,
    past = "past".lowerCase,
    pastParticiple = "pastParticiple".lowerCase
  )

  // def processQuestionOld(question: String) = {
  //   val questionTokensIsh = question.init.split(" ").toVector.map(_.lowerCase)
  //   val qPreps = questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
  //   val qPrepBigrams = questionTokensIsh
  //     .sliding(2)
  //     .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
  //     .map(_.mkString(" ").lowerCase)
  //     .toSet
  //   val stateMachine = new TemplateStateMachineOld(Vector(), genericInflectedForms, Some(qPreps ++ qPrepBigrams))
  //   val template = new QuestionProcessorOld(stateMachine)

  //   template.processStringFully(question)
  // }

  def processQuestion(question: String) = {
    val questionTokensIsh = question.init.split(" ").toVector.map(_.lowerCase)
    val qPreps = questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
    val qPrepBigrams = questionTokensIsh
      .sliding(2)
      .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
      .map(_.mkString(" ").lowerCase)
      .toSet
    val stateMachine = new TemplateStateMachine(Vector(), genericInflectedForms, Some(qPreps ++ qPrepBigrams))
    val template = new QuestionProcessor(stateMachine)

    template.processStringFully(question)
  }

  // val pairedProcessingDiffs = {
  //   questions.map(q => processQuestionOld(q) -> processQuestion(q)).filter(Function.tupled(_ != _))
  // }
  // println(s"${pairedProcessingDiffs.size} differences between old and new: ")
  // pairedProcessingDiffs.foreach { case (old, cur) =>
  //   // def str(s: List[CompleteState]) = s.map(s => s.frame.args.toString + ": " + s.answerSlot.toString).map("  " + _).mkString("\n")
  //   println(s"Old:\n${old}")
  //   println(s"Cur:\n${cur}")
  //   println
  // }

  val processedQuestionResults = questions.map(processQuestion)

  val (invalidQuestionsWithStates, goodQuestionsWithStates) = (questions.zip(processedQuestionResults)).map {
    case (q, Left(x)) => Left(q -> x)
    case (q, Right(x)) => Right(q -> x)
  }.separate
  val (invalidQuestions, invalidStates) = invalidQuestionsWithStates.unzip
  val (goodQuestions, goodStates) = goodQuestionsWithStates.unzip

  val splitGoodStates = goodStates.map(
    _.map(QuestionProcessor.ValidState.eitherIso.get).toList.separate
  )

  test("all reference questions are valid") {
    val x = invalidQuestions.isEmpty
    val questionsWord = if(invalidStates.size == 1) "question" else "questions"
    assert(x) withClue (
      s"for ${invalidQuestions.size} $questionsWord:\n" +
        invalidQuestions.take(10).map("  " + _).mkString("\n")
    )
  }

  test("no reference questions are incomplete") {
    splitGoodStates.foreach { case (inProgress, _) =>
      inProgress should have size 0
    }
  }

  val completeStates = splitGoodStates.map(_._2)

  test("all reference questions have some complete state") {
    completeStates.foreach(_ should not be empty)
  }

  test("all complete states should reproduce the reference question text") {
    goodQuestions.zip(completeStates).foreach { case (q, state) =>
      state.foreach(_.fullText shouldEqual q)
    }
  }

  test("all complete frames should produce a singleton list of questions") {
    goodQuestions.zip(completeStates).foreach { case (q, state) =>
      state.foreach(s => s.frame.questionsForSlot(s.answerSlot).size shouldEqual 1)
    }
  }

  test("all complete frames should reproduce the source question") {
    goodQuestions.zip(completeStates).foreach { case (q, state) =>
      state.foreach(s => s.frame.questionsForSlot(s.answerSlot).foreach(_ shouldEqual q))
    }
  }

  val completeStatesReferenceHist = Map(
    1 -> 10882,
    2 -> 2599,
    3 -> 135
  )
  val completeStatesHist = completeStates.map(_.size).groupBy(x => x).map { case (k, v) => k -> v.size }

  test("complete states have the expected number of frames") {
    completeStatesHist shouldEqual completeStatesReferenceHist
  }

  import QuestionProcessor.CompleteState

  // val _completeStatesOld = {
  //   val processedQuestionResultsOld = goodQuestions.map(processQuestionOld)
  //   val (invalidStatesOld, goodStatesOld) = processedQuestionResultsOld.separate
  //   val splitGoodStatesOld = goodStatesOld.map(
  //     _.map(QuestionProcessor.ValidState.eitherIso.get).toList.separate
  //   )
  //   splitGoodStatesOld.map(_._2)
  // }

  // // differences between new and old
  // val stateDiffs = _completeStatesOld.zip(completeStates).filter {
  //   case (old, cur) => old != cur
  // }

  // // TODO: wait until we change this as well
  // println(s"${stateDiffs.size} differences between old and new: ")
  // stateDiffs.foreach { case (old, cur) =>
  //   def str(s: List[CompleteState]) = s.map(s => s.frame.args.toString + ": " + s.answerSlot.toString).map("  " + _).mkString("\n")
  //   println(s"Old:\n${str(old)}")
  //   println(s"Cur:\n${str(cur)}")
  //   println
  // }

  def getSatisfyingStateLists(p: CompleteState => Boolean) = {
    completeStates.map(_.filter(p)).filter(_.nonEmpty)
  }

  def getStateSetClue(s: List[List[CompleteState]]) = {
    val numQs = s.size
    val questionWord = if(numQs == 1) "question" else "questions"
    val numQsToPrint = 10
    s"for $numQs $questionWord:\n" + (
      s.map(set =>
        f"${set.head.fullText}%-60s " + set.toList.map(s =>
          // SlotBasedLabel.getSlotsForQuestionStructure(s.frame, s.answerSlot).renderQuestionString(identity)
          s.frame.args.toString + ": " + s.answerSlot
        ).mkString("\t|\t")
      ).map("  " + _).take(numQsToPrint).mkString("\n") + (
        if(s.size > numQsToPrint) "\n  ...\n" else "\n"
      )
    )
  }


  def assertNoSatisfyingStates(p: CompleteState => Boolean) = {
    val satisfyingStateLists = getSatisfyingStateLists(p)
    val x = satisfyingStateLists.isEmpty
    assert(x) withClue getStateSetClue(satisfyingStateLists)
  }

  test("no frames have whitespace prepositions") {
    assertNoSatisfyingStates(s =>
      s.frame.args.get(Obj2).collect {
        case Prep(p, _) => p.trim == ""
      }.exists(identity)
    )
  }

  // NOTE: actually right now we allow this
  // old: false for 1689 questions
  // test("no frames have do/doing preps") {
  //   assertNoSatisfyingStates(s =>
  //     s.frame.args.get(Obj2).collect {
  //       case Prep(p, _) => p.toString.endsWith("do") || p.toString.endsWith("doing")
  //     }.exists(identity)
  //   )
  // }

  val slotLists = completeStates.map(_.map(s => SlotBasedLabel.getSlotsForQuestionStructure(s.frame, s.answerSlot)))

  test("slots are deterministic for a question") {
    val nonDeterministicSlotLists = slotLists.map(_.toSet)
      .filter(_.size != 1) // 95 left
      .filter { slotSet =>
      slotSet.toList match {
        // remove the case where obj and obj2 are switched with an empty prep in between
        case fst :: snd :: Nil =>
          !(
            fst.prep.isEmpty && snd.prep.isEmpty && fst.obj == snd.obj2 && snd.obj == fst.obj2 && (
              fst.copy(obj = snd.obj, obj2 = snd.obj2) == snd
            )
          )
        case _ => true
      }
    }
    val questionsWord = if(nonDeterministicSlotLists.size == 1) "question" else "questions"
    val x = nonDeterministicSlotLists.isEmpty
    assert(x) withClue (
      s"for ${nonDeterministicSlotLists.size} $questionsWord:\n" +
        nonDeterministicSlotLists.take(10).map(_.map(_.renderWithSeparator(identity, ",")).mkString("\t")).map("  " + _).mkString("\n")
    )
  }

  val prepHasWhitespace = (s: SlotBasedLabel[LowerCaseString]) => s.prep.exists(p => p.toString != p.trim)
  val prepIsWhitespace = (s: SlotBasedLabel[LowerCaseString]) => s.prep.exists(_.trim == "")
  val prepContainsDo =   (s: SlotBasedLabel[LowerCaseString]) => {
    s.prep.exists(_.toString.endsWith("do")) || s.prep.exists(_.toString.endsWith("doing"))
  }
  val toDoIsSplit = (s: SlotBasedLabel[LowerCaseString]) => {
    s.prep.map(_.toString).exists(p => p.endsWith(" to") || p == "to") &&
      s.obj2.map(_.toString).exists(o => o.startsWith("do ") || o == "do")
  }
  val hasDoSomeone = (s: SlotBasedLabel[LowerCaseString]) => {
    s.obj2.exists(o => o == "do someone".lowerCase || o == "doing someone".lowerCase)
  }

  val slotsPassMuster =  (s: SlotBasedLabel[LowerCaseString]) =>
    !(prepHasWhitespace(s) || prepIsWhitespace(s) || prepContainsDo(s) || toDoIsSplit(s))

  def getSatisfyingSlotLists(p: SlotBasedLabel[LowerCaseString] => Boolean) = {
    slotLists.map(_.filter(p)).filter(_.nonEmpty)
  }

  def getSlotSetClue(s: List[List[SlotBasedLabel[LowerCaseString]]]) = {
    val numQs = s.foldMap(_.size)
    val questionWord = if(numQs == 1) "question" else "questions"
    val numQsToPrint = 10
    s"for $numQs $questionWord:\n" + (
      s.map(set =>
        set.toList.map(_.renderWithSeparator(identity, ",")).mkString("\t|\t")
      ).map("  " + _).take(numQsToPrint).mkString("\n") + (
        if(s.size > numQsToPrint) "\n  ...\n" else "\n"
      )
    )
  }

  def assertNoSatisfyingSlots(p: SlotBasedLabel[LowerCaseString] => Boolean) = {
    val satisfyingSlotLists = getSatisfyingSlotLists(p)
    val x = satisfyingSlotLists.isEmpty
    assert(x) withClue getSlotSetClue(satisfyingSlotLists)
  }

  test("slots reconstruct original question") {
    assertNoSatisfyingStates(s => s.fullText != SlotBasedLabel.getSlotsForQuestionStructure(s.frame, s.answerSlot).renderQuestionString(identity))
  }

  // old: failed for 136 questions
  test("no prepositions are whitespace/empty") {
    assertNoSatisfyingSlots(prepIsWhitespace)
  }

  test("no prepositions have extra whitespace") {
    assertNoSatisfyingSlots(prepHasWhitespace)
  }

  test("no prepositions contain do/doing") {
    assertNoSatisfyingSlots(prepContainsDo)
  }

  test("\"to do\" is not split between slots") {
    assertNoSatisfyingSlots(toDoIsSplit)
  }

  test("\"do/doing someone\" never appears") {
    assertNoSatisfyingSlots(hasDoSomeone)
  }

  // TODO: test that you only take slots where there is an obj and no obj2 or whatever
}
