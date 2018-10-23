package qasrl.test

import qasrl._
import qasrl.labeling.SlotBasedLabel

import nlpdata.datasets.wiktionary.InflectedForms
import nlpdata.util.LowerCaseStrings._

import cats.implicits._

import scala.util.{Success, Try}

import java.nio.file.Paths

// import utest.assert

// object QuestionTests extends TestSuite {
//   val tests = Tests {

//     "hello world" - {
//       println("Hello world!")
//     }

//     val questionsTry = Try(scala.io.Source.fromURL(getClass.getResource("/question-strings.txt")).getLines.toList)

//     "reference questions can be read from resources" - {
//       assert(questionsTry.get.size == 13621)
//     }

//     val questions = questionsTry.get

//     val genericInflectedForms = InflectedForms(
//       stem = "stem".lowerCase,
//       present = "present".lowerCase,
//       presentParticiple = "presentParticiple".lowerCase,
//       past = "past".lowerCase,
//       pastParticiple = "pastParticiple".lowerCase
//     )

//     def processQuestion(question: String) = {
//       val questionTokensIsh = question.init.split(" ").toVector.map(_.lowerCase)
//       val qPreps = questionTokensIsh.filter(TemplateStateMachine.allPrepositions.contains).toSet
//       val qPrepBigrams = questionTokensIsh
//         .sliding(2)
//         .filter(_.forall(TemplateStateMachine.allPrepositions.contains))
//         .map(_.mkString(" ").lowerCase)
//         .toSet
//       val stateMachine = new TemplateStateMachine(Vector(), genericInflectedForms, Some(qPreps ++ qPrepBigrams))
//       val template = new QuestionProcessor(stateMachine)

//       template.processStringFully(question)
//     }

//     val processedQuestionResults = questions.map(processQuestion)

//     val (invalidStates, goodStates) = processedQuestionResults.separate

//     val splitGoodStates = goodStates.map(
//       _.map(QuestionProcessor.ValidState.eitherIso.get).toList.separate
//     )

//     "all reference questions are valid" - {
//       assert(invalidStates.size == 0)
//     }

//     "no reference questions are incomplete" - {
//       splitGoodStates.foreach { case (inProgress, _) =>
//         assert(inProgress.size == 0)
//       }
//     }

//     val completeStates = splitGoodStates.map(_._2)

//     "all reference questions have some complete state" - {
//       completeStates.foreach(s => assert(s.nonEmpty))
//     }

//     "all complete states should reproduce the reference question text" - {
//       questions.zip(completeStates).foreach { case (q, state) =>
//         state.foreach(s => assert(s.fullText == q))
//       }
//     }

//     val completeStatesReferenceHist = Map(
//       1 -> 10887,
//       2 -> 2599,
//       3 -> 135
//     )
//     val completeStatesHist = completeStates.map(_.size).groupBy(x => x).map { case (k, v) => k -> v.size }

//     "complete states have the expected number of frames" - {
//       assert(completeStatesHist == completeStatesReferenceHist)
//     }

//     val slotLists = completeStates.map(_.map(s => SlotBasedLabel.getSlotsForQuestionStructure(s.frame, s.answerSlot)))

//     "no prepositions are whitesspace/empty" - {
//       slotLists.foreach { slotSet =>
//         slotSet.foreach { slots =>
//           val qStr = slots.renderWithSeparator(identity, ",")
//           slots.prep.foreach(p => assert{qStr; p.trim != ""})
//         }
//       }
//     }

//     "no prepositions contain do/doing" - {
//       slotLists.foreach { slotSet =>
//         slotSet.foreach { slots =>
//           val qStr = slots.renderWithSeparator(identity, ",")
//           slots.prep.foreach(p => assert{qStr; !.p.endsWith("do".lowerCase)})
//           slots.prep.foreach(p => assert{qStr; !.p.endsWith("doing".lowerCase)})
//         }
//       }
//     }

//     "\"to do\" is not separated between slots" - {
//       slotLists.foreach { slotSet =>
//         slotSet.foreach { slots =>
//           val qStr = slots.renderWithSeparator(identity, ",")
//           assert { qStr;
//             slots.prep.exists(p => p.endsWith(" to".lowerCase) || p == "to".lowerCase) &&
//              slots.obj2.exists(o => o.startsWith("do ".lowerCase) || o == "do".lowerCase)
//           }
//         }
//       }
//     }
//   }
// }


import org.scalatest._
import org.scalatest.prop._

class QuestionTests extends FunSuite with Matchers {

  import org.scalatest.Inside._
  import org.scalatest.AppendedClues._

  test("hello world") {
    println("Hello world!")
  }

  val questionsTry = Try(scala.io.Source.fromURL(getClass.getResource("/question-strings.txt")).getLines.toList)

  test("reference questions can be read from resources") {
    inside(questionsTry) {
      case Success(lines) => lines should have size 13621
    }
  }

  val questions = questionsTry.get

  val genericInflectedForms= InflectedForms(
    stem = "stem".lowerCase,
    present = "present".lowerCase,
    presentParticiple = "presentParticiple".lowerCase,
    past = "past".lowerCase,
    pastParticiple = "pastParticiple".lowerCase
  )

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

  val processedQuestionResults = questions.map(processQuestion)

  val (invalidStates, goodStates) = processedQuestionResults.separate

  val splitGoodStates = goodStates.map(
    _.map(QuestionProcessor.ValidState.eitherIso.get).toList.separate
  )

  test("all reference questions are valid") {
    invalidStates should have size 0
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
    questions.zip(completeStates).foreach { case (q, state) =>
      state.foreach(_.fullText shouldEqual q)
    }
  }

  val completeStatesReferenceHist = Map(
    1 -> 10887,
    2 -> 2599,
    3 -> 135
  )
  val completeStatesHist = completeStates.map(_.size).groupBy(x => x).map { case (k, v) => k -> v.size }

  test("complete states have the expected number of frames") {
    completeStatesHist shouldEqual completeStatesReferenceHist
  }

  val slotLists = completeStates.map(_.map(s => SlotBasedLabel.getSlotsForQuestionStructure(s.frame, s.answerSlot)).toSet)

  def getSatisfyingSlotLists(p: SlotBasedLabel[LowerCaseString] => Boolean) = {
    slotLists.map(_.filter(p)).filter(_.nonEmpty)
  }

  def getSlotSetClue(s: List[Set[SlotBasedLabel[LowerCaseString]]]) = {
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

  val prepIsWhitespace = (s: SlotBasedLabel[LowerCaseString]) =>
    s.prep.exists(_.toString == "")
  val prepContainsDo =   (s: SlotBasedLabel[LowerCaseString]) =>
    s.prep.forall(_.toString.endsWith("do")) ||
      s.prep.forall(_.toString.endsWith("doing"))
  val toDoIsSplit = (s: SlotBasedLabel[LowerCaseString]) =>
    s.prep.map(_.toString).exists(p => p.endsWith(" to") || p == "to") &&
      s.obj2.map(_.toString).exists(o => o.startsWith("do ") || o == "do")

  test("no prepositions are whitesspace/empty") {
    assertNoSatisfyingSlots(prepIsWhitespace)
  }

  test("no prepositions contain do/doing") {
    assertNoSatisfyingSlots(prepContainsDo)
  }

  test("\"to do\" is not split between slots") {
    assertNoSatisfyingSlots(toDoIsSplit)
  }

  val otherwiseGoodQuestions = getSatisfyingSlotLists(x => prepIsWhitespace(x) && prepContainsDo(x) && toDoIsSplit(x))
}
