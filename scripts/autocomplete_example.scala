import qasrl.TemplateStateMachine
import qasrl.QuestionProcessor
import qasrl.Autocomplete
// imports typeclass instances, e.g., Foldable[Vector] is used for text rendering
import cats.implicits._
import nlpdata.datasets.wiktionary.WiktionaryFileSystemService
// imports the LowerCaseString convenience type, associated ops/conversions, and the new .lowerCase extension method on String
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import java.nio.file.Paths

// Wiktionary data contains a bunch of inflections, used for the main verb in the QA-SRL template
val wiktionary = new WiktionaryFileSystemService(Paths.get("datasets/wiktionary"))

val exampleSentence = "Rolls-Royce Motor Cars Inc. said it expects its U.S. sales to remain steady at about 1,200 cars in 1990 .".split(" ").toVector

// Inflections object stores inflected forms for all of the verb tokens seen in exampleSentence.iterator
val inflections = wiktionary.getInflectionsForTokens(exampleSentence.iterator)
// Inflected forms object holds stem, present, past, past-participle, and present-participle forms
val remainInflectedForms = inflections.getInflectedForms("remain".lowerCase).get
// State machine stores all of the logic of QA-SRL templates and connects them to / iteratively constructs their Frames (see Frame.scala)
val stateMachine = new TemplateStateMachine(exampleSentence, remainInflectedForms)
// Question processor provides a convenient interface for using the state machine to process a string
val questionProcessor = new QuestionProcessor(stateMachine)
// autocomplete provides a convenient interface for running QA-SRL autocomplete with question suggestions
val autocomplete = new Autocomplete(questionProcessor)

def run = {
  val in = new java.util.Scanner(System.in)
  // completeQuestions keeps track of the frames/arguments of questions already asked, to be fed into autocomplete
  // to produce question suggestions
  var completeQuestions = Set.empty[QuestionProcessor.CompleteState]
  var done = false
  while(in.hasNextLine && !done) {
    val qPrefix = in.nextLine
    if(qPrefix == "exit") {
      done = true
    } else {
      println
      println(Text.render(exampleSentence))
      completeQuestions.map(_.fullText).toList.sorted.foreach(println)
      println(s"Prefix: $qPrefix")
      autocomplete(qPrefix.lowerCase, completeQuestions) match {
        // in this case, at some point (badStartIndex) the query string deviated from the allowable QA-SRL questions.
        // suggestions are provided anyway, given the last good prefix, and noting where the query deviated.
        case Autocomplete.Incomplete(suggestions, Some(badStartIndex)) =>
          println(s"Status: Invalid")
          suggestions.toList.foreach { case Autocomplete.Suggestion(text, isComplete) =>
            val prefix = if(isComplete) " *" else "  "
            println(s"${prefix}${text.take(badStartIndex)}│${text.drop(badStartIndex)}")
          }
        // in this case, the query string has valid continuations under the QA-SRL paradigm.
        case Autocomplete.Incomplete(suggestions, None) =>
          println(s"Status: Incomplete")
          suggestions.toList.foreach { case Autocomplete.Suggestion(text, isComplete) =>
            val prefix = if(isComplete) " *" else "  "
            println(s"${prefix}${text}")
          }
        // in this case, the query string was exactly a QA-SRL question.
        case Autocomplete.Complete(completeStates) =>
          println(s"Status: Complete")
          completeQuestions = completeQuestions ++ completeStates
      }
    }
  }
}
println("Enter \"run\" to begin the autocomplete example. Then enter \"exit\" to finish.")
