import qasrl._
import cats.implicits._
import nlpdata.datasets.wiktionary._
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import java.nio.file.Paths

val Wiktionary = new WiktionaryFileSystemService(Paths.get("datasets/wiktionary"))

val exampleSentence = "Rolls-Royce Motor Cars Inc. said it expects its U.S. sales to remain steady at about 1,200 cars in 1990 .".split(" ").toVector

val inflections = Wiktionary.getInflectionsForTokens(exampleSentence.iterator)
val remainInflectedForms = inflections.getInflectedForms("remain".lowerCase).get
val stateMachine = new TemplateStateMachine(exampleSentence, remainInflectedForms)
val questionProcessor = new QuestionProcessor(stateMachine)
val autocomplete = new Autocomplete(questionProcessor)

def run = {
  val in = new java.util.Scanner(System.in)
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
        case Autocomplete.Incomplete(suggestions, Some(badStartIndex)) =>
          println(s"Status: Invalid")
          suggestions.toList.foreach { case Autocomplete.Suggestion(text, isComplete) =>
            val prefix = if(isComplete) " *" else "  "
            println(s"${prefix}${text.take(badStartIndex)}│${text.drop(badStartIndex)}")
          }
        case Autocomplete.Incomplete(suggestions, None) =>
          println(s"Status: Incomplete")
          suggestions.toList.foreach { case Autocomplete.Suggestion(text, isComplete) =>
            val prefix = if(isComplete) " *" else "  "
            println(s"${prefix}${text}")
          }
        case Autocomplete.Complete(completeStates) =>
          println(s"Status: Complete")
          completeQuestions = completeQuestions ++ completeStates
      }
    }
  }
}
println("Enter \"run\" to begin the autocomplete example. Then enter \"exit\" to finish.")
