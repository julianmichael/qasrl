package example

import spacro.ui._

import qasrl.crowd.QASRLDispatcher
import qasrl.crowd.util._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import scalajs.js.JSApp

import upickle.default._

object Dispatcher extends QASRLDispatcher[SentenceId] with JSApp {

  val dataToggle = VdomAttr("data-toggle")
  val dataPlacement = VdomAttr("data-placement")

  val TooltipsComponent = ScalaComponent.builder[VdomTag]("Tooltips")
    .render(_.props)
    .componentDidMount(_ =>
    Callback {
      scala.util.Try {
        scala.scalajs.js.Dynamic.global.$("[data-toggle=\"tooltip\"]").tooltip()
      }
      ()
    }
  ).build

  import settings._

  def example(question: String, answer: String, isGood: Boolean, tooltip: String = "") =
    <.li(
      <.span(
        if(isGood) Styles.goodGreen else Styles.badRed,
        TagMod(
          Styles.underlined,
          dataToggle := "tooltip",
          dataPlacement := "top",
          ^.title := tooltip).when(tooltip.nonEmpty),
        <.span(question),
        <.span(" --> "),
        <.span(answer)
      )
    )

  private[this] val examples = <.div(
    TooltipsComponent(
      <.div(
        <.p(Styles.bolded," This section is exactly the same between the question writing and question answering tasks. "),
        <.p(" Below, for each verb, we list a complete set of good questions (green) and some bad ones (red). ",
            " Hover the mouse over the underlined examples for an explanation. "),
        <.blockquote(
          ^.classSet1("blockquote"),
          "Protesters ", <.span(Styles.bolded, " blamed "), " the corruption scandal on local officials, who today refused to promise that they would resume the investigation before year's end. "),
        <.ul(
          example(
            "Who blamed someone?",
            "Protesters",
            true),
          example(
            "Who did someone blame something on?",
            "local officials",
            true),
          example(
            "What did someone blame someone for?",
            "the corruption scandal",
            true,
            """ "What did someone blame on someone?" would also have been okay. """),
          example(
            "Who blamed?",
            "Protesters",
            false,
            """ This question is invalid by the litmus test, because the sentence "Protesters blamed." is ungrammatical. """)
        ),

        <.blockquote(
          ^.classSet1("blockquote"),
          "Protesters blamed the corruption scandal on local officials, who today ", <.span(Styles.bolded, " refused "), " to promise that they would resume the investigation before year's end. "),
        <.ul(
          example(
            "Who refused to do something?",
            "local officials / they",
            true,
            """When answering, list all of the phrases in the sentence that refer to the correct answer, including pronouns like "they"."""),
          example(
            "What did someone refuse to do?",
            "promise that they would resume the investigation before year's end",
            true),
          example(
            "What did someone refuse to do?",
            "promise that they would resume the investigation",
            false,
            """The answer is not specific enough: it should include "before year's end" because that was part of what they were refusing to promise."""),
          example(
            "What did someone refuse to do?",
            "resume the investigation before year's end",
            false,
            """This answer is also bad: you should instead choose the more literal answer above."""),
          example(
            "When did someone refuse to do something?",
            "today",
            true),
          example(
            "Who didn't refuse to do something?",
            "Protesters",
            false,
            """The sentence does not say anything about protesters refusing or not refusing, so this question is invalid.""")
        ),

        <.blockquote(
          ^.classSet1("blockquote"),
          "Protesters blamed the corruption scandal on local officials, who today refused to ", <.span(Styles.bolded, " promise "), " that they would resume the investigation before year's end. "),
        <.ul(
          example(
            "Who didn't promise something?",
            "local officials / they",
            true,
            "Negated questions work when the sentence is indicating that the event or state expressed by the verb did not happen."),
          example(
            "What didn't someone promise?",
            "that they would resume the investigation before year's end",
            true),
          example(
            "When didn't someone promise to do something?",
            "before year's end",
            false,
            """ This question is bad because "before year's end" refers to the timeframe of resuming the investigation, not the timeframe of the promise being made.
            All such questions must pertain to the time/place of the chosen verb. """)
        ),

        <.blockquote(
          ^.classSet1("blockquote"),
          "Protesters blamed the corruption scandal on local officials, who today refused to promise that they would ", <.span(Styles.bolded, " resume "), " the investigation before year's end. "),
        <.ul(
          example(
            "Who might resume something?",
            "local officials / they",
            true,
            """Words like "might" or "would" are appropriate when the sentence doesn't clearly indicate whether something actually happened."""),
          example(
            "What might someone resume?",
            "the investigation",
            true),
          example(
            "When might someone resume something?",
            "before year's end",
            true)
        ),

        <.blockquote(
          ^.classSet1("blockquote"),
          <.span(Styles.bolded, " Let"), "'s go up to the counter and ask."),
        <.ul(
          example(
            "Who should someone let do something?",
            "'s",
            true,
            """Here, you should read 's as the word it stands for: "us".
            So by substituting back into the question, we get "someone should let us do something",
            which is what someone is suggesting when they say "Let's go". """),
          example(
            "What should someone let someone do?",
            "go up to the counter and ask",
            true,
            """It would also be acceptable to mark "go up to the counter" and "ask" as two different answers. """),
          example(
            "Where should someone let someone do something?",
            "the counter",
            false,
            """Questions should only concern the targeted verb: "letting" is not happening at the counter.""")
        ),

        <.blockquote(
          ^.classSet1("blockquote"),
          "Let's ", <.span(Styles.bolded, " go "), " up to the counter and ask."),
        <.ul(
          example(
            "Who should go somewhere?",
            "'s",
            true),
          example(
            "Where should someone go?",
            "up to the counter",
            true,
            """Since both "up" and "to the counter" describe where they will go, they should both be included in the answer to a "where" question. """))
      )
    )
  )

  import InstructionsComponent._

  val generationOverview = <.div(
    <.p(Styles.badRed, """Read through all of the instructions and make sure you understand the interface controls before beginning. A full understanding of the requirements will help make sure validators approve your work and you can retain your qualification."""),
    <.p("""This task is for an academic research project by the natural language processing group at the University of Washington.
        We wish to deconstruct the meanings of verbs in English sentences into lists of questions and answers.
        You will be presented with a selection of English text with a verb written in bold."""),
    <.p("""You will write questions about the verb and highlight their answers in the original sentence. """,
        <.b(""" Note: it takes exactly 2 clicks to highlight each answer; see the Interface & Controls tab for details. """),
    """ Questions are required to follow a strict format, which is enforced by autocomplete functionality in the interface. """,
        """ For example, the prompt below should elicit the following questions and answers: """),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Protesters ", <.span(Styles.bolded, " blamed "), " the corruption scandal on local officials, who today refused to promise that they would resume the investigation before year's end. "),
    <.ul(
      <.li("Who blamed someone? --> Protesters"),
      <.li("Who did someone blame something on? --> local officials"),
      <.li("What did someone blame on someone? --> the corruption scandal")
    ),
    <.h2("Guidelines"),
    <.ol(
      <.li(
        <.span(Styles.bolded, "Correctness. "),
        """Each question-answer pair must satisfy the litmus test that if you substitute the answer back into the question,
           the result is a grammatical statement, and it is true according to the sentence given. For example, """,
        <.span(Styles.bolded, "Who blamed someone? --> Protesters"), """ becomes """,
        <.span(Styles.goodGreen, "Protesters blamed someone, "), """ which is valid, while """,
        <.span(Styles.bolded, "Who blamed? --> Protesters"), """ would become """,
        <.span(Styles.badRed, "Protesters blamed, "), s""" which is ungrammatical, so it is invalid.
           Your questions will be judged by other annotators, and you must retain an accuracy of
           ${(100.0 * generationAccuracyBlockingThreshold).toInt}% in order to remain qualified. """),
      <.li(
        <.span(Styles.bolded, "Verb-relevance. "),
        s"""The answer to a question must pertain to the participants, time, place, reason, etc., of """,
        <.span(Styles.bolded, " the target verb in the sentence. "),
        """ For example, if the sentence is """,
        <.span(Styles.bolded,
               " He ",
               <.span(Styles.niceBlue, Styles.underlined, "promised"),
               " to come tomorrow, "),
        """ you may """, <.span(Styles.bolded, " not "), " write ",
        <.span(Styles.badRed, " When did someone promise to do something? --> tomorrow, "),
        """ because tomorrow is """, <.i(" not "),
        " the time that he made the promise, but rather the time that he might come."),
      <.li(
        <.span(Styles.bolded, "Exhaustiveness. "),
        s"""You must write as many questions, and as many answers to each question, as possible.
           Each HIT will require you to write at least one question, and you must write more than 2 questions per verb
           on average in order to remain qualified for the HIT. You will be awarded a bonus for each new question,
           starting at ${generationRewardCents}c and going up by 1c for each additional question.
           However, note that none of the answers to your questions may overlap.
           If there is more than one possible question that has the same answer, just write one of them."""
      )
    ),
    <.p("Occasionally, you may get a bolded word that isn't a verb, or is hard or impossible to write questions about. ",
        " In this case, please do your best to come up with one question, even if it is nonsensical. ",
        " While it will count against your accuracy, this case is rare enough that it shouldn't matter. ",
        " If the sentence has grammatical errors or is not a complete sentence, please write questions and answers ",
        " that are appropriate to the sentence's meaning to the best of your ability. "),
    <.p("If you are not sure about certain cases, please check the examples.")
  )

  val generationQuestionFormat = <.div(
    <.p(""" This section is just for reference to help you understand the format of the questions.
        They all will be formed by filling slots like in the table below.
        The set of words you may use in each slot may depend on the words you wrote in the previous slots."""),
    <.table(
      ^.classSet1("table"),
      <.thead(
        <.tr(
          <.th("Wh-word"), <.th("Auxiliary"), <.th("Subject"), <.th("Verb"), <.th("Object"), <.th("Preposition"), <.th("Misc")
        )
      ),
      <.tbody(
        <.tr(<.td("Who"), <.td(), <.td(), <.td("blamed"), <.td("someone"), <.td(), <.td()),
        <.tr(<.td("What"), <.td("did"), <.td("someone"), <.td("blame"), <.td("something"), <.td("on"), <.td()),
        <.tr(<.td("Who"), <.td(), <.td(), <.td("refused"), <.td(), <.td("to"), <.td("do something")),
        <.tr(<.td("When"), <.td("did"), <.td("someone"), <.td("refuse"), <.td(), <.td("to"), <.td("do something")),
        <.tr(<.td("Who"), <.td("might"), <.td(), <.td("resume"), <.td("something"), <.td(), <.td())
      )
    )
  )

  val generationControls = <.div(
    <.ul(
      <.li(
        <.span(Styles.bolded, "Questions & Autocomplete. "),
        """You can write questions by typing or selecting items in the autocomplete menu.
        You may navigate the menu using the mouse or the up & down arrows and enter key.
        It may be easiest to start off with the mouse and then switch to typing when you get used to the question format.
        You can use tab and shift+tab to switch between questions."""),
      <.li(
        <.span(Styles.bolded, "Auto-suggest. "),
        """Once you have written at least one question,
        the autocomplete dropdown will start proposing complete questions.
        The suggestions are based on the structure of your previous questions, so to get the most out of them,
        write questions with more structure (e.g., "Who looked at someone?") rather than
        less (e.g., "Who looked?").
        """),
      <.li(
        <.span(Styles.bolded, "Answers. "),
        "To highlight an answer, first click on the first word in the answer, which will turn ",
        <.span(^.backgroundColor := "#FF8000", "orange"),
        ". Then click on the last word in the answer (which may be the same word) and the whole phrase will turn ",
        <.span(^.backgroundColor := "#FFFF00", "yellow"),
        ". (You may also click them in the opposite order.) You can highlight multiple answers to the same question in this way. ",
        " To delete an answer, click on a word in that answer while it is highlighted yellow. ",
        """ None of your answers may overlap with each other; answers to questions other than the currently selected one
        will be highlighted in """,
        <.span(^.backgroundColor := "#DDDDDD", "grey"), ".")),
    <.p("""
      When a question-answer pair is complete (the question is finished and it has at least one answer),
      its input field will turn """,
        <.span(^.backgroundColor := "rgba(0, 255, 0, 0.3)", "green"),
        """. If it violates the required formatting, it will turn """, <.span(
          ^.backgroundColor := "rgba(255, 0, 0, 0.3)", "red"
        ), """. If it is a repeat of a previous question, it will turn """, <.span(
          ^.backgroundColor := "rgba(255, 255, 0, 0.3)", "yellow"
        ), """. Only complete (green) question-answer pairs will count towards your requirements and bonus. """
    )
  )

  val generationConditions = <.div(
    <.p(s"""Each question-answer pair after the first will earn you a bonus:
          ${dollarsToCents(generationReward)}c for the second question, ${dollarsToCents(generationReward) + 1}c for the third
          then ${dollarsToCents(generationReward) + 2}c, etc.
          While at least one is required to submit the HIT,
          you will need to write more than ${generationCoverageQuestionsPerVerbThreshold} questions on average in order to stay qualified.
          On average, it should take less than 30 seconds per question-answer pair, and be much quicker with practice.
          """),
    <.p("""Your questions will be evaluated by other annotators, and """,
        <.b(""" you will only be awarded bonuses for your valid question-answer pairs. """),
        s""" (However, your questions-per-verb average will include invalid questions.)
          The bonus will be awarded as soon as validators have checked all of your question-answer pairs,
          which will happen shortly after you submit (but will vary depending on worker availability).
          Your accuracy will be updated as your questions are validated
          and shown to you just below the task interface.
          (Note that the validators will sometimes make mistakes,
          so there is an element of randomness to it: don't read too deeply into small changes in your accuracy.)
          If this number drops below ${(100 * generationAccuracyBlockingThreshold).toInt},
          you will be disqualified from this task. """)
  )

  override val generationInstructions = <.div(
    Instructions(
      InstructionsProps(
        instructionsId = "instructions",
        collapseCookieId = "generationCollapseCookie",
        tabs = List(
          "Overview" -> generationOverview,
          "Interface & Controls" -> generationControls,
          "Question Format" -> generationQuestionFormat,
          "Conditions & Bonuses" -> generationConditions,
          "Examples" -> <.div(examples)
        )
      )
    )
  )

  val validationOverview = <.div(
    <.p(Styles.badRed, """Read through all of the instructions and make sure you understand the interface controls before beginning. A full understanding of the requirements will help maximize your agreement with other workers so you can retain your qualification."""),
    <.p(s"""This task is for an academic research project by the natural language processing group at the University of Washington.
           We wish to deconstruct the meanings of English sentences into lists of questions and answers.
           You will be presented with a selection of English text and a list of questions prepared by other annotators."""),
    <.p("""You will highlight the words in the sentence that correctly answer each question,
           as well as mark whether questions are invalid.""",
      <.b(""" Note: it takes exactly 2 clicks to highlight each answer; see the Controls tab for details. """),
        """For example, consider the following sentence:"""),
    <.blockquote(
      ^.classSet1("blockquote"),
      "Protesters ", <.span(Styles.bolded, " blamed "), " the corruption scandal on local officials, who today ",
      " refused to promise that they would resume the investigation before year's end. "),
    <.p("""You should choose all of the following answers:"""),
    <.ul(
      <.li("Who blamed someone? --> ", <.span(Styles.goodGreen, " Protesters ")),
      <.li("Who did someone blame something on? --> ", <.span(Styles.goodGreen, " local officials / they")),
      <.li("What did someone blame on someone? --> ", <.span(Styles.goodGreen, " the corruption scandal"))),
    <.p(s"""You will be paid a ${dollarsToCents(validationBonusPerQuestion)}c bonus per question after the first $validationBonusThreshold questions if there are more than $validationBonusThreshold."""),
    <.h2("""Guidelines"""),
    <.ol(
      <.li(
        <.span(Styles.bolded, "Correctness. "),
        """Each answer must satisfy the litmus test that if you substitute it back into the question,
           the result is a grammatical statement, and it is true according to the sentence given. For example, """,
        <.span(Styles.bolded, "Who blamed someone? --> Protesters"), """ becomes """,
        <.span(Styles.goodGreen, "Protesters blamed someone, "), """ which is valid, while """,
        <.span(Styles.bolded, "Who blamed? --> Protesters"), """ would become """,
        <.span(Styles.badRed, "Protesters blamed, "), s""" which is ungrammatical, so it is invalid.
           Your responses will be compared to other annotators, and you must agree with them
           ${(100.0 * validationAgreementBlockingThreshold).toInt}% of the time in order to remain qualified. """),
      <.li(
        <.span(Styles.bolded, "Verb-relevance. "),
        """ Answers to the questions must pertain to the participants, time, place, reason, etc., of """,
        <.span(Styles.bolded, " the target verb in the sentence, "),
        " which is bolded and colored blue in the interface. ",
        """ For example, if the sentence is """,
        <.span(Styles.bolded,
               " He ",
               <.span(Styles.niceBlue, Styles.underlined, "promised"),
               " to come tomorrow "),
        """ and the question is """,
        <.span(Styles.badRed, " When did someone promise to do something? "),
        """ you must mark it """,
        <.span(Styles.badRed, " Invalid "),
        """ because the time mentioned, """, <.i(" tomorrow, "), " is ", <.i(" not "),
        " the time that he made the promise, but rather the time that he might come."),
      <.li(
        <.span(Styles.bolded, "Exhaustiveness. "),
        s"""You must provide every possible answer to each question.
           When highlighting answers, please only include the necessary words to provide a complete, grammatical answer,
           but if all else is equal, prefer to use longer answers.
           Also please include pronouns in the sentence that refer an answer you've already given.
           However, note that none of the answers to your questions may overlap.
           If the only possible answers to a question were already used for previous questions, please mark it invalid."""
      )
    ),
    <.p(" All ungrammatical questions should be counted invalid. However, ",
        " If the sentence has grammatical errors or is not a complete sentence, please answer ",
        " questions according to the sentence's meaning to the best of your ability. "),
    <.p("Please read through the examples if you need more details.")
  )

  val validationControls = <.div(
    <.ul(
      <.li(
        <.span(Styles.bolded, "Navigation. "),
        "Change questions using the mouse, the up and down arrow keys, or W and S."),
      <.li(
        <.span(Styles.bolded, "Invalid Questions. "),
        "Click the button labeled \"Invalid\" or press the space bar to toggle a question as invalid."),
      <.li(
        <.span(Styles.bolded, "Answers. "),
        "To highlight an answer, first click on the first word in the answer, which will turn ",
        <.span(^.backgroundColor := "#FF8000", "orange"),
        ". Then click on the last word in the answer (which may be the same word) and the whole phrase will turn ",
        <.span(^.backgroundColor := "#FFFF00", "yellow"),
        ". (You may also click them in the opposite order.) You can highlight multiple answers to the same question in this way. ",
        " To delete an answer, click on a word in that answer while it is highlighted yellow. ",
        """ None of your answers may overlap with each other; answers to questions other than the currently selected one
        will be highlighted in """,
        <.span(^.backgroundColor := "#DDDDDD", "grey"), "."))
  )

  val validationConditions = <.div(
    <.p(s"""You will be paid a bonus of ${dollarsToCents(validationBonusPerQuestion)}c
        for every question beyond $validationBonusThreshold, which will be paid when the assignment is approved.
        Your judgments will be cross-checked with other workers,
        and your agreement rate will be shown to you in the interface.
        If this number drops below ${(100 * validationAgreementBlockingThreshold).toInt}
        you will no longer qualify for the task.
        (Note that other validators will sometimes make mistakes,
        so there is an element of randomness to it: don't read too deeply into small changes in your agreement rate.)
        Your work will be approved and the bonus will be paid within an hour.""")
  )


  override val validationInstructions = <.div(
    Instructions(
      InstructionsProps(
        instructionsId = "instructions",
        collapseCookieId = "validationCollapseCookie",
        tabs = List(
          "Overview" -> validationOverview,
          "Controls" -> validationControls,
          "Conditions & Payment" -> validationConditions,
          "Examples" -> examples
        )
      )
    )
  )
}
