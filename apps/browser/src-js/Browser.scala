package qasrl.apps.browser

import scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.ext.KeyCode

import scala.collection.immutable.SortedSet

import scala.concurrent.ExecutionContext.Implicits.global

import cats.Id
import cats.Order
import cats.data.NonEmptyList
import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle._
import monocle.function.{all => Optics}
import monocle.macros._

import jjm.OrWrapped
import jjm.LowerCaseString
import jjm.ling.ESpan
import jjm.ling.Text
import jjm.ui._
import jjm.implicits._

import qasrl.bank.AnnotationRound
import qasrl.bank.AnswerSource
import qasrl.bank.ConsolidatedSentence
import qasrl.bank.DataIndex
import qasrl.bank.DatasetPartition
import qasrl.bank.Document
import qasrl.bank.DocumentId
import qasrl.bank.DocumentMetadata
import qasrl.bank.Domain
import qasrl.bank.QuestionSource
import qasrl.bank.SentenceId

import qasrl.bank.service.DocumentService
// import qasrl.bank.service.Search

import qasrl.data.AnswerLabel
import qasrl.data.AnswerJudgment
import qasrl.data.Answer
import qasrl.data.InvalidQuestion
import qasrl.data.Sentence
import qasrl.data.VerbEntry
import qasrl.data.QuestionLabel

object Browser {

  val StateLocal = new LocalState[State]
  val IndexFetch = new CacheCallContent[Unit, DataIndex]
  val SearchFetch = new CacheCallContent[Set[LowerCaseString], Set[DocumentId]]
  val DocMetaLocal = new LocalState[DocumentMetadata]
  val DocFetch = new CacheCallContent[DocumentId, Document]
  val SentLocal = new LocalState[ConsolidatedSentence]
  val DivReference = new Reference[html.Div]
  val BoolLocal = new LocalState[Boolean]
  val OptIntLocal = new LocalState[Option[Int]]
  val QuestionLabelSetLocal = new LocalState[Set[QuestionLabel]]

  case class Props(
    qasrl: DocumentService[OrWrapped[AsyncCallback, *]]
  )

  @Lenses case class Search(
    text: String,
    query: Set[LowerCaseString]
  )
  object Search {
    def initial = Search("", Set.empty[LowerCaseString])
  }

  @Lenses case class Slices(
    original: Boolean,
    expansion: Boolean,
    eval: Boolean,
    qaNom: Boolean,
  )
  object Slices {
    val initial = Slices(true, true, true, true)
  }

  @Lenses case class Filters(
    partitions: Set[DatasetPartition],
    domains: Set[Domain],
    slices: Slices,
    validOnly: Boolean,
    qaNomOnly: Boolean
  )
  object Filters {
    def initial = Filters(
      Set(DatasetPartition.Dev),
      Set(Domain.Wikinews),
      Slices.initial,
      false, false
    )
  }
  implicit class FiltersLenses(val fs: Filters.type) extends AnyVal {
    def atPart(part: DatasetPartition) = Optics.at(part)(monocle.function.At.atSet)
    def train     = fs.partitions composeLens atPart(DatasetPartition.Train)
    def dev       = fs.partitions composeLens atPart(DatasetPartition.Dev)
    def test      = fs.partitions composeLens atPart(DatasetPartition.Test)

    def atDomain(domain: Domain) = Optics.at(domain)(monocle.function.At.atSet)
    def wikipedia = fs.domains composeLens atDomain(Domain.Wikipedia)
    def wikinews  = fs.domains composeLens atDomain(Domain.Wikinews)
    def tqa       = fs.domains composeLens atDomain(Domain.TQA)
  }

  def getCurDocuments(
    index: DataIndex,
    searchedIds: Set[DocumentId],
    filter: Filters,
    denseIds: Set[DocumentId],
    qaNomIds: Set[DocumentId]
  ) = {
    filter.partitions.iterator.map(
      part => index.documents(part)
        .filter(doc =>
        (!filter.qaNomOnly || qaNomIds.contains(doc.id)) &&
          filter.domains.contains(doc.id.domain) &&
          searchedIds.contains(doc.id) &&
          (filter.slices.original || filter.slices.expansion || (filter.slices.eval && denseIds.contains(doc.id)) || (filter.slices.qaNom && qaNomIds.contains(doc.id)))
      )
    ).foldLeft(SortedSet.empty[DocumentMetadata])(_ union _)
  }

  @Lenses case class State(
    search: Search,
    filter: Filters
  )
  object State {
    def initial(props: Props) = {
      val search = Search.initial
      val filter = Filters.initial
      State(search, filter)
    }
  }

  val transparent = Rgba(255, 255, 255, 0.0)
  val queryKeywordHighlightLayer = Rgba(255, 255, 0, 0.4)

  val highlightLayerColors = List(
    // Rgba(255, 255,   0, 0.2), // yellow
    Rgba(  0, 128, 255, 0.1), // green-blue
    Rgba(255,   0, 128, 0.1), // magenta?
    Rgba( 64, 192,   0, 0.1), // something. idk
    Rgba(128,   0, 255, 0.1), // mystery
    Rgba(  0, 255, 128, 0.1)  // blue-green
  )

  def checkboxToggle[A](
    label: String,
    isValueActive: StateSnapshot[Boolean]
  ) = <.div(
    <.input(S.checkbox)(
      ^.`type` := "checkbox",
      ^.value := label,
      ^.checked := isValueActive.value,
      ^.onChange --> isValueActive.modState(!_)
    ),
    <.span(S.checkboxLabel)(
      label
    )
  )

  def searchPane(search: StateSnapshot[Search]) = {
    val query = search.value.text.split("\\s+")
      .map(_.trim).toSet
      .filter(_.nonEmpty)
      .map((s: String) => s.lowerCase)
    <.div(S.searchContainer)(
      <.input(S.searchInput)(
        ^.`type` := "text",
        ^.placeholder := "Search sentences",
        ^.value := search.value.text,
        ^.onChange ==> ((e: ReactEventFromInput) => search.zoomStateL(Search.text).setState(e.target.value)),
        ^.onKeyDown ==> (
          (e: ReactKeyboardEvent) => {
            CallbackOption.keyCodeSwitch(e) {
              case KeyCode.Enter => search.zoomStateL(Search.query).setState(query)
            }
          }
        )
      ),
      <.button(S.searchSubmitButton)(
        ^.disabled := !search.value.query.isEmpty,
        ^.onClick --> search.zoomStateL(Search.query).setState(query),
        "Submit"
      ),
      <.button(S.searchClearButton)(
        ^.disabled := search.value.query.isEmpty,
        ^.onClick --> search.zoomStateL(Search.query).setState(Set.empty[LowerCaseString]),
        "Clear"
      )
      // s"Current query: ${search.get.keywords.fold("")(_.mkString(", "))}"
    )
  }

  def filterPane(filter: StateSnapshot[Filters]) = {
    val slices = filter.zoomStateL(Filters.slices)
    <.div(S.filterContainer)(
      <.div(S.partitionChooser)(
        checkboxToggle("Train", filter.zoomStateL(Filters.train)),
        checkboxToggle("Dev",   filter.zoomStateL(Filters.dev)),
        checkboxToggle("Test",  filter.zoomStateL(Filters.test))(^.visibility := "hidden")
      ),
      <.div(S.domainChooser)(
        checkboxToggle("Wikipedia", filter.zoomStateL(Filters.wikipedia)),
        checkboxToggle("Wikinews",  filter.zoomStateL(Filters.wikinews)),
        checkboxToggle("TQA",       filter.zoomStateL(Filters.tqa))
      ),
      <.div(S.sliceChooser)(
        checkboxToggle("Original",  slices.zoomStateL(Slices.original)),
        checkboxToggle("Expansion", slices.zoomStateL(Slices.expansion)),
        checkboxToggle("Eval",      slices.zoomStateL(Slices.eval)),
        checkboxToggle("QANom",     slices.zoomStateL(Slices.qaNom)),
      )
    )
  }

  val helpModalId = "help-modal"
  val helpModalLabelId = "help-modal-label"
  val dataToggle = VdomAttr("data-toggle")
  val dataTarget = VdomAttr("data-target")
  val ariaLabelledBy = VdomAttr("aria-labelledby")
  val ariaHidden = VdomAttr("aria-hidden")
  val dataDismiss = VdomAttr("data-dismiss")
  val ariaLabel = VdomAttr("aria-label")

  def helpModal = {
    <.div(^.id := helpModalId)(
      S.helpModal, ^.tabIndex := -1, ^.role := "dialog",
      ariaLabelledBy := helpModalLabelId, ariaHidden := true
    )(
      <.div(S.helpModalDialog, ^.role := "document")(
        <.div(S.helpModalContent)(
          <.div(S.helpModalHeader)(
            <.h1(S.helpModalTitle)(
              ^.id := helpModalLabelId,
              "QA-SRL + QANom Browser"
            ),
            <.button(S.helpModalHeaderCloseButton)(
              ^.`type` := "button", dataDismiss := "modal", ariaLabel := "Close",
              <.span(ariaHidden := true, "×")
            )
          ),
          <.div(S.helpModalBody)(
            <.p(
              "This page presents the consolidated data from two datasets: "
            ),
            <.ol(
              <.li(
                "The ", <.em("QA-SRL Bank 2.0"), ", presented in ",
                <.a(^.href := "https://www.aclweb.org/anthology/P18-1191/", "Large-Scale QA-SRL Parsing"),
                ", by Nicholas FitzGerald, Julian Michael, Luheng He, and Luke Zettlemoyer, ",
                " at ACL 2018. "
              ),
              <.li(
                <.em("QANom"), ", presented in ",
                <.a(^.href := "https://www.aclweb.org/anthology/2020.coling-main.274/",
                    "QANom: Question-Answer driven SRL for Nominalizations"),
                """, by Ayal Klein, Jonathan Mamou, Valentina Pyatkin, Daniela Stepanov,
                    Hangfeng He, Dan Roth, Luke Zettlemoyer, and Ido Dagan,
                    presented at COLING 2020.""",
              )
            ),
            <.p(
              "Several stages of annotations have been done on over >64,000 sentences in 3 domains. ",
              "In this data browser, you can explore how the data looks at each stage and in each domain. ",
              "A full description of the interface is given below. "
            ),
            <.p(S.helpWarningAlert)(
              <.b("Warning: "), "This interface will be messed up on mobile or if your window is too narrow. ",
              "If the interface seems laid out wrong, try zooming out in your browser (e.g., cmd+hyphen on Chrome on Mac) ",
              "or widening your browser window. "
            ),
            <.h4("About the Data"),
            <.p(
              "QA-SRL reframes the traditional problem of Semantic Role Labeling into one of writing questions and answers. ",
              "The questions are taken from a restrictive template centered around a verb being targeted as the predicate, ",
              "and answers are contiguous spans of tokens from the sentence. ",
              "In the QA-SRL Bank 2.0, verbs were chosen from the sentence as predicates. ",
              "QANom extended this by targeting nominalizations of verbs as well. ",
              "In the QA-SRL Bank 2.0, at least 3 annotators answer each question as part of quality control. ",
              "In QANom, a single trained annotator provided each set of question-answer pairs on the training set, ",
              "whereas two annotators provided them for dev and test, with a third one consolidating their annotations together. ",
              "Annotators could also mark questions as invalid, or in the case of QANom, mark nouns as not evoking a verbal predicate. "
            ),
            <.p(
              "We have data in 3 domains: "
            ),
            <.ul(
              <.li(
                <.a(^.href := "https://en.wikipedia.org", "Wikipedia"), ", "
              ),
              <.li(
                <.a(^.href := "https://en.wikinews.org",  "Wikinews"),  ", and "
              ),
              <.li(
                " science textboooks (from the ",
                <.a(^.href := "http://data.allenai.org/tqa/",  "Textbook Question Answering"),  " dataset)."
              )
            ),
            <.p(
              "QANom only covers Wikipedia and Wikinews. ",
              "Four rounds of annotated data are included; the first are from the QA-SRL Bank 2.0: "
            ),
            <.ul(
              <.li(
                <.b("Original"), ": workers on Mechanical Turk wrote and answered questions for each verb; "
              ),
              <.li(
                <.b("Expansion"), ": a model trained on the original round overgenerated questions, which turkers answered; "
              ),
              <.li(
                <.b("Eval"), ": for a small subset of dev and test, we overgenerated questions from our baseline models ",
                "and had annotators answer them at twice the original density (from 3 to 6 answer judgments per question); and, "
              ),
              <.li(
                <.b("QANom"), ": annotators wrote questions for nominalized verbs as part of the QANom dataset. "
              )
            ),
            <.p(
              "In the QA-SRL Bank 2.0, workers could mark questions as invalid, so our convention is to count a question as valid if at least ",
              "5/6 of its questions are valid — so all 3, in the case of 3 answers, or 5 out of 6 in the case of 6."
            ),
            <.h4("Filters"),
            <.ul(
              <.li(
                <.b("Train / Dev: "),
                "Toggle display of documents in the train and dev partitions."
              ),
              <.li(
                <.b("Wikipedia / Wikinews / TQA: "),
                "Toggle display of documents in the each of the three domains."
              ),
              <.li(
                <.b("Original / Expansion / Eval / QANom: "),
                "Toggle the display of data based on what round it was written in. ",
                "The eval filter will include all questions and answers (including from the other two rounds) ",
                "that were written for the sentences passed through the evaluation stage, whereas ",
                "for questions introduced in the original and expansion stages, only answers from those stages will be included, ",
                "even if those questions were also passed through the eval stage."
              ),
              <.li(
                <.b("Valid only: "),
                "Filter out questions that would be counted invalid by our heuristic, based on the set of answers included ",
                "according to the current filters. ",
              ),
              <.li(
                <.b("QANom sentences only: "),
                "Only show documents and sentences which have QANom annotations."
              ),
            ),
            <.p(
              "The questions introduced in the expansion and eval stages are very often ungrammatical or invalid, ",
              "because they were produced by ", <.i("overgenerating "), "questions from our models. ",
              "Questions were included which were assigned probabilities as low as 0.2. ",
              "See ",
              <.a(^.href := "https://www.aclweb.org/anthology/P18-1191/", "Large-Scale QA-SRL Parsing"),
              " for more details. "
            ),
            <.h4("Keyword Search"),
            <.p(
              "You may search the dataset for specific words by typing a query in the Keyword Search field and pressing enter. ",
              "The interface will then show only documents and sentences containing that word. ",
              "This does not search through document titles (use cmd+F or ctrl+F for that), ",
              "is case-insensitive, and matches all inflected forms of a verbal predicate. "
            ),
            <.h4("Data Display"),
            <.p(
              "When a sentence is selected, all answer spans admitted by the current filters ",
              "will be highlighted in that sentence in the main display. ",
              "Spans are highlighted with low opacity, so stronger colors ",
              "indicate more than one worker highlighting a span or word. ",
              "Spans are color-coded according to the verb whose question they were used to answer. ",
              "Hovering the mouse over a verb or its entry in the data table will restrict the highlighted spans to ",
              "answers of questions written for that verb. ",
              "Clicking on a verb in the sentence will scroll the display to the QA pairs for that verb. "
            ),
            <.p(
              "Questions are also color-coded with a vertical line on their left, ",
              "based on the annotation round in which they were first introduced: ",
              <.span(S.originalLegendMark)("m"),
              <.span(" Original, "),
              <.span(S.expansionLegendMark)("m"),
              <.span(" Expansion, "),
              <.span(S.evalLegendMark)("m"),
              <.span(" Eval, and"),
              <.span(S.qaNomLegendMark)("m"),
              <.span(" QANom.")
            ),
            <.p(
              "Finally, for full details on the sources of each question and answer, you can click on a question or answer ",
              "and these details will appear below it in the table. Click on it again and they will disappear. "
            )
          ),
          <.div(S.helpModalFooter)(
            <.button(S.helpModalFooterCloseButton)(
              ^.`type` := "button", dataDismiss := "modal")(
              "Close"
            )
          )
        )
      )
    )
  }

  def legendPane(validOnly: StateSnapshot[Boolean], qaNomOnly: StateSnapshot[Boolean]) = {
    <.div(S.legendContainer)(
      <.div(S.legendTitle)(
        <.span(S.legendTitleText)("Legend "),
        <.span(S.legendTitleLinkText)(
          <.a(
            ^.href := "#", "(help)",
            dataToggle := "modal",
            dataTarget := s"#$helpModalId"
          )
        ),
        ": ",
        <.div(S.originalLegendMark)("m"),
        <.span(" Original "),
        <.div(S.expansionLegendMark)("m"),
        <.span(" Expansion "),
        <.div(S.evalLegendMark)("m"),
        <.span(" Eval"),
        <.div(S.qaNomLegendMark)("m"),
        <.span(" QANom")
      ),
      <.div(S.validityLegend)(
        checkboxToggle("Valid only ", validOnly)(
          ^.marginLeft := "20px", ^.display := "inline"
        ),
        <.span("("),
        <.span(S.invalidValidityText)("≤ 4/6 ➔ invalid"),
        <.span(", "),
        <.span(S.validValidityText)("≥ 5/6 ➔ valid"),
        <.span(")")
      ),
      <.div(S.qaNomFilterLegend)(
        checkboxToggle("QANom sentences only ", qaNomOnly)(
          ^.marginLeft := "20px", ^.display := "inline"
        )
      ),
      <.div(S.highlightLegend)(
        <.span("Answer provided by "),
        (1 to 6).flatMap { i =>
          val colorStr = NonEmptyList(
            transparent,
            List.fill(i)(highlightLayerColors.head)
          ).reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
          List(
            <.span(^.key := s"slashafterlegend-$i", "/"),
            <.span(S.legendColorIndicator)(
              ^.key := s"legendnum-$i",
              ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
              f"$i%d"
            )
          )
        }.tail.toVdomArray(x => x),
        <.span(" annotators")
      )
    )
  }

  def headerPane(state: StateSnapshot[State]) = {
    <.div(S.headerContainer)(
      <.div(S.titleAndSearchContainer)(
        <.h1(S.title)("QA-SRL + QANom"),
        searchPane(state.zoomStateL(State.search))
      ),
      filterPane(state.zoomStateL(State.filter)),
      legendPane(
        state.zoomStateL(State.filter.composeLens(Filters.validOnly)),
        state.zoomStateL(State.filter.composeLens(Filters.qaNomOnly))
      )
    )
  }

  def getCurSentences(
    allSentences: SortedSet[ConsolidatedSentence],
    query: Set[LowerCaseString],
    denseIds: Set[SentenceId],
    qaNomIds: Set[SentenceId],
    slices: Slices
  ) = {
    val searchFilteredSentences = if(query.isEmpty) {
      allSentences
    } else {
      allSentences.filter { sent =>
        qasrl.bank.service.Search.getQueryMatchesInSentence(sent, qasrl.bank.service.Search.Query(None, query)).nonEmpty
      }
    }
    val sliceFilteredSentences = allSentences.filter { sent =>
      val sid = SentenceId.fromString(sent.sentenceId)
      slices.original || slices.expansion ||
        (slices.eval && denseIds.contains(sid)) ||
        (slices.qaNom && qaNomIds.contains(sid))
    }
    searchFilteredSentences.intersect(sliceFilteredSentences)
  }

  def getRoundsForQuestion(label: QuestionLabel): SortedSet[AnnotationRound] = {
    val qSources = label.questionSources.map(s => QuestionSource.fromString(s): QuestionSource)
    SortedSet(
      qSources.map {
        case QuestionSource.QANomTurker(_) => AnnotationRound.QANom
        case QuestionSource.QasrlTurker(_) => AnnotationRound.Original
        case QuestionSource.Model(_)  =>
          val hasAnswersInExpansion = label.answerJudgments.map(_.sourceId).exists(s =>
            AnswerSource.fromString(s).round == AnnotationRound.Expansion
          )
          if(hasAnswersInExpansion) AnnotationRound.Expansion else AnnotationRound.Eval
      }.toSeq: _*
    )
  }

  import cats.Order.catsKernelOrderingForOrder

  implicit val qasrlDataQuestionLabelOrder: Order[QuestionLabel] = Order.whenEqual(
    Order.by[QuestionLabel, SortedSet[AnnotationRound]](getRoundsForQuestion _),
    Order.by[QuestionLabel, String](_.questionString)
  )

  sealed trait SpanColoringSpec {
    def spansWithColors: List[(ESpan, Rgba)]
  }
  case class RenderWholeSentence(val spansWithColors: List[(ESpan, Rgba)]) extends SpanColoringSpec
  case class RenderRelevantPortion(spansWithColorsNel: NonEmptyList[(ESpan, Rgba)]) extends SpanColoringSpec {
    def spansWithColors = spansWithColorsNel.toList
  }

  def renderSentenceWithHighlights(
    sentenceTokens: Vector[String],
    coloringSpec: SpanColoringSpec,
    wordRenderers: Map[Int, VdomTag => VdomTag] = Map()
  ) = {
    val containingSpan = coloringSpec match {
      case RenderWholeSentence(_) =>
        ESpan(0, sentenceTokens.size)
      case RenderRelevantPortion(swcNel) =>
        val spans = swcNel.map(_._1)
        ESpan(spans.map(_.begin).minimum, spans.map(_.end).maximum)
    }
    val wordIndexToLayeredColors = (containingSpan.begin until containingSpan.end).map { i =>
      i -> coloringSpec.spansWithColors.collect {
        case (span, color) if span.contains(i) => color
      }
    }.toMap
    val indexAfterToSpaceLayeredColors = ((containingSpan.begin + 1) to containingSpan.end).map { i =>
      i -> coloringSpec.spansWithColors.collect {
        case (span, color) if span.contains(i - 1) && span.contains(i) => color
      }
    }.toMap
    Text.renderTokens[Int, List, List[VdomElement]](
      words = sentenceTokens.indices.toList,
      getToken = (index: Int) => sentenceTokens(index),
      spaceFromNextWord = (nextIndex: Int) => {
        if(!containingSpan.contains(nextIndex) || nextIndex == containingSpan.begin) List() else {
          val colors = indexAfterToSpaceLayeredColors(nextIndex)
          val colorStr = NonEmptyList[Rgba](transparent, colors)
            .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
          List(
            <.span(
              ^.key := s"space-$nextIndex",
              ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
              " "
            )
          )
        }
      },
      renderWord = (index: Int) => {
        if(!containingSpan.contains(index)) List() else {
          val colorStr = NonEmptyList(transparent, wordIndexToLayeredColors(index))
            .reduce((x: Rgba, y: Rgba) => x add y).toColorStyleString
          val render: (VdomTag => VdomTag) = wordRenderers.get(index).getOrElse((x: VdomTag) => x)
          val element: VdomTag = render(
            <.span(
              ^.style := js.Dynamic.literal("backgroundColor" -> colorStr),
              Text.normalizeToken(sentenceTokens(index))
            )
          )
          List(element(^.key := s"word-$index"))
        }
      }
    ).toVdomArray(x => x)
  }

  def makeAllHighlightedAnswer(
    sentenceTokens: Vector[String],
    answers: NonEmptyList[Answer],
    color: Rgba
  ): VdomArray = {
    val orderedSpans = answers.flatMap(_.spans.toNonEmptyList).sorted
    case class GroupingState(
      completeGroups: List[NonEmptyList[ESpan]],
      currentGroup: NonEmptyList[ESpan]
    )
    val groupingState = orderedSpans.tail.foldLeft(GroupingState(Nil, NonEmptyList.of(orderedSpans.head))) {
      case (GroupingState(groups, curGroup), span) =>
        if(curGroup.exists(_.overlaps(span))) {
          GroupingState(groups, span :: curGroup)
        } else {
          GroupingState(curGroup :: groups, NonEmptyList.of(span))
        }
    }
    val contigSpanLists = NonEmptyList(groupingState.currentGroup, groupingState.completeGroups)
    val answerHighlighties = contigSpanLists.reverse.map(spanList =>
      List(
        <.span(
          renderSentenceWithHighlights(sentenceTokens, RenderRelevantPortion(spanList.map(_ -> color)))
        )
      )
    ).intercalate(List(<.span(" / ")))
    answerHighlighties.zipWithIndex.toVdomArray { case (a, i) =>
      a(^.key := s"answerString-$i")
    }
  }

  def shouldAnswerBeIncluded(
    source: AnswerSource,
    slices: Slices
  ): Boolean = {
    // show answers from other rounds as well when looking at eval round
    import AnnotationRound._
    source.round match {
      case Original => slices.original || slices.eval
      case Expansion => slices.expansion || slices.eval
      case Eval => slices.eval
      case QANom => slices.qaNom
    }
  }

  def isQuestionValid(
    label: QuestionLabel,
    slices: Slices
  ): Boolean = {
    val includedJudgments = label.answerJudgments.filter(aj =>
      shouldAnswerBeIncluded(AnswerSource.fromString(aj.sourceId), slices)
    )
    val numValidJudgments = includedJudgments.count(_.judgment.isAnswer)
    numValidJudgments.toDouble / includedJudgments.size > (4.99 / 6.0)
  }

  val colspan = VdomAttr("colspan")

  def qaLabelRow(
    sentence: ConsolidatedSentence,
    label: QuestionLabel,
    slices: Slices,
    color: Rgba,
    toggleQ: Callback,
    fullDetail: Boolean
  ) = {
    val answerJudgments = label.answerJudgments.filter { aj =>
      shouldAnswerBeIncluded(AnswerSource.fromString(aj.sourceId), slices)
    }
    val qSource = label.questionSources.map(s => QuestionSource.fromString(s): QuestionSource).min
    val roundIndicatorStyle = qSource match {
      case QuestionSource.QANomTurker(_) => S.qaNomRoundIndicator
      case QuestionSource.QasrlTurker(_) => S.originalRoundIndicator
      case QuestionSource.Model(_)  =>
        val hasAnswersInExpansion = label.answerJudgments.map(_.sourceId).exists(s =>
          AnswerSource.fromString(s).round == AnnotationRound.Expansion
        )
        if(hasAnswersInExpansion) S.expansionRoundIndicator else S.evalRoundIndicator
    }
    if(!fullDetail) {
      <.tr(S.qaPairRow)(
        ^.onClick --> toggleQ,
        <.td(roundIndicatorStyle),
        <.td(S.questionCell)(
          <.span(S.questionText)(
            label.questionString
          )
        ),
        <.td(S.validityCell) {
          val numJudgments = answerJudgments.size
          val numValidJudgments = answerJudgments.count(_.judgment.isAnswer)
          val isConsideredValid = isQuestionValid(label, slices)
            <.span(if(isConsideredValid) S.validValidityText else S.invalidValidityText)(
              s"$numValidJudgments/$numJudgments"
            )
        },
        <.td(S.answerCell)(
          <.span(S.answerText) {
            NonEmptyList.fromList(
              answerJudgments.toList.collect {
                case AnswerLabel(sourceId, Answer(spans)) => Answer(spans)
              }
            ).whenDefined { answersNel =>
              makeAllHighlightedAnswer(sentence.sentenceTokens, answersNel, color)
            }
          }
        )
      )
    } else {
      <.tr(S.qaPairRow)(
        ^.onClick --> toggleQ,
        <.td(roundIndicatorStyle),
        <.td(S.questionFullDescriptionCell)(
          ^.colSpan := 3
        ) {
          val questionSourceStr = label.questionSources
            .map(qs => QuestionSource.fromString(qs): QuestionSource)
            .map {
              case QuestionSource.QANomTurker(id) => s"QANom worker $id"
              case QuestionSource.QasrlTurker(id) => s"QA-SRL Bank 2.0 worker $id"
              case QuestionSource.Model(ver) => s"QA-SRL Bank 2.0 model ($ver)"
            }
            .mkString(", ")

          <.div(
            <.span(S.questionSourceText)(s"Written by $questionSourceStr"),
            <.table(
              <.tbody(
                label.answerJudgments.toList.sortBy(aj => AnswerSource.fromString(aj.sourceId)).toVdomArray {
                  case AnswerLabel(source, judgment) =>
                    val AnswerSource(id, round) = AnswerSource.fromString(source)
                    import AnnotationRound._
                    val roundIndicatorStyle = round match {
                      case Original  => S.originalRoundIndicator
                      case Expansion => S.expansionRoundIndicator
                      case Eval      => S.evalRoundIndicator
                      case QANom     => S.qaNomRoundIndicator
                    }
                    val roundString = round match {
                      case QANom => "QANom"
                      case _ => "QA-SRL Bank 2.0"
                    }
                    <.tr(
                      ^.key := s"fulldesc-$source-$judgment",
                      <.td(roundIndicatorStyle),
                      <.td(S.answerSourceIdCell)(s"$roundString worker $id"),
                      <.td(
                        judgment match {
                          case InvalidQuestion => <.span(S.invalidValidityText)("Invalid")
                          case Answer(spans) =>   <.span(
                            spans.toList.sorted.map(s =>
                              Text.renderSpan(sentence.sentenceTokens, s)
                            ).mkString(" / ")
                          )
                        }
                      )
                    )
                }
              )
            )
          )
        }
      )
    }
  }

  def shouldQuestionBeShown(
    label: QuestionLabel,
    slices: Slices,
    validOnly: Boolean
  ): Boolean = {
    (!validOnly || isQuestionValid(label, slices)) && {
      import AnnotationRound._
      val hasEvalAnswers = label.answerJudgments.map(aj => AnswerSource.fromString(aj.sourceId).round).contains(Eval)
      (hasEvalAnswers && slices.eval) || (
        getRoundsForQuestion(label).exists {
          case Original  => slices.original
          case Expansion => slices.expansion
          case Eval      => slices.eval
          case QANom     => slices.qaNom
        }
      )
    }
  }

  def verbEntryDisplay(
    curSentence: ConsolidatedSentence,
    verb: VerbEntry,
    slices: Slices,
    validOnly: Boolean,
    color: Rgba
    // anchorCorrectionPixels: Int
  ) = {
    <.div(S.verbEntryDisplay)(
      <.div(
        <.a(
          ^.name := s"verb-${verb.verbIndex}",
          ^.display := "block",
          ^.position := "relative",
          // ^.top := s"-${anchorCorrectionPixels}px",
          ^.visibility := "hidden"
        )
      ),
      <.div(S.verbHeading)(
        <.span(S.verbHeadingText)(
          ^.color := color.copy(a = 1.0).toColorStyleString,
          curSentence.sentenceTokens(verb.verbIndex)
        )
      ),
      <.table(S.verbQAsTable)(
        QuestionLabelSetLocal.make(initialValue = Set()) { selectedQs =>
          <.tbody(S.verbQAsTableBody){
            val questionLabels = verb.questionLabels.toList.map(_._2)
              .filter(l => shouldQuestionBeShown(l, slices, validOnly))
              .sorted
            if(questionLabels.isEmpty) {
              <.tr(<.td(<.span(S.loadingNotice)("All questions have been filtered out.")))
            } else questionLabels
              .flatMap { label =>
              val thisQToggled = selectedQs.zoomStateL(Optics.at(label))
              val toggleQ = thisQToggled.modState(!_)
              List(
                List(qaLabelRow(curSentence, label, slices, color, toggleQ, false)(^.key := s"short-${label.questionString}")),
                List(
                  <.tr(S.dummyRow)(^.key :=  s"dummy-${label.questionString}"),
                  qaLabelRow(curSentence, label, slices, color, toggleQ, true)(^.key :=  s"full-${label.questionString}"),
                ).filter(_ => thisQToggled.value)
              ).flatten
            }.toVdomArray(x => x)
          }
        }
      )
    )
  }

  def docSelectionPane(
    totalNumDocs: Int,
    curDocMetas: SortedSet[DocumentMetadata],
    curDocMeta: StateSnapshot[DocumentMetadata]
  ) = {
    <.div(S.documentSelectionPaneContainer)(
      <.div(S.documentCountLabel)(
        <.span(S.documentCountLabelText)(
          s"${curDocMetas.size} / $totalNumDocs documents ("
        ),
        <.span(S.documentCountLabelRandomText)(
          <.a(
            ^.href := "#",
            ^.onClick --> curDocMeta.setState(
              curDocMetas.iterator.drop(
                new scala.util.Random().nextInt(curDocMetas.size)
              ).next
            ),
            "random"
          )
        ),
        <.span(S.documentCountLabelText)(
          s")"
        )
      ),
      <.div(S.documentSelectionPane)(
        curDocMetas.toVdomArray { docMeta =>
          <.div(S.documentSelectionEntry)(
            ^.key := docMeta.id.toString,
            if(docMeta == curDocMeta.value) S.currentSelectionEntry else S.nonCurrentSelectionEntry,
            ^.onClick --> curDocMeta.setState(docMeta),
            <.span(S.documentSelectionEntryText)(
              docMeta.title
            )
          )
        }
      )
    )
  }

  def sentenceSelectionPane(
    numSentencesInDocument: Int,
    curSentences: SortedSet[ConsolidatedSentence],
    searchQuery: Set[LowerCaseString],
    curSentence: StateSnapshot[ConsolidatedSentence]
  ) = {
    val sentencesWord = if(numSentencesInDocument == 1) "sentence" else "sentences"
    val sentenceCountLabel = if(curSentences.size == numSentencesInDocument) {
      s"$numSentencesInDocument $sentencesWord"
    } else {
      s"${curSentences.size} / $numSentencesInDocument $sentencesWord"
    }

    <.div(S.sentenceSelectionPaneContainer)(
      <.div(S.sentenceCountLabel)(
        <.span(S.sentenceCountLabelText)(
          sentenceCountLabel
        )
      ),
      <.div(S.sentenceSelectionPane)(
        curSentences.toVdomArray { sentence =>
          val spanHighlights = qasrl.bank.service.Search.getQueryMatchesInSentence(sentence, qasrl.bank.service.Search.Query(None, searchQuery)).toList.map(index =>
            ESpan(index, index + 1) -> queryKeywordHighlightLayer
          )
          <.div(S.sentenceSelectionEntry)(
            ^.key := sentence.sentenceId,
            if(sentence == curSentence.value) S.currentSelectionEntry else S.nonCurrentSelectionEntry,
            ^.onClick --> curSentence.setState(sentence),
            <.span(S.sentenceSelectionEntryText)(
              renderSentenceWithHighlights(sentence.sentenceTokens, RenderWholeSentence(spanHighlights))
            )
          )
        }
      )
    )
  }

  def sentenceDisplayPane(
    part: DatasetPartition,
    docMeta: DocumentMetadata,
    sentence: ConsolidatedSentence,
    slices: Slices,
    validOnly: Boolean
  ) = {
    val sentenceId = SentenceId.fromString(sentence.sentenceId)
    val sortedVerbs = sentence.verbEntries.values.toVector.sortBy(_.verbIndex)
    val (sortedVerbsToDisplay, sortedVerbsWithNoQuestions) = sortedVerbs.partition(verb =>
      verb.questionLabels.values.exists(l =>
        shouldQuestionBeShown(l, slices, validOnly)
      )
    )
    val nonDisplayedVerbIndices = (
      sortedVerbsWithNoQuestions.map(_.verbIndex).toSet ++
        sentence.nonPredicates.keySet
    )
    OptIntLocal.make(initialValue = None) { highlightedVerbIndex =>
      val answerSpansWithColors = for {
        (verb, index) <- sortedVerbs.zipWithIndex
        if highlightedVerbIndex.value.forall(_ == verb.verbIndex)
        question <- verb.questionLabels.values.toList
        if shouldQuestionBeShown(question, slices, validOnly)
        answerLabel <- question.answerJudgments
        if shouldAnswerBeIncluded(AnswerSource.fromString(answerLabel.sourceId), slices)
        Answer(spans) <- answerLabel.judgment.getAnswer.toList
        span <- spans.toList
      } yield span -> highlightLayerColors(index % highlightLayerColors.size)
      val verbColorMap = sortedVerbs
        .zipWithIndex.map { case (verb, index) =>
          verb.verbIndex -> (
            if(nonDisplayedVerbIndices.contains(verb.verbIndex)) Rgba.black
            else highlightLayerColors(index % highlightLayerColors.size)
          )
      }.toMap

      <.div(S.sentenceDisplayPane)(
        <.div(S.sentenceInfoContainer)(
          <.span(S.sentenceInfoText) {
            val abbrevTitle = if(docMeta.title.length <= 30) docMeta.title else docMeta.title.take(27) + "..."
            s"$part / ${docMeta.id.domain} / ${docMeta.id.id} ($abbrevTitle) / paragraph ${sentenceId.paragraphNum}, sentence ${sentenceId.sentenceNum}"
          }
        ),
        <.div(S.sentenceTextContainer)(
          <.span(S.sentenceText)(
            renderSentenceWithHighlights(
              sentence.sentenceTokens,
              RenderWholeSentence(answerSpansWithColors.toList),
              verbColorMap.collect { case (verbIndex, color) =>
                verbIndex -> (
                  (v: VdomTag) => <.a(
                    S.verbAnchorLink,
                    ^.href := s"#verb-$verbIndex",
                    v(
                      ^.color := color.copy(a = 1.0).toColorStyleString,
                      ^.fontWeight := "bold",
                      ^.onMouseMove --> (
                        if(highlightedVerbIndex.value == Some(verbIndex)) {
                          Callback.empty
                        } else highlightedVerbIndex.setState(Some(verbIndex))
                      ),
                      ^.onMouseOut --> highlightedVerbIndex.setState(None)
                    )
                  )
                )
              }
            )
          )
        ),
        <.div(S.verbEntriesContainer)(
          sortedVerbsToDisplay.toVdomArray { verb =>
            verbEntryDisplay(sentence, verb, slices, validOnly, verbColorMap(verb.verbIndex))(
              S.hoverHighlightedVerbTable.when(highlightedVerbIndex.value.exists(_ == verb.verbIndex)),
              ^.key := verb.verbIndex,
              ^.onMouseMove --> (
                if(highlightedVerbIndex.value == Some(verb.verbIndex)) {
                  Callback.empty
                } else highlightedVerbIndex.setState(Some(verb.verbIndex))
              ),
              ^.onMouseOut --> highlightedVerbIndex.setState(None)
            )
          },
          <.div(S.subVerbContainer)(
            <.span("No questions remaining: "),
            sortedVerbsWithNoQuestions.map(verb =>
              Vector(
                <.span(S.subVerbText)(
                  ^.color := verbColorMap(verb.verbIndex).copy(a = 1.0).toColorStyleString,
                  <.a(
                    ^.name := s"verb-${verb.verbIndex}",
                    ^.visibility := "hidden"
                  ),
                  sentence.sentenceTokens(verb.verbIndex)
                )
              )
            ).intercalate(Vector(<.span(", "))).toVdomArray,
          ).when(sortedVerbsWithNoQuestions.nonEmpty),
          <.div(S.subVerbContainer)(
            <.span("Non-predicates: "),
            sentence.nonPredicates.toVector.map { case (index, _) =>
              Vector(
                <.span(S.subVerbText)(
                  ^.color := Rgba.black.toColorStyleString, // maybe grey
                  sentence.sentenceTokens(index)
                )
              )
            }.intercalate(Vector(<.span(", "))).toVdomArray,
          ).when(sentence.nonPredicates.nonEmpty && slices.qaNom)
        )
      )
    }
  }

  val S = BrowserStyles

  // class Backend(scope: BackendScope[Props, State]) {

  def renderBrowser(props: Props) = {
    StateLocal.make(State.initial(props)) { stateSnapshot =>
      val state = stateSnapshot.value
      <.div(S.mainContainer)(
        helpModal,
        headerPane(stateSnapshot),
        IndexFetch.make(request = (), sendRequest = _ => props.qasrl.getDataIndex) {
          case IndexFetch.Loading =>
            <.div(S.dataContainer)(
              <.span(S.loadingNotice)("Loading metadata...")
            )
          case IndexFetch.Loaded(index) =>
            SearchFetch.make(request = state.search.query, sendRequest = kws => props.qasrl.searchDocuments(qasrl.bank.service.Search.Query(None, kws))) {
              case SearchFetch.Loading =>
                <.span(S.loadingNotice)("Waiting for search results...")
              case SearchFetch.Loaded(searchResultIds) =>
                val denseDocIds = index.denseIds.map(_.documentId)
                val qaNomDocIds = index.qaNomIds.map(_.documentId)
                val curDocMetas = getCurDocuments(index, searchResultIds, state.filter, denseDocIds, qaNomDocIds)
                if(curDocMetas.isEmpty) {
                  <.span(S.loadingNotice)("All documents have been filtered out.")
                } else DocMetaLocal.make(
                  initialValue = curDocMetas.head,
                  shouldRefresh = _ => false
                ) { curDocMeta =>
                  <.div(S.dataContainer)(
                    docSelectionPane(
                      index.numDocuments,
                      curDocMetas,
                      curDocMeta
                    ),
                    DocFetch.make(request = curDocMeta.value.id, sendRequest = id => props.qasrl.getDocument(id)) {
                      case DocFetch.Loading =>
                        <.div(S.documentContainer)(
                          <.span(S.loadingNotice)("Loading document...")
                        )
                      case DocFetch.Loaded(doc) =>
                        val curSentences = getCurSentences(doc.sentences, state.search.query, index.denseIds, index.qaNomIds, state.filter.slices)
                        if(curSentences.isEmpty) {
                          <.div(
                            <.div(<.span(S.loadingNotice)("Current document: " + doc.metadata.title)),
                            <.div(<.span(S.loadingNotice)("All sentences have been filtered out."))
                          )
                        } else SentLocal.make(initialValue = curSentences.head) { curSentence =>
                          <.div(S.documentContainer)(
                            sentenceSelectionPane(
                              doc.sentences.size,
                              curSentences,
                              state.search.query,
                              curSentence,
                            ),
                            sentenceDisplayPane(
                              index.getPart(curDocMeta.value.id),
                              curDocMeta.value,
                              curSentence.value,
                              state.filter.slices,
                              state.filter.validOnly
                            )
                          )
                        }
                    }
                  )
                }
            }
        }
      )
    }
  }
  // }

  val Component = ScalaComponent.builder[Props]("Browser")
    .render_P(renderBrowser).build
    // .initialStateFromProps((props: Props) => State.initial(props))
    // .renderBackend[Backend]

}
