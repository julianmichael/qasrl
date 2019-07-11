package qasrl.crowd.util

import jjm.ling.Text
import jjm.ling.ISpan

import org.scalajs.dom.html

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import cats.implicits._

import monocle._
import monocle.macros._

// only higher-order-ish. More for just reducing redundancy.
// feel free to abstract more stuff out of this if you need to.
object MultiContigSpanHighlightableSentenceComponent {

  case class MultiContigSpanHighlightableSentenceProps(
    sentence: Vector[String], // PTB tokens
    styleForIndex: Int => TagMod,
    highlightedSpans: List[(ISpan, TagMod)], // in order of priority, each with own style. first determines click events
    hover: Int => Callback,
    touch: Int => Callback,
    render: List[VdomTagOf[html.Span]] => VdomElement
  ) // word/span elements to whole thing

  val MultiContigSpanHighlightableSentence = ScalaComponent
    .builder[MultiContigSpanHighlightableSentenceProps]("Multi-Contig-Span Highlightable Sentence")
    .render_P {
      case MultiContigSpanHighlightableSentenceProps(
          sentence,
          styleForIndex,
          highlightedSpans,
          hover,
          touch,
          render
          ) =>
        render(
          Text.renderTokens(
            sentence.indices.toList,
            (index: Int) => sentence(index),
            (nextIndex: Int) =>
              List(
                <.span(
                  ^.key := s"space-$nextIndex",
                  highlightedSpans
                    .find(span => span._1.contains(nextIndex) && span._1.contains(nextIndex - 1))
                    .whenDefined(_._2),
                  " "
                )
            ),
            (index: Int) =>
              List(
                <.span(
                  ^.key := s"word-$index",
                  highlightedSpans.find(_._1.contains(index)).whenDefined(_._2),
                  styleForIndex(index),
                  ^.onMouseMove ==> ((e: ReactMouseEvent) => { e.stopPropagation; hover(index) }),
                  ^.onClick ==> ((e: ReactMouseEvent) => { e.stopPropagation; touch(index) }),
                  Text.normalizeToken(sentence(index))
                )
            )
          )
        )
    }
    .build
}
