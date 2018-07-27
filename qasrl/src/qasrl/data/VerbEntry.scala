package qasrl.data

import scala.collection.immutable.SortedMap

import cats.Monad
import cats.implicits._

import nlpdata.datasets.wiktionary.InflectedForms

import qasrl.util.implicits._
import qasrl.util.mergeMaps

import monocle.macros.Lenses

@Lenses case class VerbEntry(
  verbIndex: Int,
  verbInflectedForms: InflectedForms,
  questionLabels: SortedMap[String, QuestionLabel]
) {

  def combineWithLike(other: VerbEntry): Either[String, VerbEntry] = {
    if (verbIndex != other.verbIndex || verbInflectedForms != other.verbInflectedForms) {
      val thisVerbString = verbIndex + " (" + verbInflectedForms.allForms.mkString(",") + ")"
      val otherVerbString = other.verbIndex + " (" + other.verbInflectedForms.allForms
        .mkString(",") + ")"
      Left(
        s"Can only combine same verb; attempted to combine indices $thisVerbString and $otherVerbString "
      )
    } else {
      mergeMaps(questionLabels, other.questionLabels).toList
        .traverse[Either[String, ?], (String, QuestionLabel)] {
          case (qStr, qLabelIor) =>
            qLabelIor.mergeM[Either[String, ?]](_ combineWithLike _).map(qStr -> _)
        }
        .map(
          newQuestionLabels =>
            VerbEntry(
              verbIndex,
              verbInflectedForms,
              SortedMap(newQuestionLabels: _*)
          )
        )
    }
  }
}
