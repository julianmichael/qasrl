package qasrl

import cats.Order
import qasrl.data.Sentence
import qasrl.data.QuestionLabel

package object bank {
  implicit val qasrlDataSentenceOrder =
    Order.by[Sentence, SentenceId](s => SentenceId.fromString(s.sentenceId))
  implicit val qasrlDataConsolidatedSentenceOrder =
    Order.by[ConsolidatedSentence, SentenceId](s => SentenceId.fromString(s.sentenceId))
}
