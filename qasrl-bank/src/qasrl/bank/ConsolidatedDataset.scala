package qasrl.bank

import jjm.LowerCaseString
import jjm.implicits._

import io.circe.generic.JsonCodec

import cats.Monoid
import cats.implicits._

import qasrl.data.Dataset
import scala.collection.immutable.SortedMap

@JsonCodec case class ConsolidatedDataset(
  sentences: SortedMap[String, ConsolidatedSentence]
) {
  def merge(that: ConsolidatedDataset) = {
    val nonPredicates = {
      this.sentences.mapValues(_.nonPredicates).toMap |+|
        that.sentences.mapValues(_.nonPredicates).toMap
    }
    this.toDataset.merge(that.toDataset).map(data =>
      ConsolidatedDataset(
        data.sentences.map { case (k, v) =>
          val nonPreds = nonPredicates.get(k).combineAll
          k -> ConsolidatedSentence.fromSentence(v).copy(nonPredicates = nonPreds)
        }
      )
    )
  }
  def toDataset: Dataset = Dataset(sentences.map { case (k , v) => k -> v.toSentence })
}
object ConsolidatedDataset {
  def fromDataset(dataset: Dataset) = {
    ConsolidatedDataset(
      dataset.sentences.map { case (sid, sent) =>
        sid -> ConsolidatedSentence.fromSentence(sent)
      }
    )
  }

  def consolidatedDatasetMonoid(processMergeFailures: List[Dataset.DataMergeFailure] => Unit): Monoid[ConsolidatedDataset] =
    new Monoid[ConsolidatedDataset] {
      override def empty: ConsolidatedDataset = ConsolidatedDataset(SortedMap.empty[String, ConsolidatedSentence])
      override def combine(x: ConsolidatedDataset, y: ConsolidatedDataset) = {
        val (fails, result) = x.merge(y).run
        processMergeFailures(fails)
        result
      }
    }
}
