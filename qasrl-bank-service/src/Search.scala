package qasrl.bank.service

import jjm.LowerCaseString
import jjm.ling.en.InflectedForms
import jjm.ling.Text
import jjm.implicits._

import cats.Foldable
import cats.implicits._

import qasrl.bank.ConsolidatedSentence
import qasrl.bank.Document
import qasrl.bank.DocumentId

import qasrl.data.Sentence

import io.circe.generic.JsonCodec

object Search {

  @JsonCodec case class Query(
    predicateOpt: Option[InflectedForms],
    keywords: Set[LowerCaseString]) {
    def isEmpty = predicateOpt.isEmpty && keywords.isEmpty
  }

  case class Index(
    predicate: Map[InflectedForms, Set[DocumentId]],
    keyword: Map[LowerCaseString, Set[DocumentId]])

  def execute(query: Query, index: Index, documents: Map[DocumentId, Document]) = {
    if (query.isEmpty) {
      documents.keySet
    } else {
      val keywordResults = if(query.keywords.isEmpty) {
        documents.keySet
      } else query.keywords
        .map(w => index.keyword.get(w).getOrElse(Set.empty[DocumentId]))
        .reduce(_ intersect _)
      val predicateResults = query.predicateOpt.fold(documents.keySet) { pred =>
        index.predicate.get(pred).getOrElse(Set.empty[DocumentId])
      }
      keywordResults.intersect(predicateResults)
    }
  }

  def getQueryMatchesInSentence(
    sentence: ConsolidatedSentence,
    query: Search.Query
  ): Set[Int] = {
    val predicates = sentence.verbEntries.values.toList
      .filter(v => query.predicateOpt.exists(_ == v.verbInflectedForms))
      .map(_.verbIndex).toSet
    val keywords = sentence.sentenceTokens.indices.filter { i =>
      val token = sentence.sentenceTokens(i)
      query.keywords.contains(token.lowerCase) || query.keywords.contains(Text.normalizeToken(token).lowerCase) ||
      sentence.verbEntries.get(i).fold(false) { verb =>
        verb.verbInflectedForms.allForms.toSet.intersect(query.keywords).nonEmpty
      }
    }.toSet
    predicates ++ keywords
  }

  def createSearchIndex[F[_]: Foldable](documents: F[Document]) = {
    val keywordIndex = documents.foldMap { doc =>
      doc.sentences.toList.foldMap { sent =>
        sent.sentenceTokens.indices.toList.foldMap { tokIndex =>
          val tokens = List(sent.sentenceTokens(tokIndex), Text.normalizeToken(sent.sentenceTokens(tokIndex))) ++
            sent.verbEntries
            .get(tokIndex)
            .fold(List.empty[String])(verb => verb.verbInflectedForms.allForms.map(_.toString))

          tokens
            .map(_.lowerCase -> Set(doc.metadata.id))
            .toMap
        }
      }
    }

    val predicateIndex = documents.foldMap { doc =>
      doc.sentences.toList.foldMap { sent =>
        sent.verbEntries.values.toList.foldMap { verb =>
          Map(verb.verbInflectedForms -> Set(doc.metadata.id))
        }
      }
    }

    Index(predicateIndex, keywordIndex)
  }
}
