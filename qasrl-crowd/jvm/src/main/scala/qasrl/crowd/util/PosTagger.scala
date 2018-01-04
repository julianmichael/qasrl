package qasrl.crowd.util

import nlpdata.structure.Word

object PosTagger {

  import edu.stanford.nlp.tagger.maxent.MaxentTagger
  lazy val tagger: MaxentTagger  = new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger");

  val posTagCache = collection.mutable.Map.empty[Vector[String], Vector[Word]]

  import cats.Foldable
  import cats.implicits._
  /** POS-tags a sequence of tokens. */
  def posTag[F[_]: Foldable](s: F[String]): Vector[Word] = {
    val origTokens = s.toList.toVector // to prevent americanization.
    // probably we can do that with a tagger parameter...but...how about later..
    posTagCache.get(origTokens) match {
      case None =>
        val result = tagger.tagTokenizedString(s.toList.mkString(" ")).split(" ").toVector
          .map(_.split("_"))
          .map(s => (s(0), s(1)))
          .zipWithIndex
          .map { case ((token, pos), index) => Word(
                  token = token,
                  pos = pos,
                  index = index)
        }
        posTagCache.put(origTokens, result)
        result
      case Some(result) => result
    }
  }
}
