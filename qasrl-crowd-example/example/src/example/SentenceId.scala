package example

case class SentenceId(index: Int)

object SentenceId {

  // not necessarily used for serialization over the wire, but
  // used for storing to / reading from  the dataset file.
  def toString(sid: SentenceId) = sid.index.toString
  def fromString(s: String): SentenceId = SentenceId(s.toInt)

  import upickle.default._
  implicit val reader = macroR[SentenceId]
  implicit val writer = macroW[SentenceId]
}
