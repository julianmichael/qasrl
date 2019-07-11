package example

import io.circe.generic.JsonCodec

@JsonCodec case class SentenceId(index: Int)

object SentenceId {

  // not necessarily used for serialization over the wire, but
  // used for storing to / reading from  the dataset file.
  def toString(sid: SentenceId) = sid.index.toString
  def fromString(s: String): SentenceId = SentenceId(s.toInt)
}
