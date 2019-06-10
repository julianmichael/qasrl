package qasrl.crowd

import jjm.ISpan
import io.circe.generic.JsonCodec

@JsonCodec case class VerbQA(
  verbIndex: Int,
  question: String, // should be guaranteed to adhere to QA-SRL format
  answers: List[ISpan] // should not overlap
)
