package qasrl.crowd

import spacro.util.Span

case class VerbQA(
  verbIndex: Int,
  question: String, // should be guaranteed to adhere to QA-SRL format
  answers: List[Span] // should not overlap
)
object VerbQA {
  import upickle.default._
  implicit val reader = macroR[VerbQA]
  implicit val writer = macroW[VerbQA]
}
