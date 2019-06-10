package qasrl.crowd

import io.circe.generic.JsonCodec

@JsonCodec case class SourcedQuestion(verbIndex: Int, question: String, sources: Set[String])

@JsonCodec case class QASRLEvaluationPrompt[SID](id: SID, sourcedQuestions: List[SourcedQuestion])

