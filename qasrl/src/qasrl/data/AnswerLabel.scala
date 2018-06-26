package qasrl.data

import monocle.macros._

@Lenses case class AnswerLabel(sourceId: String, judgment: AnswerJudgment)
