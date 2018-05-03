package qasrl.data

import monocle.macros.Lenses

@Lenses case class AnswerSpan(
  begin: Int, // inclusive
  end: Int // exclusive
)
