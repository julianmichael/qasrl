package qasrl.data

import monocle.macros.Lenses

// TODO replace with ESpan
@Lenses case class AnswerSpan(
  begin: Int, // inclusive
  end: Int // exclusive
)
