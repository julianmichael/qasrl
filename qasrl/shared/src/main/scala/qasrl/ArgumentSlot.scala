package qasrl

import nlpdata.util.LowerCaseStrings._

sealed trait ArgumentSlot { type Arg }
case object Subj extends ArgumentSlot { type Arg = Noun }
case object Obj extends ArgumentSlot { type Arg = Noun }
case object Obj2 extends ArgumentSlot { type Arg = Argument }
case class Adv(wh: LowerCaseString) extends ArgumentSlot { type Arg = Unit }

object ArgumentSlot {
  type Aux[A] = ArgumentSlot { type Arg = A }
  def allAdvSlots = List("when", "where", "why", "how", "how long", "how much").map(s => Adv(s.lowerCase))
}

