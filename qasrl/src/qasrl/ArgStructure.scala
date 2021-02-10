package qasrl

import cats.Id

import io.circe.generic.JsonCodec

import jjm.DependentMap
import jjm.ling.en.InflectedForms

import monocle.macros._

@JsonCodec @Lenses case class ArgStructure(
  args: DependentMap[ArgumentSlot.Aux, Id],
  isPassive: Boolean
) {
  def forgetAnimacy = {
    import scala.annotation.unchecked
    val newArgs = args.keys.foldLeft(DependentMap.empty[ArgumentSlot.Aux, Id]) {
      (m, k) => (k: @unchecked) match {
        case Adv(wh) => m.put(Adv(wh), args.get(Adv(wh)).get)
        case Subj   => m.put(Subj, Noun(false))
        case Obj    => m.put(Obj, Noun(false))
        case Obj2  => m.put(
          Obj2, args.get(Obj2).get match {
            case Noun(_) => Noun(false)
            case Prep(p, Some(Noun(_))) => Prep(p, Some(Noun(false)))
            case x => x
          }
        )
      }
    }
    this.copy(args = newArgs)
  }

  def getValidArgumentSlots: Set[ArgumentSlot] = {
    (this.args.keys.toList: List[ArgumentSlot]).filter {
      case Obj2 => this.args.get(Obj2) match {
        case Some(Prep(_, None)) => false
        case _ => true
      }
      case _ => true
    }.toSet
  }


  override def toString = Frame(
    InflectedForms.generic, structure = this, tense = qasrl.Tense.Finite.Present, isPerfect = false, isProgressive = false, isNegated = false
  ).clauses.head
}
object ArgStructure
