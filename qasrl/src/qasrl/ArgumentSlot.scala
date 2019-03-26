package qasrl

import nlpdata.util.LowerCaseStrings._

sealed trait ArgumentSlot { type Arg }
case object Subj extends ArgumentSlot { type Arg = Noun }
case object Obj extends ArgumentSlot { type Arg = Noun }
case object Obj2 extends ArgumentSlot { type Arg = Argument }
case class Adv(wh: LowerCaseString) extends ArgumentSlot { type Arg = Unit }

object ArgumentSlot {
  type Aux[A] = ArgumentSlot { type Arg = A }

  def allAdvSlots =
    List("when", "where", "why", "how", "how long", "how much").map(s => Adv(s.lowerCase))

  def toString(slot: ArgumentSlot): String = slot match {
    case Subj    => "subj"
    case Obj     => "obj"
    case Obj2    => "obj2"
    case Adv(wh) => wh.toString
  }

  def fromString(str: String): Option[ArgumentSlot] = str match {
    case "subj"  => Some(Subj)
    case "obj"   => Some(Obj)
    case "obj2" => Some(Obj2)
    case wh if allAdvSlots.contains(Adv(wh.lowerCase)) => Some(Adv(wh.lowerCase))
    case _ => None
  }


  import io.circe.{KeyEncoder, KeyDecoder}
  import io.circe.{Encoder, Decoder}
  import io.circe.Json

  implicit val argumentSlotEncoder: Encoder[ArgumentSlot] = Encoder[String].contramap(ArgumentSlot.toString(_))
  implicit val argumentSlotDecoder: Decoder[ArgumentSlot] = Decoder[String].emapTry(s => scala.util.Try(ArgumentSlot.fromString(s).get))

  implicit val argumentSlotKeyEncoder = KeyEncoder.instance(ArgumentSlot.toString)
  implicit val argumentSlotKeyDecoder = KeyDecoder.instance(ArgumentSlot.fromString)

  import qasrl.util.DependentMap
  import qasrl.util.implicits.{DependentEncoder, DependentDecoder}
  import cats.Id

  implicit val dependentArgumentEncoder = new DependentEncoder[ArgumentSlot.Aux, Id] {
    final def getEncoder[A](slot: ArgumentSlot.Aux[A]) = slot match {
      case Subj   => implicitly[Encoder[Noun]]
      case Obj    => implicitly[Encoder[Noun]]
      case Obj2   => implicitly[Encoder[Argument]]
      case Adv(_) => implicitly[Encoder[Unit]].asInstanceOf[Encoder[A]] // TODO this should be acceptable w/o cast?
    }
  }

  implicit val dependentArgumentDecoder = new DependentDecoder[ArgumentSlot.Aux, Id] {
    final def getDecoder[A](slot: ArgumentSlot.Aux[A]) = slot match {
      case Subj   => implicitly[Decoder[Noun]]
      case Obj    => implicitly[Decoder[Noun]]
      case Obj2   => implicitly[Decoder[Argument]]
      case Adv(_) => implicitly[Decoder[Unit]].asInstanceOf[Decoder[A]] // TODO this should be acceptable w/o cast?
    }
  }
}
