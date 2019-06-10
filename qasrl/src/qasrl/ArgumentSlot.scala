package qasrl

import jjm.DependentEncoder
import jjm.DependentDecoder
import jjm.DotEncoder
import jjm.DotDecoder
import jjm.DotKleisli
import jjm.LowerCaseString
import jjm.implicits._

import cats.Id

sealed trait ArgumentSlot { type Out }
case object Subj extends ArgumentSlot { type Out = Noun }
case object Obj extends ArgumentSlot { type Out = Noun }
case object Obj2 extends ArgumentSlot { type Out = Argument }
case class Adv(wh: LowerCaseString) extends ArgumentSlot { type Out = Unit }

object ArgumentSlot {
  type Aux[A] = ArgumentSlot { type Out = A }

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

  implicit val argumentDependentEncoder = new DependentEncoder[ArgumentSlot.Aux, Id] {
    final def apply[A](slot: ArgumentSlot.Aux[A]) = slot match {
      case Subj   => implicitly[Encoder[Noun]]
      case Obj    => implicitly[Encoder[Noun]]
      case Obj2   => implicitly[Encoder[Argument]]
      case Adv(_) => implicitly[Encoder[Unit]].asInstanceOf[Encoder[A]] // TODO this should be acceptable w/o cast?
    }
  }

  implicit val argumentDependentDecoder = new DependentDecoder[ArgumentSlot.Aux, Id] {
    final def apply[A](slot: ArgumentSlot.Aux[A]) = slot match {
      case Subj   => implicitly[Decoder[Noun]]
      case Obj    => implicitly[Decoder[Noun]]
      case Obj2   => implicitly[Decoder[Argument]]
      case Adv(_) => implicitly[Decoder[Unit]].asInstanceOf[Decoder[A]] // TODO this should be acceptable w/o cast?
    }
  }

  // implicit val argumentDotEncoder = new DotKleisli[Encoder, ArgumentSlot] {
  //   final def apply(slot: ArgumentSlot) = slot match {
  //     case Subj   => implicitly[Encoder[Noun]]
  //     case Obj    => implicitly[Encoder[Noun]]
  //     case Obj2   => implicitly[Encoder[Argument]]
  //     case Adv(_) => implicitly[Encoder[Unit]] // TODO this should be acceptable w/o cast?
  //   }
  // }

  // implicit val argumentDotDecoder = new DotKleisli[Decoder, ArgumentSlot] {
  //   final def apply(slot: ArgumentSlot) = slot match {
  //     case Subj   => implicitly[Decoder[Noun]]
  //     case Obj    => implicitly[Decoder[Noun]]
  //     case Obj2   => implicitly[Decoder[Argument]]
  //     case Adv(_) => implicitly[Decoder[Unit]] // TODO this should be acceptable w/o cast?
  //   }
  // }
}
